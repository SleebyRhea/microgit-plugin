export VERSION = "1.0.0"
export NAME    = 'gitstatus'

-- Workaround for import being a keyword in Moonscript
go = assert loadstring([[
  -- script: lua
  return ({
    import = function (pkg)
      return import(pkg)
    end
  })
  -- script: end
]])!

os  = go.import"os"
app = go.import"micro"
buf = go.import"micro/buffer"
cfg = go.import"micro/config"
shl = go.import"micro/shell"
str = go.import"strings"
path = go.import"path"
fpath = go.import"path/file"
ioutil = go.import"ioutil"
regexp = go.import"regexp"
runtime = go.import"runtime"

ACTIVE_COMMITS = {}
errors =
  is_a_repo: "the current directory is already a repository"
  not_a_repo: "the current directory is not a repository"
  invalid_arg: "invalid_argument provided"
  bad_label_arg: "invalid argument, please provide a valid rev label"
  not_enough_args: "not enough arguments"
  unknown_label: "given label is not known to this repo"

debug = (m) ->
  app.Log "#{NAME}: #{m}"

--- Delete leading and trailing spaces, and the final newline
chomp = (s) ->
  s = s\gsub("^%s*", "")\gsub("%s*$", "")\gsub("[\n\r]*$", "")
  return s

--- Generate a function that takes a number, and returns the correct plurality of a word
wordify = (word, singular, plural) ->
  singular = word .. singular
  plural   = word .. plural
  (number) ->
    number != 1 and plural or singular

--- Run a given function for each line in a string
each_line = (input, fn) ->
  input = str.Replace chomp(input), "\r\n", "\n", -1
  input = str.Replace input, "\n\r", "\n", -1
  lines = str.Split(input, "\n")
  l_count = #lines

  stop = false
  finish = -> stop = true

  for i = 1, l_count
    return if stop

    fn lines[i], finish

path_exists = (filepath) ->
  finfo, _ = os.Stat filepath
  return finfo != nil

make_temp = (->
  rand = go.import "math/rand"
  
  chars = 'qwertyuioasdfghjklzxcvbnm'
  return ->
    dir = path.Join "#{cfg.ConfigDir}", "tmp"
    err = os.MkdirAll(dir, 0x1F8) -- 770, to account for octal
    assert not err, err
    
    file = ("#{NAME}.commit.") .. ("XXXXXXXXXXXX")\gsub '[xX]', =>
      i = rand.Intn(25) + 1
      c = chars\sub i, i
      switch @
        when 'x'
          return c
        when 'X'
          return string.upper c

    debug "Generated new tempfile: #{dir}/#{file}"

    return tostring path.Join dir, file
)!

-- Inspired heavily by filemanager
-- https://github.com/micro-editor/updated-plugins/blob/master/filemanager-plugin/filemanager.lua
send_block = (->
  re_special_chars = regexp.MustCompile"\\x1B\\[([0-9]{1,3}(;[0-9]{1,3})*)?[mGK]"

  return (header, output) ->
    old_view = (app.CurPane!)\GetView!
    h = old_view.Height

    output = re_special_chars\ReplaceAllString output, ""

    -- Creates a new split, set's it's height to a fifth of the current pane height
    -- makes it readonly, unsaveable, and then dumps the output string into its
    -- buffer. Follow this by setting the cursor to 0,0 to move to the top
    send_pane = (app.CurPane!)\HSplitIndex(buf.NewBuffer(output, header), true)
    send_pane\ResizePane h - (h / 5)
    send_pane.Buf.Type.Scratch = true
    send_pane.Buf.Type.Readonly = true
    send_pane.Buf.Type.Syntax = false
    send_pane.Buf\SetOptionNative "softwrap", true
    send_pane.Buf\SetOptionNative "ruler", false
    send_pane.Buf\SetOptionNative "autosave", false
    send_pane.Buf\SetOptionNative "statusformatr", ""
    send_pane.Buf\SetOptionNative "statusformatl", header
    send_pane.Buf\SetOptionNative "scrollbar", false
    send_pane.Cursor.Loc.Y = 0
    send_pane.Cursor.Loc.X = 0
    send_pane.Cursor\Relocate!
)!

make_commit_pane = (root, output, header, fn) ->
  old_view = root\GetView!
  h = old_view.Height

  filepath = make_temp!

  debug "Populating temporary commit file #{filepath} ..."
  ioutil.WriteFile filepath, output, 0x1B0 -- 0660, to account for octal

  -- Creates a new split, sets it's height to a third of the current pane height
  -- makes and then dumps the output string into its buffer
  -- Follow this by setting the cursor to 0,0 to move to the top
  debug "Generating new buffer for #{filepath}"
  commit_pane = (app.CurPane!)\HSplitIndex(buf.NewBuffer(output, filepath), true)
  commit_pane\ResizePane h - (h / 3)
  commit_pane.Buf.Type.Scratch = false
  commit_pane.Buf.Type.Readonly = false
  commit_pane.Buf.Type.Syntax = false
  commit_pane.Buf\SetOptionNative "softwrap", true
  commit_pane.Buf\SetOptionNative "ruler", false
  commit_pane.Buf\SetOptionNative "autosave", false
  commit_pane.Buf\SetOptionNative "statusformatr", ""
  commit_pane.Buf\SetOptionNative "statusformatl", header
  commit_pane.Buf\SetOptionNative "scrollbar", false
  commit_pane.Cursor.Loc.Y = 0
  commit_pane.Cursor.Loc.X = 0
  commit_pane.Cursor\Relocate!

  table.insert ACTIVE_COMMITS, {
    callback: fn
    pane: commit_pane
    file: filepath
    done: ready
    root: root
  }

local git
local set_callbacks

bound = (n, min, max) ->
  debug "bound: got: #{n}"
  n > max and max or (n < min and min or n)

-- filepath.Abs and filepath.IsAbs both exist, however, their use in Lua code
-- here currently panics the application. Until then, we'll just have to rely
-- on something hacky in the meantime. This is pretty gross, but it works.
get_path_info = (->
  s = string.char os.PathSeparator

  re_abs  = regexp.MustCompile("^#{runtime.GOOS == 'windows' and '[a-zA-Z]:' or ''}#{s}#{s}?.+#{s}#{s}?.*")
  re_part = regexp.MustCompile("#{s}#{s}?")
  re_root = regexp.MustCompile("^#{runtime.GOOS == 'windows' and '[a-zA-Z]:' or ''}#{s}#{s}$")

  return (_string) ->
    if re_root\Match _string
      debug "get_path_info: #{_string} matched regexp[#{re_root\String!}]"
      return _string, _string, _string
    
    if re_abs\Match _string
      debug "get_path_info: #{_string} matched regexp[#{re_abs\String!}]"
      split_path = re_part\Split _string, -1
      l = #split_path
      return _string, str.TrimSuffix(_string, split_path[bound(l, 1, l)]), split_path[bound(l, 1, l)]

    debug "get_path_info: #{_string} is relative"
    pwd, err = os.Getwd!
    assert not err, "failed to get current working directory"
    abs = (pwd .. s .. _string)
    split_path = re_part\Split abs, -1
    l = #split_path
    
    debug "get_path_info: bound: #{bound(l-1, 1, l)}"
    debug "get_path_info: Bound: #{bound(l, 1, l)}"
    return abs, pwd, split_path[bound(l, 1, l)]
)!

git = (->
  w_commit  = wordify 'commit', '', 's'
  w_line    = wordify 'line', '', 's'
  re_commit = regexp.MustCompile"^commit[\\s]+([^\\s]+).*$"
  
  new_command = (filepath) ->
    if type(filepath) != 'string' or filepath == ''
      debug "filepath [#{filepath}] is not a valid editor path (need string): (got: #{type filepath})"
      return nil, "Please run this in a file pane"

    abs, dir, name = get_path_info filepath
   
    exec = (...) ->
      unless path_exists dir
        return nil, "directory #{dir} does not exist"

      debug "Parent directory #{dir} exists, continuing ..."
      base = cfg.GetGlobalOption "git.path"
      if base == ""
        base, _ = shl.ExecCommand "command", "-v", "git"
        base = chomp base
        if base == '' or not base
          return nil, "no git configured"

      debug "Found valid git path: #{base}"
      unless path_exists base
        return nil, err.Error!

      debug "Running ..."
      out = shl.ExecCommand base, "-C", dir, ...
      return out
      
    in_repo = ->
      out, _ = exec "rev-parse", "--is-inside-work-tree"
      return chomp(out) == 'true'
      
    get_branches = ->
      out, _ = exec "branch", "-al"
      branches = {}

      each_line out, (line) ->
        debug "Attempting to match: #{line}"
        name = line\match "^%s*%*?%s*([^%s]+)"
        return unless name

        debug "Found branch: #{name}"
        revision, err = exec "rev-parse", name
        if err and err != ""
          debug "Failed to rev-parse #{name}: #{err}"
          return

        table.insert branches,
          commit: chomp revision
          :name

      return branches

    known_label = (label) ->
      out, err = exec "rev-parse", label
      return err != "" and false or chomp(out)

    return { :new, :exec, :in_repo, :known_label, :get_branches }

  --- Issue a message to the InfoBar with a neat syntax
  send = setmetatable {}, __index: (_, cmd) ->
    cmd = cmd\gsub "_", "-"
    (msg, config) ->
      line_count = select(2, string.gsub(tostring(msg), "[\r\n]", ""))

      debug "LineCount: #{line_count}"
      if line_count > 1
        header = "git-#{cmd}"
        if type(config) == "table"
          if config.header != nil
            header = "#{header}: #{config.header}"
        send_block header, msg
        return

      (app.InfoBar!)\Message "git-#{cmd}: #{msg}"
      return

  return {  
    init: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.init err
      
      unless not cmd.in_repo!
        return send.init errors.is_a_repo
      out, err = cmd.exec "init"
      return send.init err if err
      send.init out
      
    fetch: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.fetch err
      unless cmd.in_repo!
        return send.fetch errors.not_a_repo
      out, err = cmd.exec "fetch"
      return send.fetch err if err
      send.fetch out

    checkout: (->
      re_valid_label = regexp.MustCompile"^[a-zA-Z-_/.]+$"

      return (label) =>
        cmd, err = new_command @Buf.Path
        unless cmd
          return send.checkout err
        
        unless cmd.in_repo!
          return send.checkout errors.not_a_repo

        unless label != nil
          return send.checkout errors.not_enough_args .. "(supply a branch/tag/commit)"

        unless re_valid_label\Match label
          return send.checkout errors.bad_label_arg

        unless cmd.known_label label
          return send.checkout errors.unknown_label

        out, err = cmd.exec "checkout", label
        return send.checkout err if err
        send.checkout out
    )!

    --- List all of the branches in the current repository
    listbranches: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.list err
      
      unless cmd.in_repo!
        return send.checkout errors.not_a_repo

      branches = cmd.get_branches!
      current  = ''
      output   = ''

      if current_branch != ''
        output ..= "\nCurrent: #{current}\n"

      output ..= "Branches:\n"
      for branch in *branches
        if branch.name == current
          output ..= "-> "
        else
          output ..= "   "
        output ..= "#{branch.name} - rev:#{branch.commit}\n"

      return send.list_branches output
      
    status: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.status err
    
      unless cmd.in_repo!
        return send.status errors.not_a_repo

      status_out, err = cmd.exec "status"
      return send.status err if err
      send.status status_out
    
    --- Create a new git-branch and switch to it
    branch: (->
      re_valid_label = regexp.MustCompile"^[a-zA-Z-_/.]+$"

      return (label) =>
        cmd, err = new_command @Buf.Path
        unless cmd
          return send.branch err
        
        unless cmd.in_repo!
          return send.branch errors.not_a_repo
          
        unless re_valid_label\Match label
          return send.branch errors.invalid_lbl

        -- Intentionally ignoring any errors here, in case this is a local
        -- only repository
        out = ''
        fetch_out, _ = cmd.exec "fetch"
        out ..= "> git fetch"
        out ..= fetch_out

        if rev = cmd.known_label label
          return send.branch errors.invalid_arg ..
            ", please supply an unused label (#{label} is rev:#{rev})"

        branch_out, err = cmd.exec "branch", label
        out ..= "> git branch #{label}"
        out ..= branch_out

        unless err
          chkout_out, _ = cmd.exec "checkout", label
          out ..= "> git checkout #{label}"
          out ..= chkout_out

        send.branch out
    )!

    --- Commit changes to the current branch
    commit: (->    
      msg_line = regexp.MustCompile"^\\s*([^#])"
      base_msg = "\n"
      base_msg ..= "# Please enter the commit message for your changes. Lines starting\n"
      base_msg ..= "# with '#' will be ignored, and an empty message aborts the commit.\n#\n"

      return (msg) =>
        cmd, err = new_command @Buf.Path
        unless cmd
          return send.commit err
      
        unless cmd.in_repo!
          return send.commit errors.not_a_repo

        if msg
          commit_out, err = cmd.exec "commit", "-m", msg
          return send.commit err if err
          return send.commit commit_out

        commit_msg_start = base_msg
        status_out, _ = cmd.exec "status"
        each_line status_out, (line) ->
          commit_msg_start ..= "# #{line}\n"
          
        header = "[new commit: save and quit to finalize]"
        
        make_commit_pane self, commit_msg_start, header, (file, _) ->
          commit_msg = ioutil.ReadFile file
          commit_msg = str.TrimSuffix commit_msg, commit_msg_start
          
          if commit_msg == ""
            return send.commit "Aborting, empty commit"

          final_commit = ''
          each_line commit_msg, (line, final) ->
            return if line == nil
            
            line = chomp line
            if msg_line\Match line
              final_commit ..= "#{line}\n"

          ioutil.WriteFile file, final_commit, 0x1B0 -- 0660 octal

          if "" == chomp final_commit
            return send.commit "Aborting, empty commit"
            
          commit_out, err = cmd.exec "commit", "-F", file
          return send.commit err if err
          send.commit commit_out

        debug "Awaiting commit completion within onQuit"
        return
    )!

    --- Push local changes for branch (or all, if none specified) to remotes
    push: (->    
      re_valid_label = regexp.MustCompile"^[a-zA-Z-_/.]+$"
      
      return (branch) =>
        cmd, err = new_command @Buf.Path
        unless cmd
          return send.push err
      
        unless cmd.in_repo!
          return send.push errors.not_a_repo

        if branch != nil  
          unless re_valid_label\Match branch
            return send.push errors.bad_label_arg
        else
          branch = "--all"

        push_out, err = cmd.exec "push", branch
        return send.push err if err
        send.push push_out
    )!

    --- Pull changes from remotes into local
    pull: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.pull err
      
      unless cmd.in_repo!
        return send.pull errors.not_a_repo
      
      pull_out, err = cmd.exec "pull"
      return send.pull err if err
      send.pull pull_out

    --- Show git commit log
    log: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.log err
      
      unless cmd.in_repo!
        return send.log errors.not_a_repo

      count = 0
      out, err = cmd.exec "log"
      return send.log if err
      each_line out, (line) ->
        count += 1 if re_commit\MatchString line

      send.log out, {
        header: "#{count} #{w_commit count}"
      }

    add: (...) =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.add err

      unless cmd.in_repo!
        return send.add errors.not_a_repo
      
      files = {}
      for file in *{...}
        if file == "."
          files = { "." }
          break

        unless path_exists file
          return send.add errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless #files > 0
        return send.add errors.not_enough_args .. ", please supply a file"

      cmd.exec "add", unpack files

      
    rm: (...) =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.rm err

      unless cmd.in_repo!
        return send.add errors.not_a_repo
    
      files = {}
      for file in *{...}
        if file == "."
          files = { "." }
          break

        unless path_exists file
          return send.rm errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless #files > 0
        return send.rm errors.not_enough_args .. ", please supply a file"

      cmd.exec "rm", unpack files
  }
)!

-- TODO: Implement git statusline information
cfg.RegisterCommonOption "git", "path", ""
cfg.RegisterCommonOption "git", "onsave", true
cfg.RegisterCommonOption "git", "status_line", true

--- Register a provided function+callback as a command
-- Wraps the given function to account for Micros handling of arguments placing
-- additional arguments of len size > 1 in a Go array
registerCommand = (name, fn, cb) ->
  cmd = (any, extra) ->
    debug "command[#{name}] started"
    if extra
      fn any, unpack([a for a in *extra])
    else
      fn any
    debug "command[#{name}] completed"

  cfg.MakeCommand name, cmd, cb

export init = ->
  debug "Initializing #{NAME}"

  cmd = cfg.GetGlobalOption "git.path"
  if cmd == ""
    cmd, _ = shl.ExecCommand "command", "-v", "git"
    if cmd == '' or not cmd
      app.TermMessage "#{NAME}: git not present in $PATH or set, some functionality will not work correctly"

  registerCommand "git.init", git.init, cfg.NoComplete
  registerCommand "git.pull", git.pull, cfg.NoComplete
  registerCommand "git.push", git.push, cfg.NoComplete
  registerCommand "git.list", git.listbranches, cfg.NoComplete
  registerCommand "git.log", git.log, cfg.NoComplete
  registerCommand "git.commit", git.commit, cfg.NoComplete
  registerCommand "git.status", git.status, cfg.NoComplete
  registerCommand "git.checkout", git.checkout, cfg.NoComplete
  registerCommand "git.add", git.add, cfg.NoComplete
  registerCommand "git.rm", git.rm, cfg.NoComplete

export preinit = ->
  debug "Clearing stale commit files ..."
  pfx = "#{NAME}.commit."
  dir = path.Join "#{cfg.ConfigDir}", "tmp"

  files, err = ioutil.ReadDir dir
  unless err
    for f in *files
      debug "Does #{f\Name!} have the prefix #{pfx}?"
      if str.HasPrefix f\Name!, pfx
        filepath = path.Join dir, f\Name!
        debug "Clearing #{filepath}"
        os.Remove filepath

export onSave = =>
    return unless #ACTIVE_COMMITS > 0

    for i, commit in ipairs ACTIVE_COMMITS
      if commit.pane == @
        debug "Marking commit #{i} as ready ..."
        commit.ready = true
        break

export onQuit = =>
  debug "Caught onQuit, buf:#{@}"
  return unless #ACTIVE_COMMITS > 0

  debug "Populating temporary table for active commits ..."
  active = [commit for commit in *ACTIVE_COMMITS]

  debug "Iterating through known commits ..."
  for i, commit in ipairs ACTIVE_COMMITS
    if commit.pane == self
      if commit.ready
        debug "Commit #{i} is ready, fulfilling active commit ..."
        commit.callback commit.file
      else
        if @Buf\Modified!
          -- We need to override the current YNPrompt if it exists,
          -- and then close it. This way, we can hijack the save/quit
          -- prompt and inject our own
          --
          -- TODO: Get rid of the hijack, or find some way to make it
          --       less terrible. Ideally, we actually want to tell it
          --       to cancel properly, but AbortCommand() does not call
          --       the InfoBar.YNPrompt() callback with true as the 2nd
          --       boolean (which it probably should)
          info = app.InfoBar!
          if info.HasYN and info.HasPrompt
            debug "Removing message: #{info.Message}"
            info.YNCallback = ->
            info\AbortCommand!

          cancel = false
          
          info\YNPrompt "Would you like to save and commit? (y,n,esc)",
            (yes, cancelled) ->
              if cancelled
                cancel = true
                return
              if yes
                @Buf\Save!
                @ForceQuit!
                commit.callback commit.file
                return
              else
                info\Message "Aborted commit (closed before saving)"
              @ForceQuit!
              return
              
          return if cancel

      debug "Removing #{commit.file}"
      os.Remove commit.file

      debug "Popping commit #{i} from stack"
      for t, _temp in ipairs active
        if _temp == commit
          table.remove active, t
          ACTIVE_COMMITS = active
          break
      break
