_G.VERSION = "1.0.0"
_G.NAME	   = 'gitstatus'

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
rgx = go.import"regexp"
iou = go.import"ioutil"
path = go.import"path"

ACTIVE_COMMITS = {}
errors =
  is_a_repo: "the current directory is already a repository"
  not_a_repo: "the current directory is not a repository"
  invalid_arg: "invalid_argument provided"
  bad_label_arg: "invalid argument, please provide a valid rev label"
  not_enough_args: "not enough arguments"
  unknown_label: "given label is not known to this repo"

-- package.path = "#{cfg.ConfigDir}/plug/#{NAME}/lib/?.lua;#{package.path}"
-- package.path = "#{cfg.ConfigDir}/plug/#{NAME}/lib/?/init.lua;#{package.path}"

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
each_line = (->
  str = go.import "strings"
  return (input, fn) ->
    input = str.Replace chomp(input), "\r\n", "\n", -1
    input = str.Replace input, "\n\r", "\n", -1
    lines = str.Split(input, "\n")
    l_count = #lines

    stop = false
    finish = -> stop = true

    for i = 1, l_count
      return if stop

      fn lines[i], finish
)!


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
  re_special_chars = rgx.MustCompile"\\x1B\\[([0-9]{1,3}(;[0-9]{1,3})*)?[mGK]"

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

make_commit_pane = (output, header, fn) ->
  old_view = (app.CurPane!)\GetView!
  h = old_view.Height

  filepath = make_temp!

  -- TODO: This is giving permission errors, find out why
  debug "Populating temporary commit file #{filepath} ..."
  iou.WriteFile filepath, output, 0x1B0 -- 0660, to account for octal

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
  commit_pane.Buf\SetOptionNative "", false
  --commit_pane.Buf.EventHandler\Insert buf.Loc(0, 0), output
  commit_pane.Cursor.Loc.Y = 000
  commit_pane.Cursor.Loc.X = 0
  commit_pane.Cursor\Relocate!

  table.insert ACTIVE_COMMITS, {
    buffer:commit_pane,
    callback:fn,
    file: filepath
    done: ready
  }

local git
local set_callbacks

git = (->
  w_commit  = wordify 'commit', '', 's'
  w_line    = wordify 'line', '', 's'
  re_commit = rgx.MustCompile"^commit[\\s]+([^\\s]+).*$"

  --- Execute a git command with the provided args
  exec = (...) ->
    -- debug "Determining git command ..."
    cmd = cfg.GetGlobalOption "git.path"
    if cmd == ""
      -- debug "Getting command from shell ..."
      cmd, _ = shl.ExecCommand "command", "-v", "git"
      cmd = chomp cmd
      if cmd == '' or not cmd
        return "", "no git configured"

    -- debug "Stat'ing #{cmd}"
    finfo, err = os.Stat cmd
    unless finfo
      return "", err.Error!

    -- debug "Attempting to run #{cmd} ..."
    out, err = shl.ExecCommand cmd, ...
    return out, err

  --- Check if the current working directory is a git repo
  in_repo = ->
    out, _ = exec "rev-parse", "--is-inside-work-tree"
    return chomp(out) == 'true'

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

  --- Get all of the branches in the current repository
  get_branches = () ->
    out, _ = exec "branch", "-al"
    branches = {}

    each_line out, ->
      debug "Attempting to match: #{line}"
      name = line\match "^%s*%*?%s*([^%s]+)"
      return unless name

      debug "Found branch: #{name}"
      commit, err = exec "rev-parse", name
      if err and err != ""
        debug "Failed to rev-parse #{name}: #{err}"
        return

      table.insert branches,
        commit: chomp commit
        :name

    return branches

  --- Get the revision for a label, or return false
  known_label = (label) ->
    out, err = exec "rev-parse", label
    return err != "" and false or chomp(out)

  get_args = (goarray) ->
    return unpack [a for a in *goarray]

  export onSave = =>
    debug "Caught onSave, buf:#{@}"
    return unless in_repo!
    return unless #ACTIVE_COMMITS > 0

    for i, commit in ipairs ACTIVE_COMMITS
      if commit.buffer == @
        debug "Marking commit #{i} as ready ..."
        commit.ready = true
        break

  return {  
    init: =>
      unless not in_repo!
        return send.init errors.is_a_repo
      out, err = exec "init"
      send.init out

    fetch: =>
      unless in_repo!
        return send.fetch errors.not_a_repo

      out, err = exec "fetch"
      send.fetch out

    checkout: (->
      re_valid_label = rgx.MustCompile"^[a-zA-Z-_/.]+$"

      return (argv) =>
        label = get_args argv
      
        unless in_repo!
          return send.checkout errors.not_a_repo

        unless label != nil
          return send.checkout errors.not_enough_args .. "(supply a branch/tag/commit)"

        unless re_valid_label\Match label
          return send.checkout errors.bad_label_arg

        unless known_label label
          return send.checkout errors.unknown_label

        out, err = exec "checkout", label        
        send.checkout out
    )!

    --- List all of the branches in the current repository
    listbranches: =>
      unless in_repo!
        return send.checkout errors.not_a_repo

      branches = get_branches!
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
      unless in_repo!
        return send.status errors.not_a_repo

      status_out = exec "status"
      return send.status status_out
    
    --- Create a new git-branch and switch to it
    branch: (->
      re_valid_label = rgx.MustCompile"^[a-zA-Z-_/.]+$"

      return (argv) =>
        label = get_args argv
      
        unless in_repo!
          return send.branch errors.not_a_repo
          
        unless re_valid_label\Match label
          return send.branch errors.invalid_lbl

        -- Intentionally ignoring any errors here, in case this is a local
        -- only repository
        out = ''
        fetch_out, _ = exec "fetch"
        out ..= "> git fetch"
        out ..= fetch_out

        if rev = known_label label
          return send.branch errors.invalid_arg ..
            ", please supply an unused label (#{label} is rev:#{rev})"

        branch_out, err = exec "branch", label
        out ..= "> git branch #{label}"
        out ..= branch_out

        unless err
          chkout_out, _ = exec "checkout", label
          out ..= "> git checkout #{label}"
          out ..= chkout_out

        send.branch out
    )!

    --- Commit changes to the current branch
    commit: (->    
      base_msg = "\n\n"
      base_msg ..= "# Please enter the commit message for your changes. Lines starting\n"
      base_msg ..= "# with '#' will be ignored, and an empty message aborts the commit.\n#\n"

      msg_line = rgx.MustCompile"^\\s*([^#])"

      return (msg) =>
        unless in_repo!
          return send.commit errors.not_a_repo

        if msg
          commit_out = exec "commit", "-m", msg
          return send.commit commit_out

        debug "Processing git-status ..."
        commit_msg_start = base_msg
        status_out, _ = exec "status"
        each_line status_out, (line) ->
          commit_msg_start ..= "# #{line}\n"

        debug "Populating commit pane ..."
        header = "[new commit: save and quit to finalize]"
        
        make_commit_pane commit_msg_start, header, (file, _) ->
          debug "Beginning commit callback ..."
          commit_msg = iou.ReadFile file
          commit_msg = str.TrimSuffix commit_msg, commit_msg_start
          
          if commit_msg == ""
            return send.commit "Aborting, empty commit"

          final_commit = ''
          each_line commit_msg, (line, final) ->
            return if line == nil
            
            line = chomp line
            if msg_line\Match line
              final_commit ..= "#{line}\n"

          debug "Committing: #{final_commit}"
          iou.WriteFile file, final_commit, 0x1B0 -- 0660 octal

          if "" == chomp final_commit
            return send.commit "Aborting, empty commit"
            
          commit_out = exec "commit", "-F", file
          send.commit commit_out

        debug "Awaiting commit completion within onQuit"
        return
    )!

    --- Push local changes for branch (or all, if none specified) to remotes
    push: (->
      re_valid_label = rgx.MustCompile"^[a-zA-Z-_/.]+$"
      
      return (branch) =>
        unless in_repo!
          return send.push errors.not_a_repo

        if branch
          unless re_valid_label\Match branch
            return send.push errors.bad_label_arg
        else
          branch == "--all"

        push_out, _ = exec "push", branch
        send.push push_out
    )!

    --- Pull changes from remotes into local
    pull: =>
      pull_out, _ = exec "pull"
      send.pull pull_out

    --- Show git commit log
    log: =>
      unless in_repo!
        return send.log "the current directory is not a repository"

      count = 0
      out, err = exec "log"
      each_line out, (line) ->
        count += 1 if re_commit\MatchString line

      send.log out, {
        header: "#{count} #{w_commit count}"
      }

    add: (...) =>
      files = {}
      for file in *{...}
        if file == "."
          files = { "." }
          break

        finfo, _ = os.Stat file
        unless finfo
          return send.add errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless #files > 0
        return send.add errors.not_enough_args .. ", please supply a file"

      exec "add", unpack files

      
    rm: (...) =>
      files = {}
      for file in *{...}
        if file == "."
          files = { "." }
          break

        finfo, _ = os.Stat file
        unless finfo
          return send.rm errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless #files > 0
        return send.rm errors.not_enough_args .. ", please supply a file"

      exec "rm", unpack files
  }
)!

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

  registerCommand "git.raw", git.raw, cfg.NoComplete
  registerCommand "git.init", git.init, cfg.NoComplete
  registerCommand "git.pull", git.pull, cfg.NoComplete
  registerCommand "git.push", git.push, cfg.NoComplete
  registerCommand "git.list", git.listbranches, cfg.NoComplete
  registerCommand "git.log", git.log, cfg.NoComplete
  registerCommand "git.commit", git.commit, cfg.NoComplete
  registerCommand "git.status", git.status, cfg.NoComplete
  registerCommand "git.add", git.add, cfg.NoComplete
  registerCommand "git.rm", git.rm, cfg.NoComplete

-- TODO: Clear out stale commit files from previous commit attempts
export preinit = ->
  debug "Clearing stale commit files ..."
  pfx = "#{NAME}.commit."
  dir = path.Join "#{cfg.ConfigDir}", "tmp"

  files, err = iou.ReadDir dir
  unless err
    for f in *files
      debug "Does #{f\Name!} have the prefix #{pfx}?"
      if str.HasPrefix f\Name!, pfx
        filepath = path.Join dir, f\Name!
        debug "Clearing #{filepath}"
        os.Remove filepath

export onQuit = =>
  info = app.InfoBar!

  debug "Caught onQuit, buf:#{@}"
  return unless #ACTIVE_COMMITS > 0

  debug "Populating temporary table for active commits ..."
  active = [commit for commit in *ACTIVE_COMMITS]

  debug "Iterating through known commits ..."
  for i, commit in ipairs ACTIVE_COMMITS
    if commit.buffer == @
      if commit.ready
        debug "Commit #{i} is ready, fulfilling active commit ..."
        commit.callback commit.file
      else
        if @Buf.modified
          -- We need to override the current YNPrompt if it exists,
          -- and then close it. This way, we can hijack the save/quit
          -- prompt and inject our own
          --
          -- TODO: Get rid of the hijack, or find some way to make it
          --       less terrible. Ideally, we actually want to tell it
          --       to cancel properly, but AbortCommand() does not call
          --       the InfoBar.YNPrompt() callback with true as the 2nd
          --       boolean (which it probably should)
          if info.HasYN and info.HasPrompt
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
