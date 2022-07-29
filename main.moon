export VERSION = "1.0.0"
export NAME    = 'microgit'
export DESC    = 'Git for Micro'

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
BRANCH_STATUS = {}
BUFFER_BRANCH = {}

LOADED_COMMANDS = { __order: {} } unless LOADED_COMMANDS
LOADED_OPTIONS = { __order: {} } unless LOADED_OPTIONS
LOADED_LINEFNS = { __order: {} } unless LOADED_LINEFNS

errors =
  is_a_repo: "the current directory is already a repository"
  not_a_repo: "the current directory is not a repository"
  invalid_arg: "invalid_argument provided"
  bad_label_arg: "invalid argument, please provide a valid rev label"
  not_enough_args: "not enough arguments"
  unknown_label: "given label is not known to this repo"
  command_not_found: "invalid command provided (not a command)"
  no_help: "FIXME: no help for command git."

local git
bound = (n, min, max) -> n > max and max or (n < min and min or n)
debug = (m) -> app.Log "#{NAME}: #{m}"

--- Delete leading and trailing spaces, and the final newline
chomp = (s) ->
  s = s\gsub("^%s*", "")\gsub("%s*$", "")\gsub("[\n\r]*$", "")
  return s


--- Run a given function for each line in a string
each_line = (input, fn) ->
  input = str.Replace input, "\r\n", "\n", -1
  input = str.Replace input, "\n\r", "\n", -1
  lines = str.Split(input, "\n")
  l_count = #lines

  stop = false
  finish = -> stop = true

  for i = 1, l_count
    return if stop

    fn lines[i], i, l_count, finish

--- Register a provided function+callback as a command
-- Wraps the given function to account for Micros handling of arguments placing
-- additional arguments of len size > 1 in a Go array
add_command = (name, fn, cb) ->
  return if LOADED_COMMANDS[name]

  external_name = "git.#{name}"
  cmd = (any, extra) ->
    debug "command[#{external_name}] started"
    if extra
      fn any, unpack([a for a in *extra])
    else
      fn any
    debug "command[#{external_name}] completed"

    if any and any.Buf then
      git.update_branch_status any.Buf
    elseif any.Path
      git.update_branch_status any
    
    return

  cfg.MakeCommand external_name, cmd, cb
  LOADED_COMMANDS[name] = { :cmd, help: git[name .. "_help"] }
  table.insert LOADED_COMMANDS.__order, name


--- Register a provided configuration option
-- Canonicalizes the option name, and places the provided information into
-- the LOADED_OPTIONS table for later usage
add_config = (name, default, description) ->
  return if LOADED_OPTIONS[name]
  
  cfg.RegisterCommonOption NAME, name, default
  LOADED_OPTIONS[name] = description
  table.insert LOADED_OPTIONS.__order, name


--- Register a provided statusline parameter function
-- Canonicalizes the parameter name, and places the provided information into
-- the LOADED_PARAMS table for later usage
add_statusinfo = (name, fn, description) ->
  return if LOADED_LINEFNS[name]

  app.SetStatusInfoFn "#{NAME}.#{name}"
  LOADED_LINEFNS[name] = description
  table.insert LOADED_LINEFNS.__order, name

--- Generates the help page from the various registered components, and loads it
generate_help = ->
  commands_help = "# Microgit\n#{DESC}\n\n## Commands"
  for name in *LOADED_COMMANDS.__order
    debug "Adding #{name} to help"
    command =  LOADED_COMMANDS[name]
    commands_help ..= "\n* %pub%.#{name}"
    continue if not command.help

    on_line = 1
    margin = ''
    
    each_line command.help, (line, _, total) ->
      return if on_line == 1 and line\match"^%s*$"
      margin = line\match"^(%s*).+$" if on_line == 1
      line = line\gsub(margin, "", 1)
      return if (on_line >= total and line\match"^%s*$")
      commands_help ..= "\n>  #{line}"
      on_line += 1

    commands_help ..= "\n"


  options_help = "# Microgit\n#{DESC}\n\n## Options"
  for name in *LOADED_OPTIONS.__order
    debug "Adding #{name} to help"
    options_help ..= "\n* %NAME%.#{name}"
    continue if not LOADED_OPTIONS[name]

    on_line = 1
    margin = ''
    
    each_line LOADED_OPTIONS[name], (line, _, total) ->
      return if on_line == 1 and line\match"^%s*$"
      margin = line\match"^(%s*).+$" if on_line == 1
      line = line\gsub(margin, "", 1)
      return if (on_line >= total and line\match"^%s*$")
      options_help ..= "\n>  #{line}"
      on_line += 1

    options_help ..= "\n"
    

  statusline_help = "# Microgit\n#{DESC}\n\n## Statusline Help"
  for name in *LOADED_LINEFNS.__order
    debug "Adding #{name} to help"
    statusline_help ..= "\n* %NAME%.#{name}"
    continue if not LOADED_LINEFNS[name]

    on_line = 1
    margin = ''
    
    each_line LOADED_LINEFNS[name], (line, _, total) ->
      return if on_line == 1 and line\match"^%s*$"
      margin = line\match"^(%s*).+$" if on_line == 1
      line = line\gsub(margin, "", 1)
      return if (on_line >= total and line\match"^%s*$")
      statusline_help ..= "\n>  #{line}"
      on_line += 1

    statusline_help ..= "\n"

  options_help = str.Replace options_help, '%pub%', 'git', -1
  options_help = str.Replace options_help, '%NAME%', NAME, -1
  commands_help = str.Replace commands_help, '%pub%', 'git', -1
  commands_help = str.Replace commands_help, '%NAME%', NAME, -1    
  statusline_help = str.Replace statusline_help, '%pub%', 'git', -1
  statusline_help = str.Replace statusline_help, '%NAME%', NAME, -1
  
  cfg.AddRuntimeFileFromMemory cfg.RTHelp, "#{NAME}.commands", commands_help
  cfg.AddRuntimeFileFromMemory cfg.RTHelp, "#{NAME}.options", options_help
  cfg.AddRuntimeFileFromMemory cfg.RTHelp, "#{NAME}.statusline", statusline_help

--- Generate a function that takes a number, and returns the correct plurality of a word
wordify = (word, singular, plural) ->
  singular = word .. singular
  plural   = word .. plural
  (number) ->
    number != 1 and plural or singular


--- If a path is accessible, return true. Otherwise, false
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


--- Create a new readonly scratch pane with the contents of output
--
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


--- Create a new readonly scratch pane and return it
make_empty_pane = (root, rszfn, header) ->
  old_view = root\GetView!
  h = old_view.Height
  pane = root\HSplitIndex(buf.NewBuffer("", header), true)
  pane\ResizePane rszfn h
  pane.Buf.Type.Scratch = true
  pane.Buf.Type.Readonly = true
  pane.Buf.Type.Syntax = false
  pane.Buf\SetOptionNative "softwrap", true
  pane.Buf\SetOptionNative "ruler", false
  pane.Buf\SetOptionNative "autosave", false
  pane.Buf\SetOptionNative "statusformatr", ""
  pane.Buf\SetOptionNative "statusformatl", header
  pane.Buf\SetOptionNative "scrollbar", false
  pane.Cursor.Loc.Y = 0
  pane.Cursor.Loc.X = 0
  return pane



--- Create a new readonly scratch pane with the contents of output. Add that
-- pane to the list of ACTIVE_COMMITS, and write the contents of output to
-- a temporary file.
--
-- The provided callback function should have the signature with string being
-- a filepath.
--   (string) ->
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
    done: false
    root: root
  }


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
    
    return abs, pwd, split_path[bound(l, 1, l)]
)!


git = (->
  w_commit  = wordify 'commit', '', 's'
  w_line    = wordify 'line', '', 's'
  re_commit = regexp.MustCompile"^commit[\\s]+([^\\s]+).*$"

  --- Generate a new git command context for the filepath. All git commands
  -- run through this context will run with -C "filepath"
  new_command = (filepath) ->
    if type(filepath) != 'string' or filepath == ''
      debug "filepath [#{filepath}] is not a valid editor path (need string): (got: #{type filepath})"
      return nil, "Please run this in a file pane"

    abs, dir, name = get_path_info filepath

    --- Execute a git command with arguments and return the output
    exec = (...) ->
      unless path_exists dir
        return nil, "directory #{dir} does not exist"

      debug "Parent directory #{dir} exists, continuing ..."
      base = cfg.GetGlobalOption "#{NAME}.command"
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


    --- Execute the git command with arguments in the background, and fill a new
    -- pane with the contents of it's stdout.
    exec_async = (cmd, ...) =>
      unless path_exists dir
        return nil, "directory #{dir} does not exist"

      debug "Parent directory #{dir} exists, continuing ..."
      base = cfg.GetGlobalOption "#{NAME}.command"
      if base == ""
        base, _ = shl.ExecCommand "command", "-v", "git"
        base = chomp base
        if base == '' or not base
          return nil, "no git configured"

      debug "Found valid git path: #{base}"
      unless path_exists base
        return nil, err.Error!

      resize_fn = (h) -> h - ( h / 3 )
      pane = make_empty_pane self, resize_fn, "git-#{cmd}"

      on_emit = (_str, _) ->
        pane.Buf\Write _str
        return

      on_exit = (_, _) ->
        pane.Buf\Write "\n[command has completed, ctrl-q to exit]\n"
        return

      args = {...}
      table.insert args, 1, cmd
      shl.JobSpawn base, args, on_emit, on_emit, on_exit
      return "", nil

    --- Return true or false dependent on whether or not the context is a repo
    in_repo = ->
      out, _ = exec "rev-parse", "--is-inside-work-tree"
      return chomp(out) == 'true'


    --- Parse all of the known branches and return both those branches, and the 
    -- name of the current branch
    get_branches = ->
      out, _ = exec "branch", "-al"
      branches = {}
      current = ''

      each_line chomp(out), (line) ->
        debug "Attempting to match: #{line}"
        cur, name = line\match "^%s*(%*?)%s*([^%s]+)"
        if name
          if cur == '*'
            current = name
        else
          name = cur
        return unless name

        debug "Found branch: #{name}"
        revision, err = exec "rev-parse", name
        if err and err != ""
          debug "Failed to rev-parse #{name}: #{err}"
          return

        table.insert branches,
          commit: chomp revision
          :name

      return branches, current


    --- Return the revision hash for a given label, or false
    known_label = (label) ->
      out, err = exec "rev-parse", "--quiet", "--verify", label
      unless (err and err != "") or (out and out != "")
        return false, err
      return chomp(out)

    return { :new, :exec, :exec_async, :in_repo, :known_label, :get_branches }


  --- Issue a message to the buffer with a neat syntax.
  -- If a string has more than one line, use a pane. Otherwise, issue a message
  -- via the infobar
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


  --- Update branch tracked branch information for a buffer
  update_branch_status = (cmd) =>
    debug "update_branch_status: Update initiated"

    unless @Path
      debug "update_branch_status: was called with a non-buffer object!"
      return
    
    return unless cfg.GetGlobalOption "#{NAME}.updateinfo"
    return unless (not @Type.Scratch) and (@Path != '')
    
    debug "update_branch_status: Beginning update process for #{self}"

    unless cmd
      cmd, err = new_command @Path
      return send.updater (err .. " (to suppress this message, set git.statusline to false)") unless cmd    

    local branch

    debug "update_branch_status: Getting branch label ..."

    return unless cmd.in_repo!
    out, err = cmd.exec "branch", "--show-current"
    unless err
      branch = chomp out
    else
      return

    BUFFER_BRANCH[@Path] = branch
    BRANCH_STATUS[branch] = { 
      name: (branch or '')
      ahead: "-"
      behind: "-"
      staged: "-"
      :commit
    }

    return unless branch

    debug "update_branch_status: Getting HEAD short hash ..."
    out, err = cmd.exec "rev-parse", "--short", branch
    unless err
      BRANCH_STATUS[branch].commit = chomp out

    if branch
      debug "update_branch_status: Getting revision count differences"
      out, err = cmd.exec "rev-list", "--left-right", "--count", "origin/#{branch}...#{branch}"
      unless err
        a, b = (chomp out)\match("^(%d+)%s+(%d+)$")
        BRANCH_STATUS[branch].ahead = a or "-"
        BRANCH_STATUS[branch].behind = b or "-"

    debug "update_branch_status: Getting staged count"
    out, err = cmd.exec "diff", "--name-only", "--cached"
    unless err
      staged = select(2, (chomp out)\gsub("([^%s\r\n]+)", ''))
      BRANCH_STATUS[branch].staged = staged

    debug "update_branch_status: Done"

  return {
    :update_branch_status
      
    init: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.init err
      
      unless not cmd.in_repo!
        return send.init errors.is_a_repo
      out, err = cmd.exec "init"
      return send.init err if err
      send.init out
      
    init_help: [[
      Usage: %pub%.init
        Initialize a repository in the current panes directory
    ]]
    
    fetch: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.fetch err
      unless cmd.in_repo!
        return send.fetch errors.not_a_repo
      out, err = cmd.exec "fetch"
      return send.fetch err if err
      send.fetch out

    fetch_help: [[
      Usage: %pub%.fetch
        Fetch latest changes from remotes
    ]]

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

    checkout_help: [[
      Usage: %pub%.help <label>
        Checkout a specific branch, tag, or revision
    ]]

    list: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.list err
      
      unless cmd.in_repo!
        return send.checkout errors.not_a_repo

      branches, current = cmd.get_branches!
      output   = ''

      output ..= "Branches:\n"
      for branch in *branches
        if branch.name == current
          output ..= "-> "
        else
          output ..= "   "
        output ..= "#{branch.name} - rev:#{branch.commit}\n"

      return send.list_branches output

    list_help: [[
      Usage: %pub%.list
        List branches, and note the currently active branch
    ]]

      
    status: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.status err
    
      unless cmd.in_repo!
        return send.status errors.not_a_repo

      status_out, err = cmd.exec "status"
      return send.status err if err
      send.status status_out

    status_help: [[
      Usage: %pub%.status
        Show current status of the active repo
    ]]
    
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
        out ..= "> git fetch\n"
        out ..= "#{fetch_out}"

        if rev = cmd.known_label label
          return send.branch errors.invalid_arg ..
            ", please supply an unused label (#{label} is rev:#{rev})"

        branch_out, err = cmd.exec "branch", label
        out ..= "> git branch #{label}\n"
        out ..= branch_out

        unless err
          chkout_out, _ = cmd.exec "checkout", label
          out ..= "> git checkout #{label}\n"
          out ..= chkout_out

        send.branch out
    )!

    branch_help: [[
      Usage: %pub%.branch <label>
        Create a new local branch, and switch to it, also note that it performs a 
        git-fetch prior to making any changes.
    ]]

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
        each_line chomp(status_out), (line) ->
          commit_msg_start ..= "# #{line}\n"
          
        header = "[new commit: save and quit to finalize]"
        
        make_commit_pane self, commit_msg_start, header, (file, _) ->
          commit_msg = ioutil.ReadFile file
          commit_msg = str.TrimSuffix commit_msg, commit_msg_start
          
          if commit_msg == ""
            return send.commit "Aborting, empty commit"

          final_commit = ''
          each_line chomp(commit_msg), (line) ->
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

    commit_help: [[
      Usage: %pub%.commit [<commit message>]
        Begin a new commit. If a commit message is not provided, opens a new
        pane to enter the desired message into. Commit is initiated when the
        pane is saved and then closed.
    ]]

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

        _, err = cmd.exec_async self, "push", branch
        return send.push err if err
        return
    )!

    push_help: [[
      Usage: %pub%.push [<label>]
        Push local changes onto remote. A branch label is optional, and limits
        the scope of the push to the provided branch. Otherwise, all changes
        are pushed.
    ]]

    pull: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.pull err
      
      unless cmd.in_repo!
        return send.pull errors.not_a_repo
      
      pull_out, err = cmd.exec "pull"
      return send.pull err if err
      send.pull pull_out

    pull_help: [[
      Usage: %pub%.pull
        Pull all changes from remote into the working tree
    ]]

    log: =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.log err
      
      unless cmd.in_repo!
        return send.log errors.not_a_repo

      count = 0
      out, err = cmd.exec "log"
      return send.log if err
      each_line chomp(out), (line) ->
        count += 1 if re_commit\MatchString line

      send.log out, {
        header: "#{count} #{w_commit count}"
      }

    log_help: [[
      Usage: %pub%.log
        Show the commit log
    ]]

    stage: (...) =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.stage err

      unless cmd.in_repo!
        return send.stage errors.not_a_repo
      
      files = {}
      for file in *{...}
        continue if file == ".."
        if file == "--all"
          files = { "." }
          break

        unless path_exists file
          return send.stage errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless #files > 0
        return send.stage errors.not_enough_args .. ", please supply a file"

      cmd.exec "add", unpack files

    stage_help: [[
      Usage: %pub%.stage [<file1>, <file2>, ...] [<options>]
        Stage a file (or files) to commit.

      Options:
        --all   Stage all files
    ]]

    unstage: (...) =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.unstage err

      unless cmd.in_repo!
        return send.unstage errors.not_a_repo
      
      files = {}
      all = false
      for file in *{...}
        continue if file == ".."
        if file == "--all"
          files = {}
          all = true
          break

        unless path_exists file
          return send.unstage errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless (#files > 0) or all
        return send.unstage errors.not_enough_args .. ", please supply a file"

      cmd.exec "reset", "--", unpack files

    unstage_help: [[
      Usage: %pub%.unstage [<file1>, <file2>, ...] [<options>]
        Unstage a file (or files) to commit.

      Options:
        --all   Unstage all files
    ]]

    rm: (...) =>
      cmd, err = new_command @Buf.Path
      unless cmd
        return send.rm err

      unless cmd.in_repo!
        return send.add errors.not_a_repo
    
      files = {}
      for file in *{...}
        continue if file == ".."
        if file == "."
          files = { "." }
          break

        unless path_exists file
          return send.rm errors.invalid_arg .. "(file #{file} doesn't exist)"

        table.insert files, file

      unless #files > 0
        return send.rm errors.not_enough_args .. ", please supply a file"

      cmd.exec "rm", unpack files

    rm_help: [[
      Usage: %pub%.rm [<file1>, <file2>, ...]
        Stage the removal of a file (or files) from the git repo.
    ]]
  }
)!

export numahead = =>
  return "-" unless BUFFER_BRANCH[@Path]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[@Path]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[@Path]].ahead

export numbehind = =>
  return "-" unless BUFFER_BRANCH[@Path]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[@Path]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[@Path]].behind

export numstaged = =>
  return "-" unless BUFFER_BRANCH[@Path]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[@Path]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[@Path]].staged

export oncommit = =>
  return "-" unless BUFFER_BRANCH[@Path]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[@Path]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[@Path]].commit

export onbranch = =>
  return tostring(BUFFER_BRANCH[@Path] or "")


export preinit = ->
  add_config "command", "", [[
    The absolute path to the command to use for git operations (type: string) 
  ]]

  add_config "updateinfo", true, [[
    Update tracked branch information during select callbacks (type: boolean)

    Note: Required for statusline
  ]]

  add_statusinfo "numahead", numahead, [[
    The number of commits ahead of your branches origin (type: number)
  ]]
  
  add_statusinfo "numbehind", numbehind, [[
    The number of commits behind of origin your branches tree is (type: number)
  ]]
  
  add_statusinfo "numstaged", numstaged, [[
    The number of files staged in the local branch (type: number)
  ]]
  
  add_statusinfo "onbranch", onbranch, [[
    The current branch of a pane
  ]]
  
  add_statusinfo "oncommit", oncommit, [[
    The latest commit short hash
  ]]

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

export init = ->
  debug "Initializing #{NAME}"

  cmd = tostring cfg.GetGlobalOption "#{NAMES}.command"
  if cmd == ""
    cmd, _ = shl.ExecCommand "command", "-v", "git"
    if cmd == '' or not cmd
      app.TermMessage "#{NAME}: git not present in $PATH or set, some functionality will not work correctly"

  add_command "init", git.init, cfg.NoComplete
  add_command "pull", git.pull, cfg.NoComplete
  add_command "push", git.push, cfg.NoComplete
  add_command "list", git.list, cfg.NoComplete
  add_command "log", git.log, cfg.NoComplete
  add_command "commit", git.commit, cfg.NoComplete
  add_command "status", git.status, cfg.NoComplete
  add_command "branch", git.branch, cfg.NoComplete
  add_command "checkout", git.checkout, cfg.NoComplete
  add_command "stage", git.stage, cfg.FileComplete
  add_command "unstage", git.unstage, cfg.FileComplete
  add_command "rm", git.rm, cfg.FileComplete

  generate_help!


--- Populate branch tracking information for the buffer
export onBufPaneOpen = =>
  debug "Caught onBufPaneOpen bufpane:#{self}"
  git.update_branch_status @Buf

--- Update branch tracking for the buffer, and if its a commit pane mark it as
-- ready to commit
export onSave = =>
  debug "Caught onSave bufpane:#{self}"
  git.update_branch_status @Buf
  return unless #ACTIVE_COMMITS > 0

  for i, commit in ipairs ACTIVE_COMMITS
    if commit.pane == @
      debug "Marking commit #{i} as ready ..."
      commit.ready = true
      break

--- Remove a buffers path from tracking, and if we are in a commit pane call its
-- callback function if it's been modified and saved. Alternatively, hijack the
-- commit save prompt and offer a confirmation to save and commit.
export onQuit = =>
  debug "Caught onQuit, buf:#{@}"

  if @Path and BUFFER_BRANCH[@Path]
    BUFFER_BRANCH[@Path] = nil
    
  return unless #ACTIVE_COMMITS > 0

  debug "Populating temporary table for active commits ..."
  active = [commit for commit in *ACTIVE_COMMITS]

  debug "Iterating through known commits ..."
  for i, commit in ipairs ACTIVE_COMMITS
    if commit.pane == self
      if commit.ready
        debug "Commit #{i} is ready, fulfilling active commit ..."
        commit.callback commit.file
        for t, _temp in ipairs active
          if _temp == commit
            table.remove active, t
            ACTIVE_COMMITS = active
            break
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
          
          info\YNPrompt "Would you like to save and commit? (y,n,esc)",
            (yes, cancelled) ->
              return if cancelled
              
              if yes
                @Buf\Save!
                @ForceQuit!
                
                commit.callback commit.file
                debug "Removing #{commit.file}"
                os.Remove commit.file
                debug "Popping commit #{i} from stack"
                
                for t, _temp in ipairs ACTIVE_COMMITS
                  if _temp == commit
                    table.remove ACTIVE_COMMITS, t
                    break
                    
                return
              else
                info\Message "Aborted commit (closed before saving)"
                @ForceQuit!
              return
        break
