export VERSION = "1.0.0"
export NAME    = 'microgit'
export DESC    = 'Git for Micro'

local git

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

ACTIVE_UPDATES = {}
ACTIVE_COMMITS = {}
BRANCH_STATUS = {}
BUFFER_BRANCH = {}

LOADED_COMMANDS = { __order: {} } unless LOADED_COMMANDS
LOADED_OPTIONS = { __order: {} } unless LOADED_OPTIONS
LOADED_LINEFNS = { __order: {} } unless LOADED_LINEFNS

bound  = (n, min, max) -> n > max and max or (n < min and min or n)
debug  = (m) -> app.Log "#{NAME}: #{m}"
truthy = (v) -> if v then return true else false
errors = {
  is_a_repo: "the current file is already in a repository"
  not_a_repo: "the current file is not in a repository"
  invalid_arg: "invalid_argument provided"
  bad_label_arg: "invalid argument, please provide a valid rev label"
  not_enough_args: "not enough arguments"
  unknown_label: "given label is not known to this repo"
  command_not_found: "invalid command provided (not a command)"
  no_help: "FIXME: no help for command git."
  no_scratch: "this cannot be run out of a temporary pane"
}


--- Delete leading and trailing spaces, and the final newline
chomp = (s) ->
  s = s\gsub("^%s*", "")\gsub("%s*$", "")\gsub("[\n\r]*$", "")
  return s


--- Reimplementation of util.ReplaceHome, as it's not exposed to lua
replace_home = (_path) ->
  switch true
    when truthy str.HasPrefix(_path, "~")
      home, err = os.UserHomeDir!
      return nil, err if err
      return str.Replace _path, "~", 1
    when truthy str.HasPrefix(_path, "%USERPROFILE%")
      home, err = os.UserHomeDir!
      return nil, err if err
      return str.Replace _path, "%USERPROFILE%", 1
  return _path


--- Run a given function for each line in a string
each_line = (input, fn) ->
  input = str.Replace input, "\r\n", "\n", -1
  input = str.Replace input, "\n\r", "\n", -1
  lines = str.Split(input, "\n")
  l_count = #lines

  local finish_ret, finish_err
  stop = false
  finish = (v, err) ->
    stop = true
    finish_ret = v
    finish_err = err if err

  for i = 1, l_count
    break if stop
    fn lines[i], i, l_count, finish

  return finish_ret, finish_err

-- filepath.Abs and filepath.IsAbs both exist, however, their use in Lua code
-- here currently panics the application. Until then, we'll just have to rely
-- on something hacky in the meantime. This is pretty gross, but it works.
get_path_info = (->
  s = string.char os.PathSeparator

  re_abs  = regexp.MustCompile("^#{runtime.GOOS == 'windows' and '[a-zA-Z]:' or ''}#{s}#{s}?.+#{s}#{s}?.*")
  re_part = regexp.MustCompile("#{s}#{s}?")
  re_root = regexp.MustCompile("^#{runtime.GOOS == 'windows' and '[a-zA-Z]:' or ''}#{s}#{s}?$")

  return (_string) ->
    pwd, err = os.Getwd!
    assert not err, "failed to get current working directory"

    -- String is just the root filepath (ie; /)
    if re_root\Match _string
      return _string, _string, _string, pwd

    -- String is absolute (ie; /path/to/file)
    if re_abs\Match _string
      split_path = re_part\Split _string, -1
      l = #split_path
      name = split_path[bound(l, 1, l)]
      return _string, str.TrimSuffix(_string, name), name, pwd

    -- String is relative (ie; path/to/file)
    abs = (pwd .. s .. _string)
    split_path = re_part\Split abs, -1
    l = #split_path
    name = split_path[bound(l, 1, l)]
    return abs, str.TrimSuffix(abs, name), name, pwd
)!

--- Register a provided function+callback as a command
-- Wraps the given function to account for Micros handling of arguments placing
-- additional arguments of len size > 1 in a Go array
add_command = (name, fn, cb) ->
  return if LOADED_COMMANDS[name]

  external_name = "git.#{name}"
  cmd = (any, extra) ->
    local _path, _buf

    if any and any.Buf then
      _buf = any.Buf
    else
      _buf = any

    local dir, abs, name, pwd, _finfo
    if _buf.Path and _buf.Path != ''
      abs, dir, name, pwd = get_path_info _buf.Path
      _finfo = { :dir, :abs, :name, :pwd }
    
    debug "command[#{external_name}] started"
    fn any, _finfo, unpack([a for a in *(extra or {})])  
    git.update_branch_status _buf, _finfo
    git.update_git_diff_base _buf, _finfo
    debug "command[#{external_name}] completed"
    
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

--- Generates our help pages for the various registered components and loads them
generate_help = (-> 
  get_parser = ->
    on_line, margin, parsed = 1, '', ''
    _get_result = -> parsed
    _parse_line = (line, _, total) ->
      return if on_line == 1 and line\match"^%s*$"
      margin = line\match"^(%s*).+$" if on_line == 1
      line = line\gsub(margin, "", 1)
      return if (on_line >= total and line\match"^%s*$")
      parsed ..= "\n>  #{line}"
      on_line += 1
    return _parse_line, _get_result

  return ->
    commands_help = "# Microgit\n#{DESC}\n\n## Commands"
    for name in *LOADED_COMMANDS.__order
      debug "Adding #{name} to help"
      commands_help ..= "\n* %pub%.#{name}"
      continue if not LOADED_COMMANDS[name].help
      parser, parser_result = get_parser!
      each_line LOADED_COMMANDS[name].help, parser
      commands_help ..= "#{parser_result!}\n"

    options_help = "# Microgit\n#{DESC}\n\n## Options"
    for name in *LOADED_OPTIONS.__order
      debug "Adding #{name} to help"
      options_help ..= "\n* %NAME%.#{name}"
      continue if not LOADED_OPTIONS[name]
      parser, parser_result = get_parser!
      each_line LOADED_OPTIONS[name], parser
      options_help ..= "#{parser_result!}\n"

    statusline_help = "# Microgit\n#{DESC}\n\n## Statusline Help"
    for name in *LOADED_LINEFNS.__order
      debug "Adding #{name} to help"
      statusline_help ..= "\n* %NAME%.#{name}"
      continue if not LOADED_LINEFNS[name]
      parser, parser_result = get_parser!
      each_line LOADED_LINEFNS[name], parser
      statusline_help ..= "#{parser_result!}\n"

    options_help = str.Replace options_help, '%pub%', 'git', -1
    options_help = str.Replace options_help, '%NAME%', NAME, -1
    commands_help = str.Replace commands_help, '%pub%', 'git', -1
    commands_help = str.Replace commands_help, '%NAME%', NAME, -1
    statusline_help = str.Replace statusline_help, '%pub%', 'git', -1
    statusline_help = str.Replace statusline_help, '%NAME%', NAME, -1
    
    cfg.AddRuntimeFileFromMemory cfg.RTHelp, "#{NAME}.commands", commands_help
    cfg.AddRuntimeFileFromMemory cfg.RTHelp, "#{NAME}.options", options_help
    cfg.AddRuntimeFileFromMemory cfg.RTHelp, "#{NAME}.statusline", statusline_help
)!

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
  return (header='XXX') ->
    dir = path.Join "#{cfg.ConfigDir}", "tmp"
    err = os.MkdirAll(dir, 0x1F8) -- 770, to account for octal
    assert not err, err
    
    file = ("#{NAME}.#{header}.") .. ("XXXXXXXXXXXX")\gsub '[xX]', =>
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
    send_pane.Buf\SetOptionNative "diffgutter", false
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
  pane.Buf\SetOptionNative "diffgutter", false
  pane.Cursor.Loc.Y = 0
  pane.Cursor.Loc.X = 0
  return pane


--- Create a new pane with the contents of output. Add that
-- pane to the list of ACTIVE_COMMITS, and write the contents of output to
-- a temporary file.
--
-- The provided callback function should have the signature with string being
-- a filepath.
--   (string) ->
make_commit_pane = (root, output, header, fn) ->
  old_view = root\GetView!
  h = old_view.Height

  filepath = make_temp 'commit'

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
  commit_pane.Buf\SetOptionNative "diffgutter", false
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


git = (->
  w_commit  = wordify 'commit', '', 's'
  w_line    = wordify 'line', '', 's'
  re_commit = regexp.MustCompile"^commit[\\s]+([^\\s]+).*$"
  is_scratch = => @Type.Scratch

  --- Issue a message to the buffer with a neat syntax.
  -- If a string has more than one line, use a pane. Otherwise, issue a message
  -- via the infobar
  send = setmetatable {}, __index: (_, cmd) ->
    cmd = cmd\gsub "_", "-"
    (msg, config) ->
      debug "git-#{cmd}: Issuing message - #{msg}"
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

  --- Generate a new git command context for the directory. All git commands
  -- run through this context will run with -C "directory"
  new_command = (directory) ->
    if type(directory) != 'string' or directory == ''
      debug "filepath '#{filepath}' is not a valid editor path (need non-empty string): (got: #{type filepath})"
      return nil, "Please run this in a file pane"

    --- Execute a git command with arguments and return the output
    exec = (command, ...) ->
      unless path_exists directory
        return nil, "directory #{directory} does not exist"

      debug "Parent directory #{directory} exists, continuing ..."
      base = cfg.GetGlobalOption "#{NAME}.command"
      if base == ""
        base, _ = shl.ExecCommand "command", "-v", "git"
        base = chomp base
        if base == '' or not base
          return nil, "no git configured"

      debug "Found valid git path: #{base}"
      unless path_exists base
        return nil, err.Error!

      out = shl.ExecCommand base, "-C", directory, command, ...
      return out


    --- Execute the git command with arguments in the background, and fill a new
    -- pane with the contents of it's stdout.
    exec_async = (cmd, ...) =>
      unless path_exists directory
        return nil, "directory #{directory} does not exist"

      debug "Parent directory #{directory} exists, continuing ..."
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
      table.insert args, 1, directory
      table.insert args, 1, "-C"
      shl.JobSpawn base, args, on_emit, on_emit, on_exit
      return "", nil

    exec_async_cb = (cmd, on_stdout, on_stderr, on_exit, ...) ->
      unless path_exists directory
        return nil, "directory #{directory} does not exist"

      debug "Parent directory #{directory} exists, continuing ..."
      base = cfg.GetGlobalOption "#{NAME}.command"
      if base == ""
        base, _ = shl.ExecCommand "command", "-v", "git"
        base = chomp base
        if base == '' or not base
          return nil, "no git configured"

      debug "Found valid git path: #{base}"
      unless path_exists base
        return nil, err.Error!

      on_stdout = assert on_stdout, "exec_async_cb requires an on_stdout callback"
      on_stderr = assert on_stderr, "exec_async_cb requires an on_stderr callback"
      on_exit = assert on_exit, "exec_async_cb requires an on_exit callback"

      args = {...}
      table.insert args, 1, cmd
      table.insert args, 1, directory
      table.insert args, 1, "-C"
      debug "Launching: #{base} #{str.Join args, " "}"
      shl.JobSpawn base, args, on_stdout, on_stderr, on_exit
      return
    

    --- Return true or false dependent on whether or not the context is a repo
    in_repo = ->
      out, err = exec "rev-parse", "--is-inside-work-tree"
      return send.in_repo err if err
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

    return {
      :new, :exec, :exec_async,
      :exec_async_cb, :in_repo,
      :known_label, :get_branches
    }


  --- Update branch tracked branch information for a buffer
  update_branch_status = (finfo, cmd) =>
    return unless truthy cfg.GetGlobalOption "#{NAME}.updateinfo"
    return unless (not @Type.Scratch) and (@Path != '') and finfo

    debug "update_branch_status: Beginning update process for #{self}"
    unless cmd
      cmd, err = new_command finfo.dir
      return send.updater (err .. " (to suppress this message, set #{NAME}.updateinfo to false)") unless cmd    

    debug "update_branch_status: Getting branch label ..."
    return unless cmd.in_repo!
    branch, err = cmd.exec "branch", "--show-current"
    return if err
    
    branch = chomp branch
    BUFFER_BRANCH[finfo.abs] = branch
    return unless branch
    
    BRANCH_STATUS[branch] = BRANCH_STATUS[branch] or { 
      name: (branch or '')
      ahead: "-"
      behind: "-"
      staged: "-"
      :commit
    }

    return if BRANCH_STATUS[branch].__updating
    BRANCH_STATUS[branch].__updating = true

    diff_string  = ''
    short_commit = ''
    count_staged = ''
    count_behind = 0
    count_ahead  = 0

    debug "update_branch_status: Generating diff finalizer fn ..."
    finish_update = ->
      count_staged = select(2, (chomp count_staged)\gsub("([^%s\r\n]+)", ''))
      BRANCH_STATUS[branch] = {
        __updating: false
        staged: count_staged
        commit: short_commit
        behind: count_behind
        ahead: count_ahead
        name: branch
      }

    start_get_countstaged = ->
      a, b = (chomp diff_string)\match("^(%d+)%s+(%d+)$")
      count_ahead, count_behind = (a or "-"), (b or "-")
      cmd.exec_async_cb "diff",
        ((out) -> count_staged ..= out),
        ((out) -> count_staged ..= out),
        finish_update,
        "--name-only",
        "--cached"
        
    start_get_diffstring = ->
      short_commit = chomp(short_commit)
      cmd.exec_async_cb "rev-list",
        ((out) -> diff_string ..= out),
        ((out) -> diff_string ..= out),
        start_get_countstaged,
        "--left-right",
        "--count",
        ("origin/#{branch}...#{branch}")

    cmd.exec_async_cb "rev-parse",
      ((out) -> short_commit ..= out),
      ((out) -> short_commit ..= out),
      start_get_diffstring,
      "--short",
      branch


  suppress = " (to suppress this message, set #{NAME}.gitgutter to false)"
  update_git_diff_base = (finfo, cmd) =>
    return unless truthy cfg.GetGlobalOption "#{NAME}.gitgutter"
    return unless @Settings["diffgutter"] and finfo and (not @Type.Scratch) and (@Path != '')

    return if ACTIVE_UPDATES[finfo.abs]
    ACTIVE_UPDATES[finfo.abs] = true

    debug "update_branch_status: Beginning update process for #{self}"
    unless cmd
      cmd, err = new_command finfo.dir
      return send.diffupdate "#{err}#{suppress}" unless cmd  

    return unless cmd.in_repo!

    repo_relative_path = ''
    top_level = ''
    diff_base = ''

    start_set_diffbase = ->
      diff_base = @Bytes! unless diff_base and diff_base != ''
      @SetDiffBase diff_base
      ACTIVE_UPDATES[finfo.abs] = false

    start_get_diffbase = ->
      repo_relative_path = str.TrimPrefix finfo.abs, chomp(top_level)
      cmd.exec_async_cb "show",
        ((out) -> diff_base ..= out),
        ((out) -> diff_base ..= out),
        start_set_diffbase,
        ":./#{repo_relative_path}"

    cmd.exec_async_cb "rev-parse",
      ((out) -> top_level ..= out),
      ((out) -> top_level ..= out),
      start_get_diffbase,
      "--show-toplevel"  

  return {
    :update_branch_status
    :update_git_diff_base
      
    init: (finfo) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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

    diff: (finfo) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
      unless cmd
        return send.diff err

      unless cmd.in_repo!
        return send.diff errors.not_a_repo

      repo_relative_path = str.TrimPrefix finfo.abs, finfo.pwd
      repo_relative_file = str.TrimPrefix @Buf.Path, repo_relative_path
      out, err = cmd.exec "diff", repo_relative_file
      return send.diff err if err
      send.diff out

    diff_help: [[
      Usage: %pub%.diff
        Git diff HEAD for the current file
    ]]
    
    fetch: (finfo) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
      unless cmd
        return send.fetch err
      unless cmd.in_repo!
        return send.fetch errors.not_a_repo
      _, err = cmd.exec_async self, "fetch"
      return send.fetch err if err
      return

    fetch_help: [[
      Usage: %pub%.fetch
        Fetch latest changes from remotes
    ]]

    checkout: (->
      re_valid_label = regexp.MustCompile"^[a-zA-Z-_/.]+$"

      return (finfo, label) =>
        return send.init errors.no_scratch if is_scratch @Buf
        cmd, err = new_command finfo.dir
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

    list: (finfo) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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

      
    status: (finfo) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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

      return (finfo, label) =>
        return send.init errors.no_scratch if is_scratch @Buf
        cmd, err = new_command finfo.dir
        unless cmd
          return send.branch err
        
        unless cmd.in_repo!
          return send.branch errors.not_a_repo
          
        unless re_valid_label\Match label
          return send.branch errors.invalid_lbl

        if rev = cmd.known_label label
          return send.branch errors.invalid_arg ..
            ", please supply an unused label (#{label} is rev:#{rev})"

        branch_out, err = cmd.exec "branch", label
        out = "> git branch #{label}\n"
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

      return (finfo, msg) =>
        return send.init errors.no_scratch if is_scratch @Buf
        cmd, err = new_command finfo.dir
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
      
      return (finfo, branch) =>
        return send.init errors.no_scratch if is_scratch @Buf
        cmd, err = new_command finfo.dir
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

    pull: (finfo) =>
      cmd, err = new_command finfo.dir
      unless cmd
        return send.pull err
      
      unless cmd.in_repo!
        return send.pull errors.not_a_repo
      
      _, err = cmd.exec_async self, "pull"
      return send.pull err if err
      return

    pull_help: [[
      Usage: %pub%.pull
        Pull all changes from remote into the working tree
    ]]

    log: (finfo) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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

    stage: (finfo, ...) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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

        file, err = replace_home file
        return send.stage err if err
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

    unstage: (finfo, ...) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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
          
        file, err = replace_home file
        return send.unstage err if err
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

    rm: (finfo, ...) =>
      return send.init errors.no_scratch if is_scratch @Buf
      cmd, err = new_command finfo.dir
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

        file, err = replace_home file
        return send.rm err if err
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
  return "-" unless @Path and @Path != ''
  abs = get_path_info @Path
  return "-" unless BUFFER_BRANCH[abs]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[abs]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[abs]].ahead

export numbehind = =>
  return "-" unless @Path and @Path != ''
  abs = get_path_info @Path
  return "-" unless BUFFER_BRANCH[abs]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[abs]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[abs]].behind

export numstaged = =>
  return "-" unless @Path and @Path != ''
  abs = get_path_info @Path
  return "-" unless BUFFER_BRANCH[abs]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[abs]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[abs]].staged

export oncommit = =>
  return "-" unless @Path and @Path != ''
  abs = get_path_info @Path
  return "-" unless BUFFER_BRANCH[abs]
  return "-" unless BRANCH_STATUS[BUFFER_BRANCH[abs]]
  return tostring BRANCH_STATUS[BUFFER_BRANCH[abs]].commit

export onbranch = =>
  return "-" unless @Path and @Path != ''
  abs = get_path_info @Path
  return "-" unless BUFFER_BRANCH[abs]
  return tostring(BUFFER_BRANCH[abs] or "")

export preinit = ->
  add_config "command", "", [[
    The absolute path to the command to use for git operations (type: string) 
  ]]

  add_config "updateinfo", true, [[
    Update tracked branch information during select callbacks (type: boolean)

    Note: Required for statusline
  ]]

  add_config "gitgutter", true, [[
    Enable or disable updating the diff gutter with git changes (type: boolean)

    Note: To use this, ensure diffgutter is enabled
  ]]

  add_config "cleanstale", true, [[
    Enable or disable whether this plugin deletes it's old tempfiles on startup (type: boolean)
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

  if truthy cfg.GetGlobalOption "#{NAME}.cleanstale"
    debug "Clearing stale temporary files ..."
    pfx = "#{NAME}."
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
  add_command "fetch", git.fetch, cfg.NoComplete
  add_command "checkout", git.checkout, cfg.NoComplete
  add_command "stage", git.stage, cfg.FileComplete
  add_command "unstage", git.unstage, cfg.FileComplete
  add_command "rm", git.rm, cfg.FileComplete
  add_command "diff", git.diff, cfg.FileComplete

  generate_help!


--- Populate branch tracking information for the buffer
export onBufPaneOpen = =>
  debug "Caught onBufPaneOpen bufpane:#{self}"
  abs, dir, name, pwd = get_path_info @Buf.Path
  _finfo = {:dir, :abs, :name, :pwd}
  git.update_branch_status @Buf, _finfo
  git.update_git_diff_base @Buf, _finfo
  return

--- Update branch tracking for the buffer, and if its a commit pane mark it as
-- ready to commit
export onSave = =>
  debug "Caught onSave bufpane:#{self}"
  abs, dir, name, pwd = get_path_info @Buf.Path
  _finfo = {:dir, :abs, :name, :pwd}
  git.update_branch_status @Buf, _finfo
  git.update_git_diff_base @Buf, _finfo
  return unless #ACTIVE_COMMITS > 0

  for i, commit in ipairs ACTIVE_COMMITS
    if commit.pane == @
      debug "Marking commit #{i} as ready ..."
      commit.ready = true
      break

  return

--- Remove a buffers path from tracking, and if we are in a commit pane call its
-- callback function if it's been modified and saved. Alternatively, hijack the
-- commit save prompt and offer a confirmation to save and commit.
export onQuit = =>
  debug "Caught onQuit, buf:#{@}"

  if @Path and @Path != ''
    _, abs, _ = get_path_info @Path
    if BUFFER_BRANCH[abs]
      BUFFER_BRANCH[abs] = nil
    
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
  return
