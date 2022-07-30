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

GITLINE_ACTIVE = false
ACTIVE_UPDATES = {}
ACTIVE_COMMITS = {}
CALLBACKS_SET = {}
BUFFER_REPO = {}
REPO_STATUS = {}

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
  need_file: "this can only be run in a file pane"
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
      return str.Replace _path, "~", (home .. string.char os.PathSeparator), 1
    when truthy str.HasPrefix(_path, "%USERPROFILE%")
      home, err = os.UserHomeDir!
      return nil, err if err
      return str.Replace _path, "%USERPROFILE%", (home .. string.char os.PathSeparator), 1
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
  insert = table.insert
  windows = (runtime.GOOS == "windows" and true or false)

  re_part = regexp.MustCompile "[^#{s}]+"
  re_root = regexp.MustCompile "^#{windows and '[a-zA-Z]:' or ''}#{s}#{s}?"

  convert = (goarray) ->
    tbl, len = {}, #goarray
    for i = 1, len
      insert tbl, goarray[i]
    return tbl

  has_root = (s) ->
    re_root\Match s

  array = (tbl) ->
    len = #tbl
    i = 0
    return ->
      i += 1
      return if i > len
      return i, tbl[i], len

  return (filepath="") ->
    return nil unless filepath != "" and type(filepath) == "string"
  
    pwd, err = os.Getwd!
    assert not err, "failed to get current working directory"

    work_string = filepath
    unless has_root filepath  
      work_string = pwd .. s .. work_string

    skip = 0
    canon_split = {}
    for i, ent, len in array re_part\FindAllString work_string, -1
      switch true
        when ent == "."
          continue
        when ent == ".."
          skip += 1
          continue
        when skip > 0
          skip -= 1
          continue
        when skip > len - i
          return nil, "get_path_info: #{work_string} invalid path, too many parent traversals"

      insert canon_split, ent

    absolute = str.Join canon_split, s
    unless windows and (absolute\sub 1, 1) == s
      absolute = s .. absolute
  
    canon_split = re_part\FindAllString absolute, -1

    len = #canon_split
    name = canon_split[len]
    parent = (str.TrimSuffix absolute, name) or ""
    parent = (str.TrimSuffix parent, s) or ""

    return absolute, parent, name, pwd
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

    local _finfo
    abs, dir, name, pwd = get_path_info _buf.Path
    _finfo = { :dir, :abs, :name, :pwd } if pwd
    
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


--- Add a function to the list of callbacks for the given callback string
add_callback = (callback, fn) ->
  unless CALLBACKS_SET[callback]
    CALLBACKS_SET[callback] = {}
  table.insert CALLBACKS_SET[callback], fn


--- Run all callbacks for the given callback against the arguments provided
-- If callback returns a truthy value, it is removed from the list. Otherwise
-- it is retained
run_callbacks = (callback, ...) ->
  active = {}
  for i, fn in ipairs (CALLBACKS_SET["onQuit"] or {})
    unless fn ...
      table.insert active, fn
  CALLBACKS_SET["onQuit"] = active
      

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


--- Create a new readonly scratch hsplit pane and return it
--
-- Initially inspired by filemanager
-- https://github.com/micro-editor/updated-plugins/blob/master/filemanager-plugin/filemanager.lua
make_empty_hsplit = (root, rszfn, header, output, filepath) ->
  if not output and not filepath
    output = ""

  local pane
  old_view = root\GetView!
  h = old_view.Height
  if filepath
    pane = root\HSplitIndex(buf.NewBufferFromFile(filepath), true)
  else
    pane = root\HSplitIndex(buf.NewBuffer(output, ""), true)
    
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
  pane.Cursor\Relocate!
  return pane

--- Create a new readonly scratch vsplit pane and return it
make_empty_vsplit = (root, rszfn, header, output, filepath) ->
  local pane
  old_view = root\GetView!
  w = old_view.Width
  if filepath
    pane = root\VSplitIndex(buf.NewBufferFromFile(filepath, true), true)
  else
    pane = root\VSplitIndex(buf.NewBuffer(output, ""), true)
  pane\ResizePane rszfn w
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
  pane.Cursor\Relocate!
  return pane

  
--- Create a new readonly scratch pane with the contents of output
send_block = (header, output, syntax=false) ->
  pane = make_empty_hsplit app.CurPane!, ((h) -> h - (h / 5)), header, output
  pane.Buf.Type.Syntax = truthy syntax


git = (->
  w_commit  = wordify 'commit', '', 's'
  w_line    = wordify 'line', '', 's'
  re_commit = regexp.MustCompile"^commit[\\s]+([^\\s]+).*$"
  is_scratch = => @Type.Scratch

  --- Issue a message to the buffer with a neat syntax.
  -- If a string has more than one line, use a pane. Otherwise, issue a message
  -- via the infobar
  send = setmetatable {}, __index: (_, cmd) ->
    (msg, config) ->
      debug "git-#{cmd}: Issuing message - #{msg}"
      line_count = select(2, string.gsub(tostring(msg), "[\r\n]", ""))

      debug "LineCount: #{line_count}"
      if line_count > 1
        header = "git.#{cmd}"
        if type(config) == "table"
          if config.header != nil
            header = "#{header}: #{config.header}"
        send_block header, msg
        return

      (app.InfoBar!)\Message "git.#{cmd}: #{msg}"
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
      pane = make_empty_hsplit self, resize_fn, "git-#{cmd}"

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


    top_level = ->
      out, err = exec "rev-parse", "--show-toplevel"
      return nil unless out
      return chomp out

    --- Parse all of the known branches and return both those branches, and the 
    -- name of the current branch
    get_branches = ->
      out, err = exec "branch", "-al"
      if err
        send.get_branches "failed to get branches: #{err}"
        return nil, nil
      
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
      :known_label, :get_branches,
      :top_level
    }


  --- Update branch tracked branch information for a buffer
  update_branch_status = (->
    re_sha_sum = regexp.MustCompile"^\\s*([0-9a-zA-Z]{40}?)\\s*$"
    (finfo, cmd) =>
      GITLINE_ACTIVE = truthy cfg.GetGlobalOption "#{NAME}.updateinfo"
      return unless GITLINE_ACTIVE
      return unless finfo and (not @Type.Scratch) and (@Path != '')

      unless cmd
        cmd, err = new_command finfo.dir
        return send.updater (err .. " (to suppress this message, set #{NAME}.updateinfo to false)") unless cmd    

      return unless cmd.in_repo!
      
      if BUFFER_REPO[finfo.abs]
        return if BUFFER_REPO[finfo.abs].__updating
        if BUFFER_REPO[finfo.abs].branch
          if REPO_STATUS[BUFFER_REPO[finfo.abs].branch]
            return if REPO_STATUS[BUFFER_REPO[finfo.abs].branch].__updating
      
      branch = ''
      diff_string  = ''
      short_commit = ''
      first_commit = ''
      count_staged = ''
      count_behind = ''
      count_ahead  = ''

      first_commit_err = ''
      short_commit_err = ''
      count_staged_err = ''
      diff_string_err = ''
      branch_err = ''

      detached = false

      set_empty = (reason) ->
        debug "update_branch_status: Setting empty tableset for #{finfo.abs}: #{reason}"
        BUFFER_REPO[finfo.abs] = { repoid: false, branch: false }

      debug "update_branch_status: Beginning update process for #{self}"
      
      finish_update = ->
        unless count_staged_err == ''
          return set_empty "error encountered getting list of staged files: #{count_staged_err}"
      
        count_staged = select(2, (chomp count_staged)\gsub("([^%s\r\n]+)", ''))
        REPO_STATUS[first_commit][branch].staged = count_staged
        REPO_STATUS[first_commit][branch].__updating = false
        BUFFER_REPO[finfo.abs].__updating = false

      start_get_countstaged = ->
        unless diff_string_err == ''
          return set_empty "error encountered getting diff string: #{diff_string_err}"

        a, b = 0, 0
        unless diff_string and diff_string != ''
          originless = false
          a, b = (chomp diff_string)\match("^(%d+)%s+(%d+)$")
        
        REPO_STATUS[first_commit][branch].ahead = a
        REPO_STATUS[first_commit][branch].behind = b
          
        cmd.exec_async_cb "diff",
          ((out) -> count_staged ..= out),
          ((err) -> count_staged_err ..= err),
          finish_update,
          "--name-only",
          "--cached"
          
      start_get_diffstring = ->
        unless short_commit and short_commit != ''
          return set_empty "got empty short commit"
        unless short_commit_err == ''
          return set_empty "error encountered getting short commit: #{short_commit_err}"

        revlist_str = "#{branch}...#{branch}"
        revlist_str = "origin/#{revlist_str}" unless detached
        
        short_commit = chomp short_commit
        REPO_STATUS[first_commit][branch].commit = short_commit        
        cmd.exec_async_cb "rev-list",
          ((out) -> diff_string ..= out),
          ((err) -> diff_string_err ..= err),
          start_get_countstaged,
          "--left-right",
          "--count",
          revlist_str

      start_get_detached = ->
        unless branch and branch != ''
          return set_empty "got empty branch list"
        unless branch_err == ''
          return set_empty "error encountered getting branch: #{branch_err}"

        local commit
        return set_empty "failed to parse state:" unless each_line chomp(branch),
          (line, i, len, final) ->
            _hash = line\match'^%s*%*%s*%(%s*HEAD%s+detached%s+at%s+([0-9A-Za-z]+)%s*%)%s*$'
            if _hash
              commit = _hash
              return final true

        branch, err = cmd.exec "rev-parse", commit
        if err or not branch
          return set_empty "could not match detached HEAD to a revision: #{err}"

        branch = chomp branch
        unless REPO_STATUS[first_commit][branch]
          REPO_STATUS[first_commit][branch] = {
            name: branch
          }

        unless BUFFER_REPO[finfo.abs]
          BUFFER_REPO[finfo.abs] = {}
        BUFFER_REPO[finfo.abs].repoid = first_commit
        BUFFER_REPO[finfo.abs].branch = branch
        BUFFER_REPO[finfo.abs].display = "HEAD:#{commit}"
        
        cmd.exec_async_cb "rev-parse",
          ((out) -> short_commit ..= out),
          ((err) -> short_commit_err ..= err),
          start_get_diffstring,
          "--short",
          branch

      start_get_commit = ->
        unless branch and branch != ''
          branch = ''
          branch_err = ''
          detached = true
          return cmd.exec_async_cb "branch",
            ((out) -> branch ..= out),
            ((err) -> branch_err ..= err),
            start_get_detached,
            "--contains",
            "HEAD"

        unless branch_err == ''
          return set_empty "error encountered getting branch: #{branch_err}"

        branch = chomp branch
        unless REPO_STATUS[first_commit][branch]
          REPO_STATUS[first_commit][branch] = {
            name: branch
          }

        unless BUFFER_REPO[finfo.abs]
          BUFFER_REPO[finfo.abs] = {}
        BUFFER_REPO[finfo.abs].repoid = first_commit
        BUFFER_REPO[finfo.abs].branch = branch
        
        cmd.exec_async_cb "rev-parse",
          ((out) -> short_commit ..= out),
          ((err) -> short_commit_err ..= err),
          start_get_diffstring,
          "--short",
          branch


      start_get_branch = ->
        unless first_commit and first_commit != ''
          return set_empty "got empty first commit" 
        unless first_commit_err == ''
          return set_empty "error encountered getting first revision: #{first_commit_err}"
        return set_empty "failed to parse first commit" unless each_line chomp(first_commit),
          (line, i, len, final) ->
            _hash = re_sha_sum\FindString line
            return final false unless _hash and _hash != ''
            first_commit = _hash
            return final true
        
        unless REPO_STATUS[first_commit]
          REPO_STATUS[first_commit] = {}

        unless BUFFER_REPO[finfo.abs]
          BUFFER_REPO[finfo.abs] = {}

        BUFFER_REPO[finfo.abs].repoid = first_commit

        cmd.exec_async_cb "branch",
          ((out) -> branch ..= out),
          ((err) -> branch_err ..= err),
          start_get_commit,
          "--show-current"

      cmd.exec_async_cb "rev-list",
        ((out) -> first_commit ..= out),
        ((err) -> first_commit_err ..= err),
        start_get_branch,
        "--parents",
        "HEAD",
        "--reverse"
  )!


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

  --- Create a new pane with the contents of output. Add that
  -- pane to the list of ACTIVE_COMMITS, and write the contents of output to
  -- a temporary file.
  --
  -- The provided callback function should have the signature with string being
  -- a filepath.
  --   (string) ->
  make_commit_pane = (root, cmd, output, fn) ->
    filepath = make_temp 'commit'
    ioutil.WriteFile filepath, output, 0x1B0 -- 0660, to account for octal
    
    commit_header = "[new commit: save and quit to finalize]"
    commit_pane = make_empty_hsplit root, ((h) -> h - (h / 3)), commit_header, output, filepath
    commit_pane.Buf.Type.Scratch = false
    commit_pane.Buf.Type.Readonly = false

    callback = fn
    diff_header = "[changes staged for commit]"
    diff_output = cmd.exec "diff", "--cached"
    if diff_output != ''
      closed = false
      diff_pane = make_empty_vsplit commit_pane, ((w) -> w / 2 ), diff_header, diff_output
      diff_pane.Buf.Type.Scratch = false

      add_callback "onQuit", (any) ->
        if (any == diff_pane) or (any == diff_pane.Buf)
          closed = true
        return closed
        
      callback = (...) ->
        diff_pane\ForceQuit! unless closed
        closed = true
        fn ...

    table.insert ACTIVE_COMMITS, {
      pane: commit_pane
      file: filepath
      done: false
      root: root
      :callback
    }

  return {
    :update_branch_status
    :update_git_diff_base
      
    init: (finfo) =>
      return send.init errors.need_file unless finfo
      return send.init errors.no_scratch if is_scratch @Buf

      cmd, err = new_command finfo.dir
      return send.init err unless cmd
      return send.init errors.is_a_repo if cmd.in_repo!
        
      out, err = cmd.exec "init"
      return send.init err if err
      send.init out
      
    init_help: [[
      Usage: %pub%.init
        Initialize a repository in the current panes directory
    ]]

    diff: (finfo, ...) =>
      return send.diff errors.need_file unless finfo
      return send.diff errors.no_scratch if is_scratch @Buf
      
      cmd, err = new_command finfo.dir
      return send.diff err unless cmd
      return send.diff errors.not_a_repo unless cmd.in_repo!

      diff_all, diff_staged, header = false, false, ''
      diff_args = {}

      if ...
        for a in *{...}
          switch a
            when '--all', '-a'
              diff_all = true
            when '--staged', '-s'
              diff_staged = true

      if diff_staged
        table.insert diff_args, "--cached"
        header ..= "(staged) "

      if not diff_all
        repo_relative_file = str.TrimPrefix finfo.abs, cmd.top_level!
        repo_relative_file = str.TrimPrefix repo_relative_file, "/"
        table.insert diff_args, "./" .. repo_relative_file
        header ..= "REPO:./#{repo_relative_file}"
      else
        header ..= "(showing all changes)"

      out, err = cmd.exec "diff", unpack diff_args
      out = "no changes to diff" if chomp(out) == ''
      return send.diff err if err
      send.diff out, :header
      
    diff_help: [[
      Usage: %pub%.diff
        Git diff HEAD for the current file

      Options:
        -s --staged   Include staged files
        -a --all       Diff entire repository
    ]]
    
    fetch: (finfo) =>
      return send.fetch errors.need_file unless finfo
      return send.fetch errors.no_scratch if is_scratch @Buf
      
      cmd, err = new_command finfo.dir
      return send.fetch err unless cmd
      return send.fetch errors.not_a_repo unless cmd.in_repo!
        
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
        return send.checkout errors.need_file unless finfo
        return send.checkout errors.no_scratch if is_scratch @Buf
        
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
      return send.list errors.need_file unless finfo
      return send.list errors.no_scratch if is_scratch @Buf
      
      cmd, err = new_command finfo.dir
      unless cmd
        return send.list err
      
      unless cmd.in_repo!
        return send.checkout errors.not_a_repo

      branches, current = cmd.get_branches!
      return unless branches
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
      return send.status errors.need_file unless finfo
      return send.status errors.no_scratch if is_scratch @Buf
      
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
        return send.branch errors.need_file unless finfo
        return send.branch errors.no_scratch if is_scratch @Buf
        
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
        return send.commit errors.need_file unless finfo
        return send.commit errors.no_scratch if is_scratch @Buf
        
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
        
        make_commit_pane self, cmd, commit_msg_start, (file, _) ->
          return unless file
          
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
        return send.push errors.need_file unless finfo
        return send.push errors.no_scratch if is_scratch @Buf
        
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
      return send.pull errors.need_file unless finfo
      return send.pull errors.no_scratch if is_scratch @Buf
    
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
      return send.log errors.need_file unless finfo
      return send.log errors.no_scratch if is_scratch @Buf
      
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
      return send.stage errors.need_file unless finfo
      return send.stage errors.no_scratch if is_scratch @Buf
      
      cmd, err = new_command finfo.dir
      unless cmd
        return send.stage err

      unless cmd.in_repo!
        return send.stage errors.not_a_repo
      
      files = {}
      for file in *{...}
        continue if file == ".."
        if file == "--all" or file =="-a"
          files = { "." }
          break

        file, err = replace_home file
        return send.stage err if err
        unless path_exists file
          return send.stage errors.invalid_arg .. ", file #{file} doesn't exist"

        table.insert files, file

      unless #files > 0
        return send.stage errors.not_enough_args .. ", please supply a file"

      cmd.exec "add", unpack files

    stage_help: [[
      Usage: %pub%.stage [<file1>, <file2>, ...] [<options>]
        Stage a file (or files) to commit.

      Options:
        -a --all   Stage all files
    ]]

    unstage: (finfo, ...) =>
      return send.unstage errors.need_file unless finfo
      return send.unstage errors.no_scratch if is_scratch @Buf
      
      cmd, err = new_command finfo.dir
      unless cmd
        return send.unstage err

      unless cmd.in_repo!
        return send.unstage errors.not_a_repo
      
      files = {}
      all = false
      for file in *{...}
        continue if file == ".."
        if file == "--all" or file == "-a"
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
        -a --all   Unstage all files
    ]]

    rm: (finfo, ...) =>
      return send.rm errors.need_file unless finfo
      return send.rm errors.no_scratch if is_scratch @Buf
      
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

    debug: (finfo, ...) =>
      return send.debug errors.need_file unless finfo
    
      debug_output = ''
      cmd, err = new_command finfo.dir
      unless cmd
        return send.debug err

      _, branch = cmd.get_branches!
      unless branch
        branch = "Error"        

      debug_output ..= "File: #{finfo.abs}\n"
      debug_output ..= "Name: #{finfo.name}\n"
      debug_output ..= "PWD: #{finfo.pwd}\n"
      debug_output ..= "Directory: #{finfo.dir}\n"
      debug_output ..= "Absolute Path: #{finfo.abs}\n"
      debug_output ..= "In Repo: #{cmd.in_repo! or false}\n"
      debug_output ..= "Branch: #{branch}\n"

      debug_output ..= "\n"
      debug_output ..= "_G.ACTIVE_UPDATES\n"
      for k, v in pairs ACTIVE_UPDATES
        debug_output ..= "  Updating Diff: #{k}: #{v}\n"

      debug_output ..= "_G.ACTIVE_COMMITS\n"
      for k, v in pairs ACTIVE_COMMITS
        debug_output ..= "  Active Commits: #{k}: #{v}\n"
        
      debug_output ..= "_G.BUFFER_REPO\n"
      for k, v in pairs BUFFER_REPO
        debug_output ..= "  File: #{k}\n"
        debug_output ..= "    #{v.repoid}\n"
        debug_output ..= "    #{v.branch}\n"
        
      debug_output ..= "_G.REPO_STATUS\n"
      for k, v in pairs REPO_STATUS
        debug_output ..= "  Repo: #{k}\n"
        for b, data in pairs REPO_STATUS[k]
          debug_output ..= "    Branch: #{b}\n"
          debug_output ..= "      a:#{data.ahead}, b:#{data.behind}, "
          debug_output ..= "c:#{data.commit}, s:#{data.staged}\n"

      debug_output ..= "_G.CALLBACKS_SET\n"
      for k, v in pairs CALLBACKS_SET
        for cb, fn in pairs CALLBACKS_SET[k]
          debug_output ..= "  #{cb}:#{fn}"

      return send.debug debug_output
      
    debug_help: [[
      Usage: %pub%.debug
        Dumps plugin operational data for easy viewing
    ]]
  }
)!

export numahead = =>
  return "-" unless GITLINE_ACTIVE
  local repoid, branch, abs
  abs = get_path_info @Path
  return "-" unless abs
  return "-" unless BUFFER_REPO[abs]
  return "-" unless BUFFER_REPO[abs]
  repoid = BUFFER_REPO[abs].repoid
  branch = BUFFER_REPO[abs].branch
  return "-" unless repoid and branch
  return "-" unless REPO_STATUS[repoid]
  return "-" unless REPO_STATUS[repoid][branch]
  return tostring REPO_STATUS[repoid][branch].ahead

export numbehind = =>
  return "-" unless GITLINE_ACTIVE
  local repoid, branch, abs
  abs = get_path_info @Path
  return "-" unless abs
  return "-" unless BUFFER_REPO[abs]
  repoid = BUFFER_REPO[abs].repoid
  branch = BUFFER_REPO[abs].branch
  return "-" unless repoid and branch
  return "-" unless REPO_STATUS[repoid]
  return "-" unless REPO_STATUS[repoid][branch]
  return tostring REPO_STATUS[repoid][branch].behind

export numstaged = =>
  return "-" unless GITLINE_ACTIVE
  local repoid, branch, abs
  abs = get_path_info @Path
  return "-" unless abs
  return "-" unless BUFFER_REPO[abs]
  repoid = BUFFER_REPO[abs].repoid
  branch = BUFFER_REPO[abs].branch
  return "-" unless repoid and branch
  return "-" unless REPO_STATUS[repoid]
  return "-" unless REPO_STATUS[repoid][branch]
  return tostring REPO_STATUS[repoid][branch].staged

export oncommit = =>
  return "-" unless GITLINE_ACTIVE
  local repoid, branch, abs
  abs = get_path_info @Path
  return "-" unless abs
  return "-" unless BUFFER_REPO[abs]
  repoid = BUFFER_REPO[abs].repoid
  branch = BUFFER_REPO[abs].branch
  return "-" unless repoid and branch
  return "-" unless REPO_STATUS[repoid]
  return "-" unless REPO_STATUS[repoid][branch]
  return tostring REPO_STATUS[repoid][branch].commit

export onbranch = =>
  local repoid, branch, abs
  abs = get_path_info @Path
  return "-" unless abs
  return "-" unless BUFFER_REPO[abs]
  repoid = BUFFER_REPO[abs].repoid
  branch = BUFFER_REPO[abs].branch
  return tostring(BUFFER_REPO[abs].display or branch or "none")

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
  add_command "debug", git.debug, cfg.FileComplete

  generate_help!


--- Populate branch tracking information for the buffer
export onBufPaneOpen = =>
  debug "Caught onBufPaneOpen bufpane:#{self}"
  local _finfo
  abs, dir, name, pwd = get_path_info @Buf.Path
  _finfo = {:dir, :abs, :name, :pwd} if pwd
  git.update_branch_status @Buf, _finfo
  git.update_git_diff_base @Buf, _finfo
  return

--- Update branch tracking for the buffer, and if its a commit pane mark it as
-- ready to commit
export onSave = =>
  debug "Caught onSave bufpane:#{self}"
  local _finfo
  abs, dir, name, pwd = get_path_info @Buf.Path
  _finfo = {:dir, :abs, :name, :pwd} if pwd
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
  run_callbacks "onQuit", self

  _, abs, _ = get_path_info @Path
  if abs and BUFFER_REPO[abs]
    BUFFER_REPO[abs] = nil
    
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
        info = app.InfoBar!
        unless @Buf\Modified!
          info\Message "Aborted commit (closed without saving)"
          commit.callback false
          os.Remove commit.file
          for t, _temp in ipairs active
            if _temp == commit
              table.remove active, t
              ACTIVE_COMMITS = active
              break
        else
          if info.HasYN and info.HasPrompt
            info.YNCallback = ->
            info\AbortCommand!
          
          info\YNPrompt "Would you like to save and commit? (y,n,esc)",
            (yes, cancelled) ->
              return if cancelled
              
              if yes
                @Buf\Save!
                @ForceQuit!
                commit.callback commit.file
                os.Remove commit.file
              else
                info\Message "Aborted commit (closed without saving)"
                os.Remove commit.file
                commit.callback false
                @ForceQuit!

              for t, _temp in ipairs ACTIVE_COMMITS
                if _temp == commit
                  table.remove ACTIVE_COMMITS, t
                  break
  return
