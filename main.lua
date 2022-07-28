VERSION = "1.0.0"
NAME = 'gitstatus'
local go = assert(loadstring([[  -- script: lua
  return ({
    import = function (pkg)
      return import(pkg)
    end
  })
  -- script: end
]])())
local os = go.import("os")
local app = go.import("micro")
local buf = go.import("micro/buffer")
local cfg = go.import("micro/config")
local shl = go.import("micro/shell")
local str = go.import("strings")
local path = go.import("path")
local fpath = go.import("path/file")
local ioutil = go.import("ioutil")
local regexp = go.import("regexp")
local runtime = go.import("runtime")
local ACTIVE_COMMITS = { }
local errors = {
  is_a_repo = "the current directory is already a repository",
  not_a_repo = "the current directory is not a repository",
  invalid_arg = "invalid_argument provided",
  bad_label_arg = "invalid argument, please provide a valid rev label",
  not_enough_args = "not enough arguments",
  unknown_label = "given label is not known to this repo"
}
local debug
debug = function(m)
  return app.Log(tostring(NAME) .. ": " .. tostring(m))
end
local chomp
chomp = function(s)
  s = s:gsub("^%s*", ""):gsub("%s*$", ""):gsub("[\n\r]*$", "")
  return s
end
local wordify
wordify = function(word, singular, plural)
  singular = word .. singular
  plural = word .. plural
  return function(number)
    return number ~= 1 and plural or singular
  end
end
local each_line
each_line = function(input, fn)
  input = str.Replace(chomp(input), "\r\n", "\n", -1)
  input = str.Replace(input, "\n\r", "\n", -1)
  local lines = str.Split(input, "\n")
  local l_count = #lines
  local stop = false
  local finish
  finish = function()
    stop = true
  end
  for i = 1, l_count do
    if stop then
      return 
    end
    fn(lines[i], finish)
  end
end
local path_exists
path_exists = function(filepath)
  local finfo, _ = os.Stat(filepath)
  return finfo ~= nil
end
local make_temp = (function()
  local rand = go.import("math/rand")
  local chars = 'qwertyuioasdfghjklzxcvbnm'
  return function()
    local dir = path.Join(tostring(cfg.ConfigDir), "tmp")
    local err = os.MkdirAll(dir, 0x1F8)
    assert(not err, err)
    local file = (tostring(NAME) .. ".commit.") .. ("XXXXXXXXXXXX"):gsub('[xX]', function(self)
      local i = rand.Intn(25) + 1
      local c = chars:sub(i, i)
      local _exp_0 = self
      if 'x' == _exp_0 then
        return c
      elseif 'X' == _exp_0 then
        return string.upper(c)
      end
    end)
    debug("Generated new tempfile: " .. tostring(dir) .. "/" .. tostring(file))
    return tostring(path.Join(dir, file))
  end
end)()
local send_block = (function()
  local re_special_chars = regexp.MustCompile("\\x1B\\[([0-9]{1,3}(;[0-9]{1,3})*)?[mGK]")
  return function(header, output)
    local old_view = (app.CurPane()):GetView()
    local h = old_view.Height
    output = re_special_chars:ReplaceAllString(output, "")
    local send_pane = (app.CurPane()):HSplitIndex(buf.NewBuffer(output, header), true)
    send_pane:ResizePane(h - (h / 5))
    send_pane.Buf.Type.Scratch = true
    send_pane.Buf.Type.Readonly = true
    send_pane.Buf.Type.Syntax = false
    send_pane.Buf:SetOptionNative("softwrap", true)
    send_pane.Buf:SetOptionNative("ruler", false)
    send_pane.Buf:SetOptionNative("autosave", false)
    send_pane.Buf:SetOptionNative("statusformatr", "")
    send_pane.Buf:SetOptionNative("statusformatl", header)
    send_pane.Buf:SetOptionNative("scrollbar", false)
    send_pane.Cursor.Loc.Y = 0
    send_pane.Cursor.Loc.X = 0
    return send_pane.Cursor:Relocate()
  end
end)()
local make_empty_pane
make_empty_pane = function(root, rszfn, header, fn)
  local old_view = root:GetView()
  local h = old_view.Height
  local pane = root:HSplitIndex(buf.NewBuffer("", header), true)
  pane:ResizePane(rszfn(h))
  pane.Buf.Type.Scratch = true
  pane.Buf.Type.Readonly = true
  pane.Buf.Type.Syntax = false
  pane.Buf:SetOptionNative("softwrap", true)
  pane.Buf:SetOptionNative("ruler", false)
  pane.Buf:SetOptionNative("autosave", false)
  pane.Buf:SetOptionNative("statusformatr", "")
  pane.Buf:SetOptionNative("statusformatl", header)
  pane.Buf:SetOptionNative("scrollbar", false)
  pane.Cursor.Loc.Y = 0
  pane.Cursor.Loc.X = 0
  return pane
end
local make_commit_pane
make_commit_pane = function(root, output, header, fn)
  local old_view = root:GetView()
  local h = old_view.Height
  local filepath = make_temp()
  debug("Populating temporary commit file " .. tostring(filepath) .. " ...")
  ioutil.WriteFile(filepath, output, 0x1B0)
  debug("Generating new buffer for " .. tostring(filepath))
  local commit_pane = (app.CurPane()):HSplitIndex(buf.NewBuffer(output, filepath), true)
  commit_pane:ResizePane(h - (h / 3))
  commit_pane.Buf.Type.Scratch = false
  commit_pane.Buf.Type.Readonly = false
  commit_pane.Buf.Type.Syntax = false
  commit_pane.Buf:SetOptionNative("softwrap", true)
  commit_pane.Buf:SetOptionNative("ruler", false)
  commit_pane.Buf:SetOptionNative("autosave", false)
  commit_pane.Buf:SetOptionNative("statusformatr", "")
  commit_pane.Buf:SetOptionNative("statusformatl", header)
  commit_pane.Buf:SetOptionNative("scrollbar", false)
  commit_pane.Cursor.Loc.Y = 0
  commit_pane.Cursor.Loc.X = 0
  commit_pane.Cursor:Relocate()
  return table.insert(ACTIVE_COMMITS, {
    callback = fn,
    pane = commit_pane,
    file = filepath,
    done = ready,
    root = root
  })
end
local git
local set_callbacks
local bound
bound = function(n, min, max)
  debug("bound: got: " .. tostring(n))
  return n > max and max or (n < min and min or n)
end
local get_path_info = (function()
  local s = string.char(os.PathSeparator)
  local re_abs = regexp.MustCompile("^" .. tostring(runtime.GOOS == 'windows' and '[a-zA-Z]:' or '') .. tostring(s) .. tostring(s) .. "?.+" .. tostring(s) .. tostring(s) .. "?.*")
  local re_part = regexp.MustCompile(tostring(s) .. tostring(s) .. "?")
  local re_root = regexp.MustCompile("^" .. tostring(runtime.GOOS == 'windows' and '[a-zA-Z]:' or '') .. tostring(s) .. tostring(s) .. "$")
  return function(_string)
    if re_root:Match(_string) then
      debug("get_path_info: " .. tostring(_string) .. " matched regexp[" .. tostring(re_root:String()) .. "]")
      return _string, _string, _string
    end
    if re_abs:Match(_string) then
      debug("get_path_info: " .. tostring(_string) .. " matched regexp[" .. tostring(re_abs:String()) .. "]")
      local split_path = re_part:Split(_string, -1)
      local l = #split_path
      return _string, str.TrimSuffix(_string, split_path[bound(l, 1, l)]), split_path[bound(l, 1, l)]
    end
    debug("get_path_info: " .. tostring(_string) .. " is relative")
    local pwd, err = os.Getwd()
    assert(not err, "failed to get current working directory")
    local abs = (pwd .. s .. _string)
    local split_path = re_part:Split(abs, -1)
    local l = #split_path
    debug("get_path_info: bound: " .. tostring(bound(l - 1, 1, l)))
    debug("get_path_info: Bound: " .. tostring(bound(l, 1, l)))
    return abs, pwd, split_path[bound(l, 1, l)]
  end
end)()
git = (function()
  local w_commit = wordify('commit', '', 's')
  local w_line = wordify('line', '', 's')
  local re_commit = regexp.MustCompile("^commit[\\s]+([^\\s]+).*$")
  local new_command
  new_command = function(filepath)
    if type(filepath) ~= 'string' or filepath == '' then
      debug("filepath [" .. tostring(filepath) .. "] is not a valid editor path (need string): (got: " .. tostring(type(filepath)) .. ")")
      return nil, "Please run this in a file pane"
    end
    local abs, dir, name = get_path_info(filepath)
    local exec
    exec = function(...)
      if not (path_exists(dir)) then
        return nil, "directory " .. tostring(dir) .. " does not exist"
      end
      debug("Parent directory " .. tostring(dir) .. " exists, continuing ...")
      local base = cfg.GetGlobalOption("git.path")
      if base == "" then
        local _
        base, _ = shl.ExecCommand("command", "-v", "git")
        base = chomp(base)
        if base == '' or not base then
          return nil, "no git configured"
        end
      end
      debug("Found valid git path: " .. tostring(base))
      if not (path_exists(base)) then
        return nil, err.Error()
      end
      debug("Running ...")
      local out = shl.ExecCommand(base, "-C", dir, ...)
      return out
    end
    local exec_async
    exec_async = function(self, cmd, ...)
      if not (path_exists(dir)) then
        return nil, "directory " .. tostring(dir) .. " does not exist"
      end
      debug("Parent directory " .. tostring(dir) .. " exists, continuing ...")
      local base = cfg.GetGlobalOption("git.path")
      if base == "" then
        local _
        base, _ = shl.ExecCommand("command", "-v", "git")
        base = chomp(base)
        if base == '' or not base then
          return nil, "no git configured"
        end
      end
      debug("Found valid git path: " .. tostring(base))
      if not (path_exists(base)) then
        return nil, err.Error()
      end
      debug("Running ...")
      local resize_fn
      resize_fn = function(h)
        return h - (h / 3)
      end
      local pane = make_empty_pane(self, resize_fn, "git-" .. tostring(cmd))
      local on_emit
      on_emit = function(_str, _)
        pane.Buf:Write(_str)
      end
      local on_exit
      on_exit = function(_, _)
        pane.Buf:Write("\n[command has completed, ctrl-q to exit]\n")
      end
      local args = {
        ...
      }
      table.insert(args, 1, cmd)
      shl.JobSpawn(base, args, on_emit, on_emit, on_exit)
      return "", nil
    end
    local in_repo
    in_repo = function()
      local out, _ = exec("rev-parse", "--is-inside-work-tree")
      return chomp(out) == 'true'
    end
    local get_branches
    get_branches = function()
      local out, _ = exec("branch", "-al")
      local branches = { }
      each_line(out, function(line)
        debug("Attempting to match: " .. tostring(line))
        name = line:match("^%s*%*?%s*([^%s]+)")
        if not (name) then
          return 
        end
        debug("Found branch: " .. tostring(name))
        local revision, err = exec("rev-parse", name)
        if err and err ~= "" then
          debug("Failed to rev-parse " .. tostring(name) .. ": " .. tostring(err))
          return 
        end
        return table.insert(branches, {
          commit = chomp(revision),
          name = name
        })
      end)
      return branches
    end
    local known_label
    known_label = function(label)
      local out, err = exec("rev-parse", label)
      return err ~= "" and false or chomp(out)
    end
    return {
      new = new,
      exec = exec,
      exec_async = exec_async,
      in_repo = in_repo,
      known_label = known_label,
      get_branches = get_branches
    }
  end
  local send = setmetatable({ }, {
    __index = function(_, cmd)
      cmd = cmd:gsub("_", "-")
      return function(msg, config)
        local line_count = select(2, string.gsub(tostring(msg), "[\r\n]", ""))
        debug("LineCount: " .. tostring(line_count))
        if line_count > 1 then
          local header = "git-" .. tostring(cmd)
          if type(config) == "table" then
            if config.header ~= nil then
              header = tostring(header) .. ": " .. tostring(config.header)
            end
          end
          send_block(header, msg)
          return 
        end
        (app.InfoBar()):Message("git-" .. tostring(cmd) .. ": " .. tostring(msg))
      end
    end
  })
  return {
    init = function(self)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.init(err)
      end
      if not (not cmd.in_repo()) then
        return send.init(errors.is_a_repo)
      end
      local out
      out, err = cmd.exec("init")
      if err then
        return send.init(err)
      end
      return send.init(out)
    end,
    fetch = function(self)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.fetch(err)
      end
      if not (cmd.in_repo()) then
        return send.fetch(errors.not_a_repo)
      end
      local out
      out, err = cmd.exec("fetch")
      if err then
        return send.fetch(err)
      end
      return send.fetch(out)
    end,
    checkout = (function()
      local re_valid_label = regexp.MustCompile("^[a-zA-Z-_/.]+$")
      return function(self, label)
        local cmd, err = new_command(self.Buf.Path)
        if not (cmd) then
          return send.checkout(err)
        end
        if not (cmd.in_repo()) then
          return send.checkout(errors.not_a_repo)
        end
        if not (label ~= nil) then
          return send.checkout(errors.not_enough_args .. "(supply a branch/tag/commit)")
        end
        if not (re_valid_label:Match(label)) then
          return send.checkout(errors.bad_label_arg)
        end
        if not (cmd.known_label(label)) then
          return send.checkout(errors.unknown_label)
        end
        local out
        out, err = cmd.exec("checkout", label)
        if err then
          return send.checkout(err)
        end
        return send.checkout(out)
      end
    end)(),
    listbranches = function(self)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.list(err)
      end
      if not (cmd.in_repo()) then
        return send.checkout(errors.not_a_repo)
      end
      local branches = cmd.get_branches()
      local current = ''
      local output = ''
      if current_branch ~= '' then
        output = output .. "\nCurrent: " .. tostring(current) .. "\n"
      end
      output = output .. "Branches:\n"
      for _index_0 = 1, #branches do
        local branch = branches[_index_0]
        if branch.name == current then
          output = output .. "-> "
        else
          output = output .. "   "
        end
        output = output .. tostring(branch.name) .. " - rev:" .. tostring(branch.commit) .. "\n"
      end
      return send.list_branches(output)
    end,
    status = function(self)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.status(err)
      end
      if not (cmd.in_repo()) then
        return send.status(errors.not_a_repo)
      end
      local status_out
      status_out, err = cmd.exec("status")
      if err then
        return send.status(err)
      end
      return send.status(status_out)
    end,
    branch = (function()
      local re_valid_label = regexp.MustCompile("^[a-zA-Z-_/.]+$")
      return function(self, label)
        local cmd, err = new_command(self.Buf.Path)
        if not (cmd) then
          return send.branch(err)
        end
        if not (cmd.in_repo()) then
          return send.branch(errors.not_a_repo)
        end
        if not (re_valid_label:Match(label)) then
          return send.branch(errors.invalid_lbl)
        end
        local out = ''
        local fetch_out, _ = cmd.exec("fetch")
        out = out .. "> git fetch"
        out = out .. fetch_out
        do
          local rev = cmd.known_label(label)
          if rev then
            return send.branch(errors.invalid_arg .. ", please supply an unused label (" .. tostring(label) .. " is rev:" .. tostring(rev) .. ")")
          end
        end
        local branch_out
        branch_out, err = cmd.exec("branch", label)
        out = out .. "> git branch " .. tostring(label)
        out = out .. branch_out
        if not (err) then
          local chkout_out
          chkout_out, _ = cmd.exec("checkout", label)
          out = out .. "> git checkout " .. tostring(label)
          out = out .. chkout_out
        end
        return send.branch(out)
      end
    end)(),
    commit = (function()
      local msg_line = regexp.MustCompile("^\\s*([^#])")
      local base_msg = "\n"
      base_msg = base_msg .. "# Please enter the commit message for your changes. Lines starting\n"
      base_msg = base_msg .. "# with '#' will be ignored, and an empty message aborts the commit.\n#\n"
      return function(self, msg)
        local cmd, err = new_command(self.Buf.Path)
        if not (cmd) then
          return send.commit(err)
        end
        if not (cmd.in_repo()) then
          return send.commit(errors.not_a_repo)
        end
        if msg then
          local commit_out
          commit_out, err = cmd.exec("commit", "-m", msg)
          if err then
            return send.commit(err)
          end
          return send.commit(commit_out)
        end
        local commit_msg_start = base_msg
        local status_out, _ = cmd.exec("status")
        each_line(status_out, function(line)
          commit_msg_start = commit_msg_start .. "# " .. tostring(line) .. "\n"
        end)
        local header = "[new commit: save and quit to finalize]"
        make_commit_pane(self, commit_msg_start, header, function(file, _)
          local commit_msg = ioutil.ReadFile(file)
          commit_msg = str.TrimSuffix(commit_msg, commit_msg_start)
          if commit_msg == "" then
            return send.commit("Aborting, empty commit")
          end
          local final_commit = ''
          each_line(commit_msg, function(line, final)
            if line == nil then
              return 
            end
            line = chomp(line)
            if msg_line:Match(line) then
              final_commit = final_commit .. tostring(line) .. "\n"
            end
          end)
          ioutil.WriteFile(file, final_commit, 0x1B0)
          if "" == chomp(final_commit) then
            return send.commit("Aborting, empty commit")
          end
          local commit_out
          commit_out, err = cmd.exec("commit", "-F", file)
          if err then
            return send.commit(err)
          end
          return send.commit(commit_out)
        end)
        debug("Awaiting commit completion within onQuit")
      end
    end)(),
    push = (function()
      local re_valid_label = regexp.MustCompile("^[a-zA-Z-_/.]+$")
      return function(self, branch)
        local cmd, err = new_command(self.Buf.Path)
        if not (cmd) then
          return send.push(err)
        end
        if not (cmd.in_repo()) then
          return send.push(errors.not_a_repo)
        end
        if branch ~= nil then
          if not (re_valid_label:Match(branch)) then
            return send.push(errors.bad_label_arg)
          end
        else
          branch = "--all"
        end
        local _
        _, err = cmd.exec_async(self, "push", branch)
        if err then
          return send.push(err)
        end
      end
    end)(),
    pull = function(self)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.pull(err)
      end
      if not (cmd.in_repo()) then
        return send.pull(errors.not_a_repo)
      end
      local pull_out
      pull_out, err = cmd.exec("pull")
      if err then
        return send.pull(err)
      end
      return send.pull(pull_out)
    end,
    log = function(self)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.log(err)
      end
      if not (cmd.in_repo()) then
        return send.log(errors.not_a_repo)
      end
      local count = 0
      local out
      out, err = cmd.exec("log")
      if err then
        return send.log
      end
      each_line(out, function(line)
        if re_commit:MatchString(line) then
          count = count + 1
        end
      end)
      return send.log(out, {
        header = tostring(count) .. " " .. tostring(w_commit(count))
      })
    end,
    add = function(self, ...)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.add(err)
      end
      if not (cmd.in_repo()) then
        return send.add(errors.not_a_repo)
      end
      local files = { }
      local _list_0 = {
        ...
      }
      for _index_0 = 1, #_list_0 do
        local file = _list_0[_index_0]
        if file == "." then
          files = {
            "."
          }
          break
        end
        if not (path_exists(file)) then
          return send.add(errors.invalid_arg .. "(file " .. tostring(file) .. " doesn't exist)")
        end
        table.insert(files, file)
      end
      if not (#files > 0) then
        return send.add(errors.not_enough_args .. ", please supply a file")
      end
      return cmd.exec("add", unpack(files))
    end,
    rm = function(self, ...)
      local cmd, err = new_command(self.Buf.Path)
      if not (cmd) then
        return send.rm(err)
      end
      if not (cmd.in_repo()) then
        return send.add(errors.not_a_repo)
      end
      local files = { }
      local _list_0 = {
        ...
      }
      for _index_0 = 1, #_list_0 do
        local file = _list_0[_index_0]
        if file == "." then
          files = {
            "."
          }
          break
        end
        if not (path_exists(file)) then
          return send.rm(errors.invalid_arg .. "(file " .. tostring(file) .. " doesn't exist)")
        end
        table.insert(files, file)
      end
      if not (#files > 0) then
        return send.rm(errors.not_enough_args .. ", please supply a file")
      end
      return cmd.exec("rm", unpack(files))
    end
  }
end)()
cfg.RegisterCommonOption("git", "path", "")
cfg.RegisterCommonOption("git", "onsave", true)
cfg.RegisterCommonOption("git", "status_line", true)
local registerCommand
registerCommand = function(name, fn, cb)
  local cmd
  cmd = function(any, extra)
    debug("command[" .. tostring(name) .. "] started")
    if extra then
      fn(any, unpack((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #extra do
          local a = extra[_index_0]
          _accum_0[_len_0] = a
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)()))
    else
      fn(any)
    end
    return debug("command[" .. tostring(name) .. "] completed")
  end
  return cfg.MakeCommand(name, cmd, cb)
end
init = function()
  debug("Initializing " .. tostring(NAME))
  local cmd = cfg.GetGlobalOption("git.path")
  if cmd == "" then
    local _
    cmd, _ = shl.ExecCommand("command", "-v", "git")
    if cmd == '' or not cmd then
      app.TermMessage(tostring(NAME) .. ": git not present in $PATH or set, some functionality will not work correctly")
    end
  end
  registerCommand("git.init", git.init, cfg.NoComplete)
  registerCommand("git.pull", git.pull, cfg.NoComplete)
  registerCommand("git.push", git.push, cfg.NoComplete)
  registerCommand("git.list", git.listbranches, cfg.NoComplete)
  registerCommand("git.log", git.log, cfg.NoComplete)
  registerCommand("git.commit", git.commit, cfg.NoComplete)
  registerCommand("git.status", git.status, cfg.NoComplete)
  registerCommand("git.checkout", git.checkout, cfg.NoComplete)
  registerCommand("git.add", git.add, cfg.NoComplete)
  return registerCommand("git.rm", git.rm, cfg.NoComplete)
end
preinit = function()
  debug("Clearing stale commit files ...")
  local pfx = tostring(NAME) .. ".commit."
  local dir = path.Join(tostring(cfg.ConfigDir), "tmp")
  local files, err = ioutil.ReadDir(dir)
  if not (err) then
    for _index_0 = 1, #files do
      local f = files[_index_0]
      debug("Does " .. tostring(f:Name()) .. " have the prefix " .. tostring(pfx) .. "?")
      if str.HasPrefix(f:Name(), pfx) then
        local filepath = path.Join(dir, f:Name())
        debug("Clearing " .. tostring(filepath))
        os.Remove(filepath)
      end
    end
  end
end
onSave = function(self)
  if not (#ACTIVE_COMMITS > 0) then
    return 
  end
  for i, commit in ipairs(ACTIVE_COMMITS) do
    if commit.pane == self then
      debug("Marking commit " .. tostring(i) .. " as ready ...")
      commit.ready = true
      break
    end
  end
end
onQuit = function(self)
  debug("Caught onQuit, buf:" .. tostring(self))
  if not (#ACTIVE_COMMITS > 0) then
    return 
  end
  debug("Populating temporary table for active commits ...")
  local active
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #ACTIVE_COMMITS do
      local commit = ACTIVE_COMMITS[_index_0]
      _accum_0[_len_0] = commit
      _len_0 = _len_0 + 1
    end
    active = _accum_0
  end
  debug("Iterating through known commits ...")
  for i, commit in ipairs(ACTIVE_COMMITS) do
    if commit.pane == self then
      if commit.ready then
        debug("Commit " .. tostring(i) .. " is ready, fulfilling active commit ...")
        commit.callback(commit.file)
      else
        if self.Buf:Modified() then
          local info = app.InfoBar()
          if info.HasYN and info.HasPrompt then
            debug("Removing message: " .. tostring(info.Message))
            info.YNCallback = function() end
            info:AbortCommand()
          end
          local cancel = false
          info:YNPrompt("Would you like to save and commit? (y,n,esc)", function(yes, cancelled)
            if cancelled then
              cancel = true
              return 
            end
            if yes then
              self.Buf:Save()
              self:ForceQuit()
              commit.callback(commit.file)
              return 
            else
              info:Message("Aborted commit (closed before saving)")
            end
            self:ForceQuit()
          end)
          if cancel then
            return 
          end
        end
      end
      debug("Removing " .. tostring(commit.file))
      os.Remove(commit.file)
      debug("Popping commit " .. tostring(i) .. " from stack")
      for t, _temp in ipairs(active) do
        if _temp == commit then
          table.remove(active, t)
          ACTIVE_COMMITS = active
          break
        end
      end
      break
    end
  end
end
