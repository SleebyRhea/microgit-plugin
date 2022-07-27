_G.VERSION = "1.0.0"
_G.NAME = 'gitstatus'
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
local rgx = go.import("regexp")
local iou = go.import("ioutil")
local path = go.import("path")
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
local each_line = (function()
  str = go.import("strings")
  return function(input, fn)
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
end)()
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
  local re_special_chars = rgx.MustCompile("\\x1B\\[([0-9]{1,3}(;[0-9]{1,3})*)?[mGK]")
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
local make_commit_pane
make_commit_pane = function(output, header, fn)
  local old_view = (app.CurPane()):GetView()
  local h = old_view.Height
  local filepath = make_temp()
  debug("Populating temporary commit file " .. tostring(filepath) .. " ...")
  iou.WriteFile(filepath, output, 0x1B0)
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
  commit_pane.Buf:SetOptionNative("", false)
  commit_pane.Cursor.Loc.Y = 000
  commit_pane.Cursor.Loc.X = 0
  commit_pane.Cursor:Relocate()
  return table.insert(ACTIVE_COMMITS, {
    buffer = commit_pane,
    callback = fn,
    file = filepath,
    done = ready
  })
end
local git
local set_callbacks
git = (function()
  local w_commit = wordify('commit', '', 's')
  local w_line = wordify('line', '', 's')
  local re_commit = rgx.MustCompile("^commit[\\s]+([^\\s]+).*$")
  local exec
  exec = function(...)
    local cmd = cfg.GetGlobalOption("git.path")
    if cmd == "" then
      local _
      cmd, _ = shl.ExecCommand("command", "-v", "git")
      cmd = chomp(cmd)
      if cmd == '' or not cmd then
        return "", "no git configured"
      end
    end
    local finfo, err = os.Stat(cmd)
    if not (finfo) then
      return "", err.Error()
    end
    local out
    out, err = shl.ExecCommand(cmd, ...)
    return out, err
  end
  local in_repo
  in_repo = function()
    local out, _ = exec("rev-parse", "--is-inside-work-tree")
    return chomp(out) == 'true'
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
  local get_branches
  get_branches = function()
    local out, _ = exec("branch", "-al")
    local branches = { }
    each_line(out, function(line)
      debug("Attempting to match: " .. tostring(line))
      local name = line:match("^%s*%*?%s*([^%s]+)")
      if not (name) then
        return 
      end
      debug("Found branch: " .. tostring(name))
      local commit, err = exec("rev-parse", name)
      if err and err ~= "" then
        debug("Failed to rev-parse " .. tostring(name) .. ": " .. tostring(err))
        return 
      end
      return table.insert(branches, {
        commit = chomp(commit),
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
  local get_args
  get_args = function(goarray)
    return unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #goarray do
        local a = goarray[_index_0]
        _accum_0[_len_0] = a
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())
  end
  onSave = function(self)
    debug("Caught onSave, buf:" .. tostring(self))
    if not (in_repo()) then
      return 
    end
    if not (#ACTIVE_COMMITS > 0) then
      return 
    end
    for i, commit in ipairs(ACTIVE_COMMITS) do
      if commit.buffer == self then
        debug("Marking commit " .. tostring(i) .. " as ready ...")
        commit.ready = true
        break
      end
    end
  end
  return {
    init = function(self)
      if not (not in_repo()) then
        return send.init(errors.is_a_repo)
      end
      local out, err = exec("init")
      return send.init(out)
    end,
    fetch = function(self)
      if not (in_repo()) then
        return send.fetch(errors.not_a_repo)
      end
      local out, err = exec("fetch")
      return send.fetch(out)
    end,
    checkout = (function()
      local re_valid_label = rgx.MustCompile("^[a-zA-Z-_/.]+$")
      return function(self, argv)
        local label = get_args(argv)
        if not (in_repo()) then
          return send.checkout(errors.not_a_repo)
        end
        if not (label ~= nil) then
          return send.checkout(errors.not_enough_args .. "(supply a branch/tag/commit)")
        end
        if not (re_valid_label:Match(label)) then
          return send.checkout(errors.bad_label_arg)
        end
        if not (known_label(label)) then
          return send.checkout(errors.unknown_label)
        end
        local out, err = exec("checkout", label)
        return send.checkout(out)
      end
    end)(),
    listbranches = function(self)
      if not (in_repo()) then
        return send.checkout(errors.not_a_repo)
      end
      local branches = get_branches()
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
      if not (in_repo()) then
        return send.status(errors.not_a_repo)
      end
      local status_out = exec("status")
      return send.status(status_out)
    end,
    branch = (function()
      local re_valid_label = rgx.MustCompile("^[a-zA-Z-_/.]+$")
      return function(self, argv)
        local label = get_args(argv)
        if not (in_repo()) then
          return send.branch(errors.not_a_repo)
        end
        if not (re_valid_label:Match(label)) then
          return send.branch(errors.invalid_lbl)
        end
        local out = ''
        local fetch_out, _ = exec("fetch")
        out = out .. "> git fetch"
        out = out .. fetch_out
        do
          local rev = known_label(label)
          if rev then
            return send.branch(errors.invalid_arg .. ", please supply an unused label (" .. tostring(label) .. " is rev:" .. tostring(rev) .. ")")
          end
        end
        local branch_out, err = exec("branch", label)
        out = out .. "> git branch " .. tostring(label)
        out = out .. branch_out
        if not (err) then
          local chkout_out
          chkout_out, _ = exec("checkout", label)
          out = out .. "> git checkout " .. tostring(label)
          out = out .. chkout_out
        end
        return send.branch(out)
      end
    end)(),
    commit = (function()
      local base_msg = "\n\n"
      base_msg = base_msg .. "# Please enter the commit message for your changes. Lines starting\n"
      base_msg = base_msg .. "# with '#' will be ignored, and an empty message aborts the commit.\n#\n"
      local msg_line = rgx.MustCompile("^\\s*([^#])")
      return function(self, msg)
        if not (in_repo()) then
          return send.commit(errors.not_a_repo)
        end
        if msg then
          local commit_out = exec("commit", "-m", msg)
          return send.commit(commit_out)
        end
        debug("Processing git-status ...")
        local commit_msg_start = base_msg
        local status_out, _ = exec("status")
        each_line(status_out, function(line)
          commit_msg_start = commit_msg_start .. "# " .. tostring(line) .. "\n"
        end)
        debug("Populating commit pane ...")
        local header = "[new commit: save and quit to finalize]"
        make_commit_pane(commit_msg_start, header, function(file, _)
          debug("Beginning commit callback ...")
          local commit_msg = iou.ReadFile(file)
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
          debug("Committing: " .. tostring(final_commit))
          iou.WriteFile(file, final_commit, 0x1B0)
          if "" == chomp(final_commit) then
            return send.commit("Aborting, empty commit")
          end
          local commit_out = exec("commit", "-F", file)
          return send.commit(commit_out)
        end)
        debug("Awaiting commit completion within onQuit")
      end
    end)(),
    push = (function()
      local re_valid_label = rgx.MustCompile("^[a-zA-Z-_/.]+$")
      return function(self, branch)
        if not (in_repo()) then
          return send.push(errors.not_a_repo)
        end
        if branch then
          if not (re_valid_label:Match(branch)) then
            return send.push(errors.bad_label_arg)
          end
        else
          local _ = branch == "--all"
        end
        local push_out, _ = exec("push", branch)
        return send.push(push_out)
      end
    end)(),
    pull = function(self)
      local pull_out, _ = exec("pull")
      return send.pull(pull_out)
    end,
    log = function(self)
      if not (in_repo()) then
        return send.log("the current directory is not a repository")
      end
      local count = 0
      local out, err = exec("log")
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
        local finfo, _ = os.Stat(file)
        if not (finfo) then
          return send.add(errors.invalid_arg .. "(file " .. tostring(file) .. " doesn't exist)")
        end
        table.insert(files, file)
      end
      if not (#files > 0) then
        return send.add(errors.not_enough_args .. ", please supply a file")
      end
      return exec("add", unpack(files))
    end,
    rm = function(self, ...)
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
        local finfo, _ = os.Stat(file)
        if not (finfo) then
          return send.rm(errors.invalid_arg .. "(file " .. tostring(file) .. " doesn't exist)")
        end
        table.insert(files, file)
      end
      if not (#files > 0) then
        return send.rm(errors.not_enough_args .. ", please supply a file")
      end
      return exec("rm", unpack(files))
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
  registerCommand("git.raw", git.raw, cfg.NoComplete)
  registerCommand("git.init", git.init, cfg.NoComplete)
  registerCommand("git.pull", git.pull, cfg.NoComplete)
  registerCommand("git.push", git.push, cfg.NoComplete)
  registerCommand("git.list", git.listbranches, cfg.NoComplete)
  registerCommand("git.log", git.log, cfg.NoComplete)
  registerCommand("git.commit", git.commit, cfg.NoComplete)
  registerCommand("git.status", git.status, cfg.NoComplete)
  registerCommand("git.add", git.add, cfg.NoComplete)
  return registerCommand("git.rm", git.rm, cfg.NoComplete)
end
preinit = function()
  debug("Clearing stale commit files ...")
  local pfx = tostring(NAME) .. ".commit."
  local dir = path.Join(tostring(cfg.ConfigDir), "tmp")
  local files, err = iou.ReadDir(dir)
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
onQuit = function(self)
  local info = app.InfoBar()
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
    if commit.buffer == self then
      if commit.ready then
        debug("Commit " .. tostring(i) .. " is ready, fulfilling active commit ...")
        commit.callback(commit.file)
      else
        if self.Buf.modified then
          if info.HasYN and info.HasPrompt then
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
