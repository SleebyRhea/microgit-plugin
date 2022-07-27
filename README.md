# Git for Micro

## Commands
  - `git.init()`
    - Initialize repository in current directory
  - `git.pull()`
    - Pulls changes from remote into the local tree
  - `git.push(str)`
    - Push changes from local onto remove for the provided label (likely a branch)
  - `git.list()`
    - List the local and remote branches, and show currently checkedout revision
  - `git.log()`
    - Show the commit log
  - `git.commit(str)`
    - Commit changes to your local tree. If `str` is provided, it is used as the commit message. Otherwise, a buffer is opened for editting and the contents will be used instead
  - `git.status()`
    - Show current status of the repository
  - `git.add(str...)`
    - Stage one or more files. If `.` is provided, stage all files
  - `git.rm`
    - Remove one or more files. If `.` is provided, remove all files
