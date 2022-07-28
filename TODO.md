## Thoughts regarding the statusline

Currently, this plugin is using `` to update the statusline directly, which
is quite cludgy.

```
update_git_line = (cmd) =>
```

SetStatusInfoFnLua exists, but using it plainly in the manner `update_git_line`
does would mean that the commands  used to form the gitline would be run on
every buffer update. That is less than ideal, and currently the line is only 
updated as needed.

Thinking on it, one potential option would be to utilize this function
to update the line whenever a change would need to occur for a particular
filepath (and then keep track of the active filepaths). The SetStatusInfoFnLua
could then supply a function that just reads the status line for the currently
active file. That would be a bit more onerous but would also be both a lot
cleaner and more configurable. 

Should that approach be taken, what should be done is to instead track
filepath state individually; which would allow the user to very easily
configure their status line with individual elements from this plugin.
