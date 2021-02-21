# ush
uwe shell

a minimalist shell

things I want to change:

- all `"$..."` ing, and `$(...)` ing should be done with `command A {command B}`, or some similar syntax
  - to get a variable, `PATH` for example, do `echo {get PATH}`
  - maybe something to redirect stderr, like `a {b}[c]` runs `b`, feeds stdout to `a`, and feeds stderr to `c`
  - and then `a [b]{c}` could run `b`, feed stderr to `a`, and feed stdout to `c`
- source every command that's run
- make helper commands that may or may not exist right now:
  - a command to set environment variables
  - a `cd` that directly sets PWD
  - a command to pipe
  - a command to write to file (hijack `tee`?)
  - a command that *doesn't* source what it runs, protecting outer environment
- strings are bloatware but maybe do them anyway
- redirect stdin into running program
- make it so if you try to call a program but its actually a directory it looks elsewhere in PATH
- make it so you can call programs directly by filepath
- make it so if a program errors it doesnt close the whole shell
