import Lake
open Lake DSL

package «advent-of-lean» {
  -- add package configuration options here
}

require std from git "https://github.com/leanprover/std4" @ "a93d4aab761b7962a6aab845b24837e192eaabc5"

lean_lib «AdventOfLean» {
  -- add library configuration options here
}

@[default_target]
lean_exe «advent-of-lean» {
  root := `Main
}
