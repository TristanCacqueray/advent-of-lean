import Lake
open Lake DSL

package «advent-of-lean» {
  -- add package configuration options here
}

require std from git "https://github.com/leanprover/std4" @ "v4.3.0"
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "v4.3.0"

lean_lib «AdventOfLean» {
  -- add library configuration options here
}

@[default_target]
lean_exe «advent-of-lean» {
  root := `Main
}
