import Lake
open Lake DSL

package «advent-of-lean» {
  -- add package configuration options here
}

lean_lib «AdventOfLean» {
  -- add library configuration options here
}

@[default_target]
lean_exe «advent-of-lean» {
  root := `Main
}
