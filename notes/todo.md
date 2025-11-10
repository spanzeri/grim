# Syntax that isn't supported yet

 - [ ] Initializer lists
   `x := AStruct{ 0 }`, `x := AStruct{ a_field = 0 }`, `x := [*]int{ 0, 1, 2 }`, `x := [3]int{ [0] = 1, [2] = 3 }`
 - [ ] `defer` statements
 - [ ] Named arguments in function calls `f :: fn(a, b: int, c := "", d: f32 = 0.0)` | `f(1, 2, d = 3.0)`
 - [ ] Variadic functions
 - [ ] `#bitflag` for enums, `#raw` for unions.
 - [ ] Inline functions

