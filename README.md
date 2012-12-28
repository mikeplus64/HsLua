HsLua
=====

Haskell `Lua` bindings, forked from Gracjan Polak's HsLua library.

# Changes
+ Use `ByteString` -- The Lua module sees extremely few uses of `String` now, aside from calls to `error` and in the instance for `StackValue String.`
+ Change the module name from `Scripting.Lua` to `Lua`.
+ *Lots* of refactoring...
+ Support using the system's `Lua` installation instead of the bundled one, with the `system-lua` cabal flag.
