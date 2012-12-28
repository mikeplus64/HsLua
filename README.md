HsLua
=====

Haskell Lua bindings, forked from Gracjan Polak's HsLua library.

# Changes
+ Use ByteString -- The Lua module no longer uses String internally *anywhere*.
+ Change the module name from Scripting.Lua to Lua.
+ Support using the system's Lua installation instead of the bundled one.
