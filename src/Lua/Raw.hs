{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module only imports Lua functions from C.
-- It is unknown which of the imported function may trigger garbage collector.
-- GC may in turn call some finalizars that will require come back to
-- the Haskell world.
--
-- This means we must declare almost all functions as /safe/. This is slow, so
-- some functions known not to call GC are marked with faster /unsafe/ modifier.
--
module Lua.Raw where
import Lua.Types
import Foreign.C.Types
import Foreign

foreign import ccall "lua.h lua_close" c_lua_close :: LuaState -> IO ()
foreign import ccall "lua.h lua_newstate" c_lua_newstate :: FunPtr LuaAlloc -> Ptr () -> IO LuaState
foreign import ccall "lua.h lua_newthread" c_lua_newthread :: LuaState -> IO LuaState
foreign import ccall "lua.h lua_atpanic" c_lua_atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)

foreign import ccall "lua.h lua_gettop" c_lua_gettop :: LuaState -> IO CInt
foreign import ccall "lua.h lua_settop" c_lua_settop :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_pushvalue" c_lua_pushvalue :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_remove" c_lua_remove :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_insert" c_lua_insert :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_replace" c_lua_replace :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_checkstack" c_lua_checkstack :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_xmove" c_lua_xmove :: LuaState -> LuaState -> CInt -> IO ()

foreign import ccall "lua.h lua_isnumber" c_lua_isnumber :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_isstring" c_lua_isstring :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_iscfunction" c_lua_iscfunction :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_isuserdata" c_lua_isuserdata :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_type" c_lua_type :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_typename" c_lua_typename :: LuaState -> CInt -> IO (Ptr CChar)

foreign import ccall "lua.h lua_equal" c_lua_equal :: LuaState -> CInt -> CInt -> IO CInt
foreign import ccall "lua.h lua_rawequal" c_lua_rawequal :: LuaState -> CInt -> CInt -> IO CInt
foreign import ccall "lua.h lua_lessthan" c_lua_lessthan :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_tonumber" c_lua_tonumber :: LuaState -> CInt -> IO LuaNumber
foreign import ccall "lua.h lua_tointeger" c_lua_tointeger :: LuaState -> CInt -> IO LuaInteger
foreign import ccall "lua.h lua_toboolean" c_lua_toboolean :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_tolstring" c_lua_tolstring :: LuaState -> CInt -> Ptr CInt -> IO (Ptr CChar)
foreign import ccall "lua.h lua_objlen" c_lua_objlen :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_tocfunction" c_lua_tocfunction :: LuaState -> CInt -> IO (FunPtr LuaCFunction)
foreign import ccall "lua.h lua_touserdata" c_lua_touserdata :: LuaState -> CInt -> IO (Ptr a)
foreign import ccall "lua.h lua_tothread" c_lua_tothread :: LuaState -> CInt -> IO LuaState
foreign import ccall "lua.h lua_topointer" c_lua_topointer :: LuaState -> CInt -> IO (Ptr ())

foreign import ccall "lua.h lua_pushnil" c_lua_pushnil :: LuaState -> IO ()
foreign import ccall "lua.h lua_pushnumber" c_lua_pushnumber :: LuaState -> LuaNumber -> IO ()
foreign import ccall "lua.h lua_pushinteger" c_lua_pushinteger :: LuaState -> LuaInteger -> IO ()
foreign import ccall "lua.h lua_pushlstring" c_lua_pushlstring :: LuaState -> Ptr CChar -> CInt -> IO ()
foreign import ccall "lua.h lua_pushstring" c_lua_pushstring :: LuaState -> Ptr CChar -> IO ()

{-
LUA_API const char *(lua_pushvfstring) (lua_State *L, const char *fmt,
                                                      va_list argp);

LUA_API const char *(lua_pushfstring) (lua_State *L, const char *fmt, ...);
-}

foreign import ccall "lua.h lua_pushcclosure" c_lua_pushcclosure :: LuaState -> FunPtr LuaCFunction -> CInt -> IO ()
foreign import ccall "lua.h lua_pushboolean" c_lua_pushboolean :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_pushlightuserdata" c_lua_pushlightuserdata :: LuaState -> Ptr a -> IO ()
foreign import ccall "lua.h lua_pushthread" c_lua_pushthread :: LuaState -> IO CInt

foreign import ccall "lua.h lua_gettable" c_lua_gettable :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_getfield" c_lua_getfield :: LuaState -> CInt -> Ptr CChar -> IO ()
foreign import ccall "lua.h lua_rawget" c_lua_rawget :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_rawgeti" c_lua_rawgeti :: LuaState -> CInt -> CInt -> IO ()
foreign import ccall "lua.h lua_createtable" c_lua_createtable :: LuaState -> CInt -> CInt -> IO ()
foreign import ccall "lua.h lua_newuserdata" c_lua_newuserdata :: LuaState -> CInt -> IO (Ptr ())
foreign import ccall "lua.h lua_getmetatable" c_lua_getmetatable :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_getfenv" c_lua_getfenv :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_getupvalue" c_lua_getupvalue :: LuaState -> CInt -> CInt -> IO (Ptr CChar)
foreign import ccall "lua.h lua_setupvalue" c_lua_setupvalue :: LuaState -> CInt -> CInt -> IO (Ptr CChar)

foreign import ccall "lua.h lua_settable" c_lua_settable :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_setfield" c_lua_setfield :: LuaState -> CInt -> Ptr CChar -> IO ()
foreign import ccall "lua.h lua_rawset" c_lua_rawset :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_rawseti" c_lua_rawseti :: LuaState -> CInt -> CInt -> IO ()
foreign import ccall "lua.h lua_setmetatable" c_lua_setmetatable :: LuaState -> CInt -> IO ()
foreign import ccall "lua.h lua_setfenv" c_lua_setfenv :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_call" c_lua_call :: LuaState -> CInt -> CInt -> IO ()
foreign import ccall "lua.h lua_pcall" c_lua_pcall :: LuaState -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "lua.h lua_cpcall" c_lua_cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO CInt

foreign import ccall "lua.h lua_load" c_lua_load :: LuaState -> FunPtr LuaReader -> Ptr () -> Ptr CChar -> IO CInt

foreign import ccall "lua.h lua_dump" c_lua_dump :: LuaState -> FunPtr LuaWriter -> Ptr () -> IO ()

foreign import ccall "lua.h lua_yield" c_lua_yield :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_resume" c_lua_resume :: LuaState -> CInt -> IO CInt
foreign import ccall "lua.h lua_status" c_lua_status :: LuaState -> IO CInt

foreign import ccall "lua.h lua_gc" c_lua_gc :: LuaState -> CInt -> CInt -> IO CInt

foreign import ccall "lua.h lua_error" c_lua_error :: LuaState -> IO CInt

foreign import ccall "lua.h lua_next" c_lua_next :: LuaState -> CInt -> IO CInt

foreign import ccall "lua.h lua_concat" c_lua_concat :: LuaState -> CInt -> IO ()

foreign import ccall "lualib.h luaL_openlibs" c_luaL_openlibs :: LuaState -> IO ()
foreign import ccall "lauxlib.h luaL_newstate" c_luaL_newstate :: IO LuaState
foreign import ccall "lauxlib.h luaL_newmetatable" c_luaL_newmetatable :: LuaState -> Ptr CChar -> IO CInt
foreign import ccall "lauxlib.h luaL_argerror" c_luaL_argerror :: LuaState -> CInt -> Ptr CChar -> IO CInt

foreign import ccall "ntrljmp.h lua_neutralize_longjmp" c_lua_neutralize_longjmp :: LuaState -> IO CInt
foreign import ccall "ntrljmp.h &lua_neutralize_longjmp" c_lua_neutralize_longjmp_addr :: FunPtr (LuaState -> IO CInt) 

{-
The following seem to be really bad idea, as calls from C
back to Haskell land are costly.

Use standard Lua malloc based allocator.

foreign export ccall "hslua_alloc" hslua_alloc :: Ptr () -> Ptr () -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "&hslua_alloc" hslua_alloc_addr :: FunPtr LuaAlloc

hslua_alloc :: Ptr () -> Ptr () -> CInt -> CInt -> IO (Ptr ())
hslua_alloc ud ptr osize nsize = reallocBytes ptr (fromIntegral nsize)

static void *l_alloc (void *ud, void *ptr, size_t osize,
                                                size_t nsize) {
       (void)ud;  (void)osize;  /* not used */
       if (nsize == 0) {
         free(ptr);
         return NULL;
       }
       else
         return realloc(ptr, nsize);
     }
-}

