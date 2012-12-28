{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind -fno-warn-unused-binds #-}
-- |
-- Module      : Lua
-- Copyright   : (c) Gracjan Polak 2007
--               (c) Mike Ledger   2012
--
-- License     : BSD3-style
--
-- Maintainer  : gracjanpolak@gmail.com
-- Stability   : alpha
-- Portability : portable, ffi
--
-- A Haskell wrapper library for a scripting language Lua.
-- See @http:\/\/www.lua.org\/@ for more details.
--
-- This module is intended to be imported @qualified@, eg.
--
-- > import qualified Scripting.Lua as Lua
--
-- This way we use Haskell module hierarchy to make Lua names shorter.
-- Haskell functions are named after Lua functions, but the @lua_@ or
-- @luaL_@ prefix.
--
-- Lua types are mapped to Haskell types as in the following table:
--
-- > int (stack index)        Int
-- > lua_Integer              LuaInteger
-- > lua_Number               LuaNumber
-- > int (bool result)        Bool
--
-- Note that in order to have Unicode support, you should use 'Data.Text.Text'
-- although you can also use 'String'. Internally everything uses 'Data.ByteString.ByteString',
-- and only instances for 'StackValue' are provided for 'String' and 'Data.Text.Text'.
--
-- > const char * (string)    ByteString
-- > void *                   Ptr ()
-- > lua_State *              LuaState
--
-- Most functions are one-to-one mappings.
-- Rare special cases are clearly marked in this document.
--
-- Minmal sample embedding:
--
-- > import qualified Scripting.Lua as Lua
--
-- > main = do
-- >     l <- Lua.newstate
-- >     Lua.openlibs l
-- >     Lua.callproc l "print" "Hello from Lua"
-- >     Lua.close l

module Lua (
    -- * Basic Lua types
    LuaState,
    LuaCFunction,
    LuaInteger,
    LuaNumber,
    LuaImport(..),

    -- * Constants and enumerations
    GCCONTROL(..),
    LTYPE(..),
    multret,
    registryindex,
    environindex,
    globalsindex,

    -- * lua_* functions
    atpanic,
    call,
    checkstack,
    close,
    concat,
    cpcall,
    createtable,
    dump,
    equal,
    --error,    -- cannot import this one, as this uses setjmp/longjmp
    gc,
    --getallocf,
    getfenv,
    getfield,
    getglobal,
    --gethook,
    --gethookcount,
    --gethookmask,
    --getinfo,
    --getlocal,
    getmetatable,
    --getstack,
    gettable,
    gettop,
    getupvalue,
    insert,
    isboolean,
    iscfunction,
    isfunction,
    islightuserdata,
    isnil,
    isnumber,
    isstring,
    istable,
    isthread,
    isuserdata,
    lessthan,
    --load,
    newstate,
    newtable,
    newthread,
    newuserdata,
    next,
    objlen,
    pcall,
    pop,
    pushboolean,
    pushcclosure,
    pushcfunction,
    --pushfstring,
    pushinteger,
    pushlightuserdata,
    --pushlstring,
    pushnil,
    pushnumber,
    pushstring,
    pushthread,
    pushvalue,
    --pushvfstring,
    rawequal,
    rawget,
    rawgeti,
    rawset,
    rawseti,
    register,
    remove,
    replace,
    resume,
    --setallocf,
    setfenv,
    setfield,
    setglobal,
    --sethook,
    --setlocal,
    setmetatable,
    settable,
    settop,
    setupvalue,
    status,
    toboolean,
    tocfunction,
    tointeger,
    --tolstring,
    tonumber,
    topointer,
    tostring,
    tothread,
    touserdata,
    ltype,
    typename,
    upvalueindex,
    xmove,
    yield,

    -- * luaL_* functions
    openlibs,
    loadfile,
    loadstring,
    newmetatable,
    argerror,

    -- * Haskell extensions
    StackValue(..),
    callproc,
    callfunc,
    getglobal2,
    newcfunction,
    freecfunction,
    luaimport,
    pushhsfunction,
    registerhsfunction) where

import Prelude hiding (concat, catch)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Control.Monad
import Foreign.Marshal.Alloc
import Data.IORef
import qualified Foreign.Storable as F
import Data.Maybe
import Control.Exception
import Data.Monoid

import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString    as B
import Data.Word (Word8)
import Data.Char (isLatin1)

import Lua.Raw
import Lua.Types

-- | See @LUA_MULTRET@ in Lua Reference Manual.
multret :: Int
multret = -1

-- Convenience functions for converting Strings to ByteStrings and back
--
w8 :: Char -> Word8
w8 = fromIntegral . fromEnum

bstr :: String -> B.ByteString
bstr = B.pack . map w8

strb :: B.ByteString -> String
strb = map (toEnum . fromIntegral) . B.unpack


-- | See @lua_settop@ in Lua Reference Manual.
settop :: LuaState -> Int -> IO ()
settop l n = c_lua_settop l (fromIntegral n)

-- | See @lua_createtable@ in Lua Reference Manual.
createtable :: LuaState -> Int -> Int -> IO ()
createtable l s z = c_lua_createtable l (fromIntegral s) (fromIntegral z)

-- | See @lua_objlen@ in Lua Reference Manual.
objlen :: LuaState -> Int -> IO Int
objlen l n = liftM fromIntegral (c_lua_objlen l (fromIntegral n))

-- | See @lua_pop@ in Lua Reference Manual.
pop :: LuaState -> Int -> IO ()
pop l n = settop l (-n-1)


-- | See @lua_newtable@ in Lua Reference Manual.
newtable :: LuaState -> IO ()
newtable l = createtable l 0 0

-- | See @lua_pushcclosure@ in Lua Reference Manual.
pushcclosure :: LuaState -> FunPtr LuaCFunction -> Int -> IO ()
pushcclosure l f n = c_lua_pushcclosure l f (fromIntegral n)

-- | See @lua_pushcfunction@ in Lua Reference Manual.
pushcfunction :: LuaState -> FunPtr LuaCFunction -> IO ()
pushcfunction l f = pushcclosure l f 0


-- | See @lua_strlen@ in Lua Reference Manual.
strlen :: LuaState -> Int -> IO Int
strlen l i =  objlen l i

-- | See @lua_type@ in Lua Reference Manual.
ltype :: LuaState -> Int -> IO LTYPE
ltype l n = liftM (toEnum . fromIntegral) (c_lua_type l (fromIntegral n))

-- | See @lua_isfunction@ in Lua Reference Manual.
isfunction :: LuaState -> Int -> IO Bool
isfunction l n = liftM (==TFUNCTION) (ltype l n)

-- | See @lua_istable@ in Lua Reference Manual.
istable :: LuaState -> Int -> IO Bool
istable l n = liftM (==TTABLE) (ltype l n)

-- | See @lua_islightuserdata@ in Lua Reference Manual.
islightuserdata :: LuaState -> Int -> IO Bool
islightuserdata l n = liftM (==TLIGHTUSERDATA) (ltype l n)

-- | See @lua_isnil@ in Lua Reference Manual.
isnil :: LuaState -> Int -> IO Bool
isnil l n = liftM (==TNIL) (ltype l n)

-- | See @lua_isboolean@ in Lua Reference Manual.
isboolean :: LuaState -> Int -> IO Bool
isboolean l n = liftM (==TBOOLEAN) (ltype l n)

-- | See @lua_isthread@ in Lua Reference Manual.
isthread :: LuaState -> Int -> IO Bool
isthread l n = liftM (==TTHREAD) (ltype l n)

-- | See @lua_none@ in Lua Reference Manual.
isnone :: LuaState -> Int -> IO Bool
isnone l n = liftM (==TNONE) (ltype l n)

-- | See @lua_noneornil@ in Lua Reference Manual.
isnoneornil :: LuaState -> Int -> IO Bool
isnoneornil l n = liftM (<=TNIL) (ltype l n)

-- | See @LUA_REGISTRYINDEX@ in Lua Reference Manual.
registryindex :: Int
registryindex = (-10000)

-- | See @LUA_ENVIRONINDEX@ in Lua Reference Manual.
environindex :: Int
environindex = (-10001)

-- | See @LUA_GLOBALSINDEX@ in Lua Reference Manual.
globalsindex :: Int
globalsindex = (-10002)

-- | See @lua_upvalueindex@ in Lua Reference Manual.
upvalueindex :: Int -> Int
upvalueindex i = (globalsindex-(i))

-- | See @lua_atpanic@ in Lua Reference Manual.
atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)
atpanic = c_lua_atpanic

-- | See @lua_tostring@ in Lua Reference Manual.
tostring :: LuaState -> Int -> IO B.ByteString
tostring l n = c_lua_tolstring l (fromIntegral n) nullPtr >>= B.packCString

-- | See @lua_tothread@ in Lua Reference Manual.
tothread :: LuaState -> Int -> IO LuaState
tothread l n = c_lua_tothread l (fromIntegral n)

-- | See @lua_touserdata@ in Lua Reference Manual.
touserdata :: LuaState -> Int -> IO (Ptr a)
touserdata l n = c_lua_touserdata l (fromIntegral n)

-- | See @lua_typename@ in Lua Reference Manual.
typename :: LuaState -> LTYPE -> IO B.ByteString
typename l n = c_lua_typename l (fromIntegral (fromEnum n)) >>= B.packCString

-- | See @lua_xmove@ in Lua Reference Manual.
xmove :: LuaState -> LuaState -> Int -> IO ()
xmove l1 l2 n = c_lua_xmove l1 l2 (fromIntegral n)

-- | See @lua_yield@ in Lua Reference Manual.
yield :: LuaState -> Int -> IO Int
yield l n = liftM fromIntegral (c_lua_yield l (fromIntegral n))

-- | See @lua_checkstack@ in Lua Reference Manual.
checkstack :: LuaState -> Int -> IO Bool
checkstack l n = liftM (/=0) (c_lua_checkstack l (fromIntegral n))

-- | See @lua_newstate@ and @luaL_newstate@ in Lua Reference Manual.
newstate :: IO LuaState
newstate = c_luaL_newstate

-- | See @lua_close@ in Lua Reference Manual.
close :: LuaState -> IO ()
close = c_lua_close

-- | See @lua_concat@ in Lua Reference Manual.
concat :: LuaState -> Int -> IO ()
concat l n = c_lua_concat l (fromIntegral n)



-- | See @lua_call@ and @lua_call@ in Lua Reference Manual. This is 
-- a wrapper over @lua_pcall@, as @lua_call@ is unsafe in controlled environment
-- like Haskell VM.
call :: LuaState -> Int -> Int -> IO Int
call l a b = liftM fromIntegral (c_lua_pcall l (fromIntegral a) (fromIntegral b) 0)

-- | See @lua_pcall@ in Lua Reference Manual.
pcall :: LuaState -> Int -> Int -> Int -> IO Int
pcall l a b c = liftM fromIntegral (c_lua_pcall l (fromIntegral a) (fromIntegral b) (fromIntegral c))

-- | See @lua_cpcall@ in Lua Reference Manual.
cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO Int
cpcall l a c = liftM fromIntegral (c_lua_cpcall l a c)

-- | See @lua_getfield@ in Lua Reference Manual.
getfield :: LuaState -> Int -> B.ByteString -> IO ()
getfield l i n = B.useAsCString n $ \n -> c_lua_getfield l (fromIntegral i) n

-- | See @lua_setfield@ in Lua Reference Manual.
setfield :: LuaState -> Int -> B.ByteString -> IO ()
setfield l i n = B.useAsCString n $ \n -> c_lua_setfield l (fromIntegral i) n

-- | See @lua_getglobal@ in Lua Reference Manual.
getglobal :: LuaState -> B.ByteString -> IO ()
getglobal l n = getfield l globalsindex n

-- | See @lua_setglobal@ in Lua Reference Manual.
setglobal :: LuaState -> B.ByteString -> IO ()
setglobal l n = setfield l globalsindex n

-- | See @luaL_openlibs@ in Lua Reference Manual.
openlibs :: LuaState -> IO ()
openlibs = c_luaL_openlibs

foreign import ccall "wrapper" mkStringWriter :: LuaWriter -> IO (FunPtr LuaWriter)

dump :: LuaState -> IO B.ByteString
dump l = do
    r <- newIORef ""
    let wr :: LuaWriter
        wr _l p s _d = do
               k <- B.packCStringLen (p,fromIntegral s)
               modifyIORef r (<> k)
               return 0
    writer <- mkStringWriter wr
    c_lua_dump l writer nullPtr
    freeHaskellFunPtr writer
    readIORef r

-- | See @lua_equal@ in Lua Reference Manual.
equal :: LuaState -> Int -> Int -> IO Bool
equal l i j = liftM (/=0) (c_lua_equal l (fromIntegral i) (fromIntegral j))

-- | See @lua_error@ in Lua Reference Manual.
--error :: LuaState -> IO Int
--error l = liftM fromIntegral (c_lua_error l)

-- | See @lua_gc@ in Lua Reference Manual.
gc :: LuaState -> GCCONTROL -> Int -> IO Int
gc l i j= liftM fromIntegral (c_lua_gc l (fromIntegral (fromEnum i)) (fromIntegral j))

-- | See @lua_getfenv@ in Lua Reference Manual.
getfenv :: LuaState -> Int -> IO ()
getfenv l n = c_lua_getfenv l (fromIntegral n)

-- | See @lua_getmetatable@ in Lua Reference Manual.
getmetatable :: LuaState -> Int -> IO Bool
getmetatable l n = liftM (/=0) (c_lua_getmetatable l (fromIntegral n))

-- | See @lua_gettable@ in Lua Reference Manual.
gettable :: LuaState -> Int -> IO ()
gettable l n = c_lua_gettable l (fromIntegral n)

-- | See @lua_gettop@ in Lua Reference Manual.
gettop :: LuaState -> IO Int
gettop l = liftM fromIntegral (c_lua_gettop l)

-- | See @lua_getupvalue@ in Lua Reference Manual.
getupvalue :: LuaState -> Int -> Int -> IO B.ByteString
getupvalue l funcindex n = c_lua_getupvalue l (fromIntegral funcindex) (fromIntegral n) >>= B.packCString

-- | See @lua_insert@ in Lua Reference Manual.
insert :: LuaState -> Int -> IO ()
insert l n  = c_lua_insert l (fromIntegral n)

-- | See @lua_iscfunction@ in Lua Reference Manual.
iscfunction :: LuaState -> Int -> IO Bool
iscfunction l n = liftM (/=0) (c_lua_iscfunction l (fromIntegral n))

-- | See @lua_isnumber@ in Lua Reference Manual.
isnumber :: LuaState -> Int -> IO Bool
isnumber l n = liftM (/=0) (c_lua_isnumber l (fromIntegral n))

-- | See @lua_isstring@ in Lua Reference Manual.
isstring :: LuaState -> Int -> IO Bool
isstring l n = liftM (/=0) (c_lua_isstring l (fromIntegral n))

-- | See @lua_isuserdata@ in Lua Reference Manual.
isuserdata :: LuaState -> Int -> IO Bool
isuserdata l n = liftM (/=0) (c_lua_isuserdata l (fromIntegral n))

-- | See @lua_lessthan@ in Lua Reference Manual.
lessthan :: LuaState -> Int -> Int -> IO Bool
lessthan l i j = liftM (/=0) (c_lua_lessthan l (fromIntegral i) (fromIntegral j))


-- | See @luaL_loadfile@ in Lua Reference Manual.
loadfile :: LuaState -> FilePath -> IO Int
loadfile l f = B.readFile f >>= \c -> loadstring l c (bstr f)

foreign import ccall "wrapper" mkStringReader :: LuaReader -> IO (FunPtr LuaReader)

-- | See @luaL_loadstring@ in Lua Reference Manual.
loadstring :: LuaState -> B.ByteString -> B.ByteString -> IO Int
loadstring l script' cn = B.useAsCStringLen script' $ \ (script, len) -> do
    w <- newIORef nullPtr
    let rd :: LuaReader
        rd _l _d ps = do
               k <- readIORef w
               if k == nullPtr
                   then do
                       writeIORef w script
                       F.poke ps (fromIntegral len)
                       return script
                   else do
                       return nullPtr
    writer <- mkStringReader rd
    res    <- B.useAsCString cn $ \cn -> c_lua_load l writer nullPtr cn
    freeHaskellFunPtr writer
    k <- readIORef w
    free k
    return (fromIntegral res)

-- | See @lua_newthread@ in Lua Reference Manual.
newthread :: LuaState -> IO LuaState
newthread l = c_lua_newthread l

-- | See @lua_newuserdata@ in Lua Reference Manual.
newuserdata :: LuaState -> Int -> IO (Ptr ())
newuserdata l s = c_lua_newuserdata l (fromIntegral s)

-- | See @lua_next@ in Lua Reference Manual.
next :: LuaState -> Int -> IO Bool
next l i = liftM (/=0) (c_lua_next l (fromIntegral i))

-- | See @lua_pushboolean@ in Lua Reference Manual.
pushboolean :: LuaState -> Bool -> IO ()
pushboolean l v = c_lua_pushboolean l (fromIntegral (fromEnum v))

-- | See @lua_pushinteger@ in Lua Reference Manual.
pushinteger :: LuaState -> LuaInteger -> IO ()
pushinteger = c_lua_pushinteger

-- | See @lua_pushlightuserdata@ in Lua Reference Manual.
pushlightuserdata :: LuaState -> Ptr a -> IO ()
pushlightuserdata = c_lua_pushlightuserdata

-- | See @lua_pushnil@ in Lua Reference Manual.
pushnil :: LuaState -> IO ()
pushnil = c_lua_pushnil

-- | See @lua_pushnumber@ in Lua Reference Manual.
pushnumber :: LuaState -> LuaNumber -> IO ()
pushnumber = c_lua_pushnumber

-- | See @lua_pushstring@ in Lua Reference Manual.
pushstring :: LuaState -> B.ByteString -> IO ()
pushstring l s = B.useAsCStringLen s $ \(s,z) -> c_lua_pushlstring l s (fromIntegral z)

-- | See @lua_pushthread@ in Lua Reference Manual.
pushthread :: LuaState -> IO Bool
pushthread l = liftM (/=0) (c_lua_pushthread l)

-- | See @lua_pushvalue@ in Lua Reference Manual.
pushvalue :: LuaState -> Int -> IO ()
pushvalue l n = c_lua_pushvalue l (fromIntegral n)

-- | See @lua_rawequal@ in Lua Reference Manual.
rawequal :: LuaState -> Int -> Int -> IO Bool
rawequal l n m = liftM (/=0) (c_lua_rawequal l (fromIntegral n) (fromIntegral m))

-- | See @lua_rawget@ in Lua Reference Manual.
rawget :: LuaState -> Int -> IO ()
rawget l n = c_lua_rawget l (fromIntegral n)

-- | See @lua_rawgeti@ in Lua Reference Manual.
rawgeti :: LuaState -> Int -> Int -> IO ()
rawgeti l k m = c_lua_rawgeti l (fromIntegral k) (fromIntegral m)

-- | See @lua_rawset@ in Lua Reference Manual.
rawset :: LuaState -> Int -> IO ()
rawset l n = c_lua_rawset l (fromIntegral n)

-- | See @lua_rawseti@ in Lua Reference Manual.
rawseti :: LuaState -> Int -> Int -> IO ()
rawseti l k m = c_lua_rawseti l (fromIntegral k) (fromIntegral m)

-- | See @lua_remove@ in Lua Reference Manual.
remove :: LuaState -> Int -> IO ()
remove l n = c_lua_remove l (fromIntegral n)

-- | See @lua_replace@ in Lua Reference Manual.
replace :: LuaState -> Int -> IO ()
replace l n = c_lua_replace l (fromIntegral n)

-- | See @lua_resume@ in Lua Reference Manual.
resume :: LuaState -> Int -> IO Int
resume l n = liftM fromIntegral (c_lua_resume l (fromIntegral n))

-- | See @lua_setfenv@ in Lua Reference Manual.
setfenv :: LuaState -> Int -> IO Int
setfenv l n = liftM fromIntegral (c_lua_setfenv l (fromIntegral n))

-- | See @lua_setmetatable@ in Lua Reference Manual.
setmetatable :: LuaState -> Int -> IO ()
setmetatable l n = c_lua_setmetatable l (fromIntegral n)

-- | See @lua_settable@ in Lua Reference Manual.
settable :: LuaState -> Int -> IO ()
settable l index = c_lua_settable l (fromIntegral index)


-- | See @lua_setupvalue@ in Lua Reference Manual.
setupvalue :: LuaState -> Int -> Int -> IO B.ByteString
setupvalue l funcindex n = c_lua_setupvalue l (fromIntegral funcindex) (fromIntegral n) >>= B.packCString

-- | See @lua_status@ in Lua Reference Manual.
status :: LuaState -> IO Int
status l = liftM fromIntegral (c_lua_status l)

-- | See @lua_toboolean@ in Lua Reference Manual.
toboolean :: LuaState -> Int -> IO Bool
toboolean l n = liftM (/=0) (c_lua_toboolean l (fromIntegral n))

-- | See @lua_tocfunction@ in Lua Reference Manual.
tocfunction :: LuaState -> Int -> IO (FunPtr LuaCFunction)
tocfunction l n = c_lua_tocfunction l (fromIntegral n)

-- | See @lua_tointeger@ in Lua Reference Manual.
tointeger :: LuaState -> Int -> IO LuaInteger
tointeger l n = c_lua_tointeger l (fromIntegral n)

-- | See @lua_tonumber@ in Lua Reference Manual.
tonumber :: LuaState -> Int -> IO CDouble
tonumber l n = c_lua_tonumber l (fromIntegral n)

-- | See @lua_topointer@ in Lua Reference Manual.
topointer :: LuaState -> Int -> IO (Ptr ())
topointer l n = c_lua_topointer l (fromIntegral n)

-- | See @lua_register@ in Lua Reference Manual.
register :: LuaState -> B.ByteString -> FunPtr LuaCFunction -> IO ()
register l n f = do
    pushcclosure l f 0
    setglobal l n

-- | See @luaL_newmetatable@ in Lua Reference Manual.
newmetatable :: LuaState -> B.ByteString -> IO Int
newmetatable l s = B.useAsCString s $ \s -> liftM fromIntegral (c_luaL_newmetatable l s)

-- | See @luaL_argerror@ in Lua Reference Manual. Contrary to the 
-- manual, Haskell function does return with value less than zero.
argerror :: LuaState -> Int -> B.ByteString -> IO CInt
argerror l n msg = B.useAsCString msg $ \msg -> do
    let doit l = c_luaL_argerror l (fromIntegral n) msg
    f <- mkWrapper doit
    c_lua_cpcall l f nullPtr
    freeHaskellFunPtr f
    -- here we should have error message string on top of the stack
    return (-1)

maybepeek :: l -> n -> (l -> n -> IO Bool) -> (l -> n -> IO r) -> IO (Maybe r)
maybepeek l n test peek = do
    v <- test l n
    if v
        then liftM Just (peek l n)
        else return Nothing

-- | A value that can be pushed and poped from the Lua stack.
-- All instances are natural, except following:
--
--  * @LuaState@ push ignores its argument, pushes current state
--
--  * @()@ push ignores its argument, just pushes nil
--
--  * @Ptr ()@ pushes light user data, peek checks for lightuserdata or userdata
class StackValue a where
    -- | Pushes a value onto Lua stack, casting it into meaningfully nearest Lua type.
    push      :: LuaState -> a -> IO ()
    -- | Check if at index @n@ there is a convertible Lua value and if so return it
    -- wrapped in @Just@. Return @Nothing@ otherwise.
    peek      :: LuaState -> Int -> IO (Maybe a)
    -- | Lua type id code of the vaule expected. Parameter is unused.
    valuetype :: a -> LTYPE

instance StackValue LuaInteger where
    push l x = pushinteger l x
    peek l n = maybepeek l n isnumber tointeger
    valuetype _ = TNUMBER

instance StackValue LuaNumber where
    push l x = pushnumber l x
    peek l n = maybepeek l n isnumber tonumber
    valuetype _ = TNUMBER

instance StackValue Int where
    push l x = pushinteger l (fromIntegral x)
    peek l n = maybepeek l n isnumber (\l n -> liftM fromIntegral (tointeger l n))
    valuetype _ = TNUMBER

instance StackValue Double where
    push l x = pushnumber l (realToFrac x)
    peek l n = maybepeek l n isnumber (\l n -> liftM realToFrac (tonumber l n))
    valuetype _ = TNUMBER

instance StackValue B.ByteString where
    push l x = pushstring l x
    peek l n = maybepeek l n isstring tostring
    valuetype _ = TSTRING

instance StackValue T.Text where
    push l x = push l (E.encodeUtf8 x)
    peek l n = fmap E.decodeUtf8 `fmap` peek l n
    valuetype _ = TSTRING

instance StackValue String where
    push l x | all isLatin1 x = push l (bstr x)
             | otherwise      = push l (T.pack x)
    peek l n    = fmap T.unpack `fmap` peek l n
    valuetype _ = TSTRING

instance StackValue Bool where
    push l x = pushboolean l x
    peek l n = maybepeek l n isboolean toboolean
    valuetype _ = TBOOLEAN

instance StackValue (FunPtr LuaCFunction) where
    push l x = pushcfunction l x
    peek l n = maybepeek l n iscfunction tocfunction
    valuetype _ = TFUNCTION

-- watch out for the asymetry here
instance StackValue (Ptr a) where
    push l x = pushlightuserdata l x
    peek l n = maybepeek l n isuserdata touserdata
    valuetype _ = TUSERDATA

-- watch out for push here
instance StackValue LuaState where
    push l _ = pushthread l >> return ()
    peek l n = maybepeek l n isthread tothread
    valuetype _ = TTHREAD

instance StackValue () where
    push l _ = pushnil l
    peek l n = maybepeek l n isnil (\_l _n -> return ())
    valuetype _ = TNIL

-- | Like @getglobal@, but knows about packages. e. g.
--
-- > getglobal l "math.sin"
--
-- returns correct result
getglobal2 :: LuaState -> B.ByteString -> IO ()
getglobal2 l n = do
    getglobal l x
    mapM_ dotable xs
  where 
    (x:xs)    = splitdot n
    splitdot  = filter (not . B.null) . B.split (w8 '.')
    dotable x = getfield l (-1) x >> gettop l >>= \n -> remove l (n-1)


typenameindex :: LuaState -> Int -> IO B.ByteString
typenameindex l n = ltype l n >>= typename l

class LuaImport a where
    luaimport' :: Int -> a -> LuaCFunction
    luaimportargerror :: Int -> B.ByteString -> a -> LuaCFunction

instance (StackValue a) => LuaImport (IO a) where
    luaimportargerror n msg _x l = argerror l n msg
    luaimport' _narg x l = x >>= push l >> return 1

instance (StackValue a,LuaImport b) => LuaImport (a -> b) where
    luaimportargerror n msg x l = luaimportargerror n msg (x undefined) l
    {-
     - FIXME: Cannot catch this exception here, because we are called from C,
     - and error propagation, stack unwinding, etc does not work.
     - Cannot call lua_error, because it uses longjmp and would skip two layers of abstraction.
     -}
    luaimport' narg x l = do
        arg <- peek l narg
        case arg of
            Just v -> luaimport' (narg+1) (x v) l
            Nothing -> do
                t   <- ltype l narg
                exp <- typename l (valuetype (fromJust arg))
                got <- typename l t
                luaimportargerror narg (exp <> " expected, got " <> got) (x undefined) l

foreign import ccall "wrapper" mkWrapper :: LuaCFunction -> IO (FunPtr LuaCFunction)

-- | Create new foreign Lua function. Function created can be called
-- by Lua engine. Remeber to free the pointer with @freecfunction@.
newcfunction :: LuaImport a => a -> IO (FunPtr LuaCFunction)
newcfunction = mkWrapper . luaimport

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
--
--   * all arguments are instances of StackValue
--   * return type is IO t, where t is an instance of StackValue
--
-- Any Haskell exception will be converted to a string and returned
-- as Lua error.
luaimport :: LuaImport a => a -> LuaCFunction
luaimport a l = luaimport' 1 a l `catch` (\e@SomeException{} -> push l (bstr (show e)) >> return (-1))

-- | Free function pointer created with @newcfunction@.
freecfunction :: FunPtr LuaCFunction -> IO ()
freecfunction = freeHaskellFunPtr

class LuaCallProc a where
    callproc' :: LuaState -> B.ByteString -> IO () -> Int -> a

-- | Call a Lua procedure. Use as:
--
-- > callproc l "proc" "abc" (1::Int) (5.0::Double)
--
callproc :: (LuaCallProc a) => LuaState -> B.ByteString -> a
callproc l f = callproc' l f (return ()) 0

class LuaCallFunc a where
    callfunc' :: LuaState -> B.ByteString -> IO () -> Int -> a

-- | Call a Lua function. Use as:
--
-- > Just v <- callfunc l "proc" "abc" (1::Int) (5.0::Double)
callfunc :: (LuaCallFunc a) => LuaState -> B.ByteString -> a
callfunc l f = callfunc' l f (return ()) 0

instance LuaCallProc (IO t) where
    callproc' l f a k = do
        getglobal2 l f
        a
        z <- pcall l k 0 0
        if z/=0
            then do
                Just msg <- peek l (-1)
                pop l 1
                error (strb msg)
            else do
                return undefined

instance (StackValue t) => LuaCallFunc (IO t) where
    callfunc' l f a k = do
        getglobal2 l f
        a
        z <- pcall l k 1 0
        if z/=0
            then do
                Just msg <- peek l (-1)
                pop l 1
                error (strb msg)
            else do
                r <- peek l (-1)
                pop l 1
                case r of
                    Just x -> return x
                    Nothing -> do
                        exp <- typename l (valuetype (fromJust r))
                        t <- ltype l (-1)
                        got <- typename l t
                        error $ strb $ "Incorrect result type (" <> exp <> " expected, got " <> got <> ")"

instance (StackValue t,LuaCallProc b) => LuaCallProc (t -> b) where
    callproc' l f a k x = callproc' l f (a >> push l x) (k+1)

instance (StackValue t,LuaCallFunc b) => LuaCallFunc (t -> b) where
    callfunc' l f a k x = callfunc' l f (a >> push l x) (k+1)

foreign export ccall hsmethod__gc :: LuaState -> IO CInt
foreign import ccall "&hsmethod__gc" hsmethod__gc_addr :: FunPtr LuaCFunction

foreign export ccall hsmethod__call :: LuaState -> IO CInt
foreign import ccall "&hsmethod__call" hsmethod__call_addr :: FunPtr LuaCFunction

hsmethod__gc :: LuaState -> IO CInt
hsmethod__gc l = do
    Just ptr <- peek l (-1)
    stableptr <- F.peek (castPtr ptr)
    freeStablePtr stableptr
    return 0

hsmethod__call :: LuaState -> IO CInt
hsmethod__call l = do
    Just ptr <- peek l (1)
    remove l 1
    stableptr <- F.peek (castPtr ptr)
    f <- deRefStablePtr stableptr
    f l

-- | Pushes Haskell function converted to a Lua function.
-- All values created will be garbage collected. Use as:
--
-- > Lua.pushhsfunction l myfun
-- > Lua.setglobal l "myfun"
--
-- You are not allowed to use @lua_error@ anywhere, but
-- use an error code of (-1) to the same effect. Push
-- error message as the sole return value.
pushhsfunction :: LuaImport a => LuaState -> a -> IO ()
pushhsfunction l f = do
    stableptr <- newStablePtr (luaimport f)
    p <- newuserdata l (F.sizeOf stableptr);
    F.poke (castPtr p) stableptr
    v <- newmetatable l "HaskellImportedFunction"
    when (v/=0) $ do
        -- create new metatable, fill it with two entries __gc and __call
        push l hsmethod__gc_addr
        setfield l (-2) "__gc"
        push l c_lua_neutralize_longjmp_addr
        setfield l (-2) "__call"
    setmetatable l (-2)
    return ()

-- | Imports a Haskell function and registers it at global name.
registerhsfunction :: LuaImport a => LuaState -> B.ByteString -> a -> IO ()
registerhsfunction l n f = pushhsfunction l f >> setglobal l n
