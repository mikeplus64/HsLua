{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-} 

-- |
-- Module      : Scripting.Lua.ConfigFile
-- Copyright   : (c) Benjamin Geer 2011
--
-- License     : BSD3-style
--
-- Maintainer  : benjamin.geer@gmail.com
-- Stability   : alpha
-- Portability : portable, ffi
--
-- Reads configuration files written in Lua.  See @http:\/\/www.lua.org\/@
-- for more details.
module Scripting.Lua.ConfigFile
       (
         Config,
         openConfig,
         closeConfig,
         getBool,
         getString,
         getInt,
         getDouble,
         getList,
         getNestedLists,
         getAssocList,
         getListOfAssocLists,
         getNestedAssocLists,
         ConfigFileException
       ) where

import qualified Scripting.Lua as Lua
import System.IO (FilePath)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM, forM_)
import Data.Typeable (Typeable)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import Data.Monoid

-- | Represents an open configuration file.
data Config = Config Lua.LuaState

-- | Thrown when an error occurs in reading a configuration file.
data ConfigFileException = ConfigFileException B.ByteString
                         deriving (Show, Typeable)
instance Exception ConfigFileException

-- | Opens a config file and returns an opaque reference to the file.
-- You must close this reference using @close@ when you're done reading
-- the file.
openConfig :: FilePath -> IO Config
openConfig path = do
  l <- Lua.newstate
  loadResult <- Lua.loadfile l path
  callResult <- Lua.call l 0 0
  if loadResult /= 0 || callResult /= 0 then
    do
      errMsg <- Lua.tostring l (-1)
      throwIO $ ConfigFileException $ "cannot run config file: " <> errMsg
    else return (Config l)

-- | Closes a configuration file.
closeConfig :: Config -> IO ()
closeConfig (Config l) =
  -- putStrLn "closing Lua"
  Lua.close l

-- | Returns a boolean value from a configuration file.  Returns @False@
-- if the value is not defined in the file.  Example:
-- 
-- > someVal = true
getBool :: Config -> B.ByteString -> IO Bool
getBool (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TBOOLEAN) -> return v
    (Nothing, Lua.TNIL) -> return False
    (_, _) -> throwIO $ ConfigFileException $
              "expected boolean value: " <> name

-- | Returns a string value from a configuration file.  Returns the
-- empty string if the value is not defined in the file.  Example:
-- 
-- > someVal = "foo"
getString :: Config -> B.ByteString -> IO B.ByteString
getString (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TSTRING) -> return v
    (Nothing, Lua.TNIL) -> return ""
    (_, _) -> throwIO $ ConfigFileException $
              "expected string value: " <> name

-- | Returns an integer value from a configuration file.  Example:
-- 
-- > someVal = 2
getInt :: Config -> B.ByteString -> IO (Maybe Int)
getInt (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TNUMBER) -> return (Just v)
    (Nothing, Lua.TNIL) -> return Nothing
    (_, _) -> throwIO $ ConfigFileException $
              "expected numeric value: " <> name

-- | Returns a double value from a configuration file.  Example:
-- 
-- > someVal = 3.1415926
getDouble :: Config -> B.ByteString -> IO (Maybe Double)
getDouble (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TNUMBER) -> return (Just v)
    (Nothing, Lua.TNIL) -> return Nothing
    (_, _) -> throwIO $ ConfigFileException $
              "expected numeric value: " <> name

-- | Returns a list of strings (i.e. a Lua table in which the keys
-- are integers and the values are strings) from a configuration file.
-- Example:
-- 
-- > someVal = { "foo", "bar", "baz" }
getList :: Config -> B.ByteString -> IO [B.ByteString]
getList (Config l) name =
  getTable l name getListOfStrings

-- | Returns a list of lists, i.e. a Lua table of tables.  In the outer
-- table, the keys are integers and the values are tables, and in the inner
-- tables, the keys are integers and the values are strings.  Example:
-- 
-- > someVal = {
-- >    { "foo one", "foo two", "foo three" },
-- >    { "bar one", "bar two", "bar three" }
-- > }
getNestedLists :: Config -> B.ByteString -> IO [[B.ByteString]]
getNestedLists (Config l) name =
  getTable l name (\l name -> getOuterList l name getListOfStrings)

-- | Returns an association list, i.e. a Lua table in which the keys
-- and values are strings.  Example:
-- 
-- > someVal = {
-- >    one = "foo",
-- >    two = "bar",
-- >    three = "baz"
-- > }
getAssocList :: Config -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
getAssocList (Config l) name =
  getTable l name getColumns

-- | Returns a list of association lists, i.e. a Lua table of tables.
-- In the outer table, the keys are integers and the values are tables,
-- and in the inner tables, the keys and values are strings.  Example:
-- 
-- > someVal = {
-- >    {
-- >       foo = "aaa",
-- >       bar = "bbb",
-- >       baz = "ccc"
-- >    },
-- >    {
-- >       foo = "ddd",
-- >       bar = "eee",
-- >       baz = "fff"
-- >    }
-- > }
getListOfAssocLists :: Config -> B.ByteString -> IO [[(B.ByteString, B.ByteString)]]
getListOfAssocLists (Config l) name =
  getTable l name (\l name -> getOuterList l name getColumns)

-- | Returns an association list of association lists, i.e. a Lua table
-- of tables.  In the outer table, the keys are strings and the values
-- are tables, and in the inner tables, the keys and values are strings.
-- Example:
-- 
-- > someVal = {
-- >    something = {
-- >       foo = "aaa",
-- >       bar = "bbb",
-- >       baz = "ccc"
-- >    },
-- >    somethingElse = {
-- >       foo = "ddd",
-- >       bar = "eee",
-- >       baz = "fff"
-- >    }
-- > }
getNestedAssocLists :: Config -> B.ByteString -> IO [(B.ByteString, [(B.ByteString, B.ByteString)])]
getNestedAssocLists (Config l) name =
  getTable l name getRows

-- Private functions

{-

Gets a Lua global and pops it off the Lua stack.

-}
getGlobalVal l name = do  
  Lua.getglobal l name
  val <- Lua.peek l (-1)
  valType <- Lua.ltype l (-1)
  Lua.pop l 1
  return (val, valType)
  
{-

Checks whether a value can be converted to a string.

-}
canBeString valType =
  valType `elem` [Lua.TSTRING, Lua.TNUMBER]

{-

Gets a Lua table, performs some action on it and returns the result
as a list.

-}
getTable :: Lua.LuaState ->
            B.ByteString ->
            (Lua.LuaState -> B.ByteString -> IO [a]) ->
            IO [a]
getTable l name f = do
  Lua.getglobal l name
  valType <- Lua.ltype l (-1)
  case valType of
    Lua.TTABLE -> do items <- f l name
                     Lua.pop l 1
                     return items
    Lua.TNIL -> return []
    _ -> throwIO $ ConfigFileException $ "expected table: " <> name
  

{-

Iterates over the elements of a Lua table whose keys are integers,  
performs some action on each element, and returns the results as a
list.

-}
forList :: Lua.LuaState ->
           IO a ->
           IO [a]
forList l f = do
  tableSize <- Lua.objlen l (-1)
  forM [1..tableSize] $ \i -> do
    Lua.push l i
    Lua.gettable l (-2)
    f

{-

Gets all elements from a Lua table representing a list.  Keys are
integers and values are strings.

-}
getListOfStrings :: Lua.LuaState ->
                    B.ByteString ->
                    IO [B.ByteString]
getListOfStrings l name =
  forList l $ do
    valType <- Lua.ltype l (-1)
    if canBeString valType then
      do
        valStr <- Lua.tostring l (-1)
        Lua.pop l 1
        return valStr
      else throwIO $ ConfigFileException $
           "expected table of strings: " <> name

{-

Gets all elements from a Lua table of tables.  In the outer table,
keys are integers and values are tables.  The function passed as an
argument knows the structure of the inner tables.

-}
getOuterList :: Lua.LuaState ->
                B.ByteString ->
                (Lua.LuaState -> B.ByteString -> IO a) ->
                IO [a]
getOuterList l name f =
  forList l $ do
    valType <- Lua.ltype l (-1)
    case valType of
      Lua.TTABLE -> do innerItems <- f l name
                       Lua.pop l 1
                       return innerItems
      _ -> throwIO $ ConfigFileException $ "expected table: " <> name

{-

Gets all elements from a Lua table of tables.  In the outer table,
each key is a string, and each value is a table.  In the inner tables,
keys and values are strings.

-}
getRows :: Lua.LuaState -> B.ByteString -> IO [(B.ByteString, [(B.ByteString, B.ByteString)])]
getRows l name = do
  -- putStrLn $ "entering getRows"
  -- stackDump l
  Lua.pushnil l
  getRemainingRows l name

{-

Recursively gets the remaining elements from a Lua table of tables.
In the outer table, each key is a string, and each value is a table.
In the inner tables, keys and values are strings.

-}
getRemainingRows :: Lua.LuaState -> B.ByteString -> IO [(B.ByteString, [(B.ByteString, B.ByteString)])]
getRemainingRows l name = do
  -- putStrLn $ "entering getRemainingRows"
  -- stackDump l
  hasNext <- Lua.next l (-2)
  if hasNext then
     do -- putStrLn $ "getRemainingRows: hasNext"
        keyType <- Lua.ltype l (-2)
        valType <- Lua.ltype l (-1)
        case (keyType, valType) of
          (Lua.TSTRING, Lua.TTABLE) ->
            do keyStr <- Lua.tostring l (-2)
               columns <- getColumns l name
               Lua.pop l 1
               rest <- getRemainingRows l name
               return ((keyStr, columns) : rest)
          (_, _) -> throwIO $ ConfigFileException $
                       "expected string keys and table values: " <> name
    else return []

{-

Gets all elements from a Lua table and returns them as a list of
key-value pairs, where keys and values are strings.

-}
getColumns :: Lua.LuaState -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
getColumns l name = do
  -- putStrLn $ "entering getColumns"
  -- stackDump l
  Lua.pushnil l
  getRemainingColumns l name

{-

Recursively gets the remaining elements from a Lua table and returns
them as a list of key-value pairs, where keys and values are strings.

-}
getRemainingColumns :: Lua.LuaState -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
getRemainingColumns l name = do
  -- putStrLn $ "entering getRemainingColumns"
  -- stackDump l
  hasNext <- Lua.next l (-2)
  if hasNext then
     do -- putStrLn $ "getRemainingColumns: hasNext"
        -- stackDump l
        keyType <- Lua.ltype l (-2)
        valType <- Lua.ltype l (-1)
        if keyType == Lua.TSTRING && canBeString valType then
          do
            keyStr <- Lua.tostring l (-2)
            valStr <- Lua.tostring l (-1)
            Lua.pop l 1
            rest <- getRemainingColumns l name
            return ((keyStr, valStr) : rest)
          else throwIO $ ConfigFileException $
               "expected string keys and string values: " <> name
    else return []

{-

Dumps the Lua stack for debugging purposes.

-}
stackDump l = do
  stackSize <- Lua.gettop l
  -- putStrLn $ "Stack dump:"
  forM_ (reverse [1..stackSize]) $ \i -> do
    let relativeIndex = stackSize - i + 1
    putStr $ "Index[" ++ show i ++ " / -" ++ show relativeIndex ++ "] = "
    itemType <- Lua.ltype l i
    case itemType of
      Lua.TNONE -> BC.putStr "TNONE"
      Lua.TNIL  -> BC.putStr "TNIL"
      Lua.TBOOLEAN -> do boolVal <- Lua.toboolean l i
                         putStr $ "TBOOLEAN " ++ show boolVal
      Lua.TLIGHTUSERDATA -> putStr "TLIGHTUSERDATA"
      Lua.TNUMBER -> do iVal <- Lua.tointeger l i
                        putStr $ "TNUMBER " ++ show iVal
      Lua.TSTRING -> do sVal <- Lua.tostring l i
                        putStr $ "TSTRING " ++ show sVal
      Lua.TTABLE    -> BC.putStr "TTABLE"
      Lua.TFUNCTION -> BC.putStr "TFUNCTION"
      Lua.TTHREAD   -> BC.putStr "TTHREAD"
    BC.putStrLn ""
  BC.putStrLn ""
