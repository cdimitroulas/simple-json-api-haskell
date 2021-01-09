module Db
  ( DbUsr (..),
    getUserStore,
    insertUser,
    deleteUser,
    mkDb,
    UserStore (..),
  )
where

import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data DbUsr = DbUsr
  { dbUsrName :: Text,
    dbUsrPassword :: Text
  }
  deriving (Show)

newtype UserStore = UserStore {unUsrStore :: IORef (Map Int DbUsr)}

-- Creates our initial empty database
mkDb :: IO UserStore
mkDb = do
  ref <- newIORef (Map.empty :: Map Int DbUsr)
  pure $ UserStore ref

-- Accepts a default value to return in case the list is empty
safeLast :: a -> [a] -> a
safeLast x [] = x
safeLast _ (x : xs) = safeLast x xs

-- Utility to allow us to read the data in our "database"
getUserStore :: UserStore -> IO (Map Int DbUsr)
getUserStore (UserStore store) = readIORef store

-- VERY naive utility to get the next unique user ID
getNextId :: UserStore -> IO Int
getNextId x = (+ 1) . safeLast 0 . sort . Map.keys <$> getUserStore x

-- insertUser uses getNextId to get the next ID and then updates our database using
-- modifyIORef from the Data.IORef library. It returns the new ID as a result.
insertUser :: UserStore -> DbUsr -> IO Int
insertUser userStore usr = do
  nextId <- getNextId userStore
  modifyIORef (unUsrStore userStore) (Map.insert nextId usr)
  pure nextId

-- deleteUser updates our database by deleting the relevant user data
deleteUser :: UserStore -> Int -> IO ()
deleteUser usrStore uid = modifyIORef' (unUsrStore usrStore) (Map.delete uid)
