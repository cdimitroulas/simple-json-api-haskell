# The simplest Haskell JSON API tutorial

I was following along on [this blogpost](https://felixmulder.com/writing/2019/10/05/Designing-testable-components.html)
by Felix Mulder and I found it quite challenging to grasp the main concepts
as a newcomer to Haskell for a variety of reasons.

This is my attempt to recreate that blogpost using a much more simplified approach which doesn't
require as much Haskell knowledge to follow. I hope this is useful to others and I'm sure it
will be a useful learning experience for myself!

## Let's build an JSON API!

In this tutorial we're going to be building a very simple HTTP API with two endpoints;

- Create a user `POST /user`, receive the new user ID (String) in the response

- Delete a user `DELETE /user/:userId`

## Getting started

This tutorial assumes that you will be using `stack` to build your Haskell project.

A `package.yaml` file you can copy to ensure you have the right dependencies and language
extensions can be:
https://github.com/cdimitroulas/simple-json-api-haskell/blob/main/package.yaml

## Our domain types

Lets begin by defining some simple types for our domain and the necessary ToJSON and FromJSON instances for them.

```haskell
-- User.hs
module User where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User
  { userId :: Text,
    userName :: Text
  }

-- Data type which describes the request which will be received to create
-- a user
data CreateUserRequest = CreateUserRequest
  { name :: Text,
    password :: Text
  }
  deriving (Generic)

-- We define a FromJSON instance for CreateUserRequest because we will want
-- to parse it from a HTTP request body (JSON).
instance FromJSON CreateUserRequest
```

## Faking a database

To keep this tutorial simple and avoid us having to set up a database we will be using a
mutable variable to store our users. Mutable variables in Haskell I hear you cry?! Is that even a thing?!

Well, it's probably not exactly what you are imagining if you are coming from another language where
mutating variables is easily achievable but in Haskell `IORef` provides a way to mutate a variable, so we
are going to use this! (This approach is obviously just for educational purposes and is in no way
recommended for a real application :D)

```haskell
-- Db.hs
module Db (DbUsr (..), getUserStore, insertUser, deleteUser, mkDb, UserStore (..)) where

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
```

## Putting together our JSON API

Now we have all of the pieces of our application that we need, it's time to put them
together and expose them via a JSON API!  In order to do this we will be
using the [`scotty`](https://github.com/scotty-web/scotty) library (very similar
to `Sinatra` from Ruby!).

```haskell
-- Main.hs
module Main where

import Control.Monad.IO.Class
import qualified Db
import User (CreateUserRequest (..))
import Web.Scotty

main :: IO ()
main = do
  -- Initialize our fake DB
  db <- Db.mkDb

  -- Run the scotty web app on port 8080
  scotty 8080 $ do
    -- Listen for POST requests on the "/users" endpoint
    post "/users" $
      do
        -- parse the request body into our CreateUserRequest type
        createUserReq <- jsonData

        -- Create our new user.
        -- In order for this compile we need to use liftIO here to lift the IO from our
        -- createUser function. This is because the `post` function from scotty expects an
        -- ActionM action instead of an IO action
        newUserId <- liftIO $ createUser db createUserReq

        -- Return the user ID of the new user in the HTTP response
        json newUserId

    -- Listen for DELETE requests on the "/users/:userId" endpoint
    delete "/users/:userId" $ do
      -- Get the value of the userId from the URL
      userId <- param "userId"

      -- Delete the relevant user
      -- Same as with the user creation, we need to use liftIO here.
      liftIO $ Db.deleteUser db userId

-- Our createUser function simply deals with constructing a DbUsr value and passes it
-- to the Db.insertUser function
createUser :: Db.UserStore -> CreateUserRequest -> IO Int
createUser db CreateUserRequest {name = uname, password = pwd} = Db.insertUser db dbusr
  where
    dbusr = Db.DbUsr {Db.dbUsrName = uname, Db.dbUsrPassword = pwd}
```
