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
