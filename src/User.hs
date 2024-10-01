module User where

import Data.Aeson
import Data.Text
import GHC.Generics
import Db (DbUsr(..))

data User = User
  { userId :: Int,
    userName :: Text
  } deriving (Generic)

-- We define a ToJSON instance for User because we will want
-- to return users in HTTP responses.
instance ToJSON User

usrFromDbUser :: (Int, DbUsr) -> User
usrFromDbUser (usrId, dbUsr) = User usrId dbUsr.dbUsrName

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
