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
