{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Vector as V
import           Yesod



data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''User)



data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/users UsersR GET
/firstUser FirstUserR GET
|]

instance Yesod HelloWorld



getUsersR :: Handler Value
getUsersR =
  returnJson
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    ]

getFirstUserR :: Handler Value
getFirstUserR = do
  users <- getUsersR
  case users of
    Array values -> return $ V.head values
    _ -> error "We expected an array!"

main :: IO ()
main = warp 3000 HelloWorld
