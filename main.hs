{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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


instance {-# OVERLAPPABLE #-} (ToJSON a) => ToContent a where
  toContent = toContent . toJSON
instance {-# OVERLAPPABLE #-} (ToJSON a) => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent



getUsersR :: Handler [User]
getUsersR =
  return
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    ]

getFirstUserR :: Handler User
getFirstUserR = head <$> getUsersR

main :: IO ()
main = warp 3000 HelloWorld
