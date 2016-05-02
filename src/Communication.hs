module Communication where

  import Network.HTTP.Conduit
  import Data.ByteString.Lazy
  import Data.Aeson
  import Control.Monad.IO.Class

  data User = User {  user_id     :: Int, -- id
                      first_name  :: String
                    } deriving Show

  data Chat = Chat {  chat_id   :: Int, -- id
                      chat_type :: String
                    } deriving Show

  data Message = Message {  message_id  :: Int, -- id
                            from        :: User,
                            date        :: Int,
                            chat        :: Chat,
                            text        :: String
                          } deriving Show

  data Update = Update {  update_id :: Int,
                          message :: Message
                        } deriving Show

  data Reply =  Reply { ok :: Bool,
                        result :: [Update]
                      } deriving Show


  manager :: IO Manager
  manager =  newManager tlsManagerSettings

  get :: String -> String -> IO ByteString
  get token method = do   request <- parseUrl $ "https://api.telegram.org/bot" ++ token ++ "/" ++ method
                          m       <- manager
                          r       <- httpLbs request m
                          return (responseBody r)

  post :: String -> String -> IO ByteString
  post _ _ = return empty
