{-# LANGUAGE OverloadedStrings #-}

module Communication where

  import Network.HTTP.Conduit
  import Network.HTTP.Types.Status
  import Data.ByteString.Lazy
  import Data.Aeson
  import Control.Monad.IO.Class
  import Control.Monad
  import Control.Exception
  import Control.Applicative

  data User = User {  user_id     :: Int,
                      first_name  :: String,
                      last_name   :: Maybe String,
                      username    :: Maybe String
                    } deriving Show

  instance FromJSON User where
    parseJSON (Object v)  = User  <$> v .: "id"
                                  <*> v .: "first_name"
                                  <*> v .:? "last_name"
                                  <*> v .:? "username"
    parseJSON _           = mzero

  data Chat = Chat {  chat_id   :: Int,
                      chat_type :: String,
                      title     :: Maybe String
                    } deriving Show

  instance FromJSON Chat where
    parseJSON (Object v)  = Chat  <$> v .: "id"
                                  <*> v .: "type"
                                  <*> v .:? "title"
    parseJSON _           = mzero

  data Message = Message {  message_id  :: Int,
                            from        :: Maybe User,
                            chat        :: Chat,
                            date        :: Int,
                            text        :: Maybe String
                          } deriving Show

  instance FromJSON Message where
    parseJSON (Object v)  = Message <$> v .: "message_id"
                                    <*> v .:? "from"
                                    <*> v .: "chat"
                                    <*> v .: "date"
                                    <*> v .:? "text"
    parseJSON _           = mzero

  data Update = Update {  update_id :: Int,
                          message   :: Message
                        } deriving Show

  instance FromJSON Update where
    parseJSON (Object v)  = Update  <$> v .: "update_id"
                                    <*> v .: "message"
    parseJSON _           = mzero

  data Reply =  ReplyUpdates {  ok      :: Bool,
                                updates :: [Update]
                              }
              | ReplyMessage {  ok        :: Bool,
                                r_message :: Message
                              }
              | ReplyError   {  ok          :: Bool,
                                error_code  :: Int,
                                description :: String
                              } deriving Show

  instance FromJSON Reply where
    parseJSON (Object v)  = (ReplyUpdates   <$> v .: "ok"
                                            <*> v .: "result")
                            <|> (ReplyMessage <$> v .: "ok"
                                              <*> v .: "result")
                            <|> (ReplyError <$> v .: "ok"
                                            <*> v .: "error_code"
                                            <*> v .: "description")
    parseJSON _           = mzero

  data SimpleMessage = SimpleMessage {  to  :: Int,
                                        msg :: String
                                      } deriving Show

  instance ToJSON SimpleMessage where
     toJSON (SimpleMessage to msg)  = object ["chat_id" .= to, "text" .= msg]

  manager :: IO Manager
  manager =  newManager tlsManagerSettings

  handle_error :: HttpException -> IO ByteString
  handle_error (StatusCodeException _ l _) = return (maybe "" fromStrict (lookup "X-Response-Body-Start" l))
  handle_error _                           = return "" -- TODO

  get :: String -> IO ByteString
  get url = do  request   <- parseUrl url
                m         <- manager
                (fmap responseBody $ httpLbs request m) `catch` handle_error


  post :: String -> ByteString -> IO ByteString
  post url body = do  r     <- parseUrl $ url
                      let request   = r { method = "POST",
                                          requestHeaders = [("Content-Type", "application/json")],
                                          requestBody = RequestBodyLBS body
                                        }
                      m     <- manager
                      (fmap responseBody $ httpLbs request m) `catch` handle_error

  apiURL :: String
  apiURL = "https://api.telegram.org/bot"

  getUpdates :: (MonadIO m) => String -> m (Maybe Reply)
  getUpdates token  = do  reply  <- liftIO $ get $ apiURL ++ token ++ "/getUpdates" -- ?offset=nnnnnn"
                          return (decode reply)

  sendMessage :: (MonadIO m) => String -> Int -> String -> m (Maybe Reply)
  sendMessage token to msg = do let  json = encode (SimpleMessage {to = to, msg = msg})
                                reply <- liftIO $ post (apiURL ++ token ++ "/sendMessage") json
                                return (decode reply)
