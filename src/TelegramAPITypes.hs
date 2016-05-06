{-# LANGUAGE OverloadedStrings #-}

module TelegramAPITypes where

  import Data.Aeson
  import Control.Applicative
  import Control.Monad

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
              | ReplyBotInfo {  ok  :: Bool,
                                bot :: User
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
                            <|> (ReplyBotInfo <$> v .: "ok"
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
