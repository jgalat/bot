{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI
        (
          getMe,
          getUpdates,
          sendMessage,
          sendMessage',
          module TelegramAPITypes
        ) where

  import Data.Aeson
  import Control.Monad.IO.Class
  import Data.ByteString.Lazy
  import Network.HTTP.Conduit (Manager)

  import TelegramAPITypes
  import Communication


  apiURL :: String
  apiURL = "https://api.telegram.org/bot"

  getMe :: (MonadIO m) => Manager -> String -> m (Maybe Reply)
  getMe m token = decode <$> liftIO (get m $ apiURL ++ token ++ "/getMe")

  getUpdates :: (MonadIO m) => Manager -> String -> Int -> m (Maybe Reply)
  getUpdates m token offset = decode <$> liftIO (get m $ apiURL ++ token ++ "/getUpdates?offset=" ++ show offset)

  sendMessage :: (MonadIO m) => Manager -> String -> Int -> String -> m (Maybe Reply)
  sendMessage m token to msg =  let json = encode SimpleMessage {to = to, msg = msg, pm = "Markdown"}
                                in  decode <$> liftIO (post m (apiURL ++ token ++ "/sendMessage") json)

  sendMessage' :: (MonadIO m) => Manager -> String -> Int -> String -> m ByteString
  sendMessage' m token to msg = let json = encode SimpleMessage {to = to, msg = msg, pm = "Markdown"}
                                in liftIO (post m (apiURL ++ token ++ "/sendMessage") json)
