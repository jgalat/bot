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

  import TelegramAPITypes
  import Communication


  apiURL :: String
  apiURL = "https://api.telegram.org/bot"

  getMe :: (MonadIO m) => String -> m (Maybe Reply)
  getMe token = decode <$> liftIO (get $ apiURL ++ token ++ "/getMe")

  getUpdates :: (MonadIO m) => String -> Int -> m (Maybe Reply)
  getUpdates token offset = decode <$> liftIO (get $ apiURL ++ token ++ "/getUpdates?offset=" ++ show offset)

  sendMessage :: (MonadIO m) => String -> Int -> String -> m (Maybe Reply)
  sendMessage token to msg =  let json = encode SimpleMessage {to = to, msg = msg, pm = "Markdown"}
                              in  decode <$> liftIO (post (apiURL ++ token ++ "/sendMessage") json)

  sendMessage' :: (MonadIO m) => String -> Int -> String -> m (ByteString)
  sendMessage' token to msg = let json = encode SimpleMessage {to = to, msg = msg, pm = "Markdown"}
                              in liftIO (post (apiURL ++ token ++ "/sendMessage") json)
