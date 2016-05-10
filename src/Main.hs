module Main where

  import System.IO
  import TelegramAPI
  import TelegramAPITypes
  import Keys
  import Parser

  main = do putStrLn "Hello World"
            sendMessage tokenBot jgalat "Hello World"
            putStrLn "Repeating everything..."
            echo 0

  echo n = do reply <- getUpdates tokenBot n
              case reply of
                Nothing -> echo n
                Just rep ->case ok rep of
                              True -> case updates rep of
                                        []  -> echo n
                                        xs  -> do mapM (\x -> sendMessage tokenBot (chat_id $ chat $ message x) (maybe "" id $ text $ message x)) xs
                                                  echo (update_id (last xs) + 1)
