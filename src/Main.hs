module Main where

  import System.IO
  import TelegramAPI
  import Keys

  main = do putStrLn "Hello World"
            sendMessage tokenBot jgalat "Hello World"
