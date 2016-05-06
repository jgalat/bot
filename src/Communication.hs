{-# LANGUAGE OverloadedStrings #-}

module Communication
        (
          get,
          post
        ) where

  import Network.HTTP.Conduit
  import Network.HTTP.Types.Status
  import Data.ByteString.Lazy
  import Control.Exception

  manager :: IO Manager
  manager =  newManager tlsManagerSettings

  handleError :: HttpException -> IO ByteString
  handleError (StatusCodeException _ l _) = return (maybe "" fromStrict (lookup "X-Response-Body-Start" l))
  handleError _                           = return "" -- TODO

  get :: String -> IO ByteString
  get url = do  request   <- parseUrl url
                m         <- manager
                (responseBody <$> httpLbs request m) `catch` handleError


  post :: String -> ByteString -> IO ByteString
  post url body = do  r     <- parseUrl url
                      let request   = r { method = "POST",
                                          requestHeaders = [("Content-Type", "application/json")],
                                          requestBody = RequestBodyLBS body
                                        }
                      m     <- manager
                      (responseBody <$> httpLbs request m) `catch` handleError
