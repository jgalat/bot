{-# LANGUAGE OverloadedStrings #-}

module Communication
        (
          get,
          post,
          managertls
        ) where

  import Network.HTTP.Conduit
  import Network.HTTP.Types.Status
  import Data.ByteString.Lazy
  import Control.Exception

  managertls :: IO Manager
  managertls =  newManager tlsManagerSettings

  handleError :: HttpException -> IO ByteString
  handleError (StatusCodeException _ l _) = return (maybe "" fromStrict (lookup "X-Response-Body-Start" l))
  handleError _                           = return "" -- TODO

  get :: Manager -> String ->IO ByteString
  get m url = do  request   <- parseUrl url
                  (responseBody <$> httpLbs request m) `catch` handleError

  post :: Manager -> String -> ByteString -> IO ByteString
  post m url body = do  r     <- parseUrl url
                        let request   = r { method = "POST",
                                            requestHeaders = [("Content-Type", "application/json")],
                                            requestBody = RequestBodyLBS body
                                          }
                        (responseBody <$> httpLbs request m) `catch` handleError
