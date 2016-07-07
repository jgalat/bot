module Execute where

  import Data.List
  import Control.Monad.IO.Class

  import CommandAST
  import Environment
  import Monads (Execution, runExecution)
  import TelegramAPI
  import Communication (post, get)
  import State (ExecState (..))


  execute :: [Expr] -> ExecState -> Comm -> IO (Either String ())
  execute args st (Comm e prmt c) = case cmpArgs args (map snd prmt) of
                                      Left err  -> return (Left err)
                                      _         -> let  envEx   = envFromList $ zip (map fst prmt) args
                                                        execSt  = st {  exprEnv = envUnion (exprEnv st) envEx,
                                                                        typeEnv = envUnion (typeEnv st) (envFromList prmt)
                                                                     }
                                                   in   runExecution (evalComms c) execSt

  cmpArgs :: [Expr] -> [Type] -> Either String ()
  cmpArgs [] [] = Right ()
  cmpArgs [] _ = Left "Error" -- TODO
  cmpArgs _ [] = Left "Error" -- TODO
  cmpArgs (Const _ : xs) (Number : ts) = cmpArgs xs ts
  cmpArgs (Str _ : xs) (String : ts) = cmpArgs xs ts
  cmpArgs (TrueExp : xs) (Bool : ts) = cmpArgs xs ts
  cmpArgs (FalseExp: xs) (Bool : ts) = cmpArgs xs ts
  cmpArgs (JsonArray _ : xs) (JSON : ts) = cmpArgs xs ts
  cmpArgs (JsonObject _ : xs) (JSON : ts) = cmpArgs xs ts
  cmpArgs _ _ = Left "Error" -- TODO

  evalComms :: [Statement] -> Execution ()
  evalComms _ = liftIO $ putStrLn "Execution!"
