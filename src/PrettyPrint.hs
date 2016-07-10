module PrettyPrint where

  import CommandAST
  import qualified Data.Map as M

  unescape :: String -> String
  unescape [] = []
  unescape ('\\':(x:xs)) = case x of
                            '\\' -> '\\' : unescape xs
                            'r'  -> '\r' : unescape xs
                            't'  -> '\t' : unescape xs
                            'n'  -> '\n' : unescape xs
                            '\'' -> '\'' : unescape xs
                            '['  -> '['  : unescape xs
                            _    -> x : unescape xs
  unescape (x:xs) = x : unescape xs

  showConst :: Double -> String
  showConst n = if fromIntegral (truncate n) < n then show n else show (truncate n)

  showExpr :: Expr -> String
  showExpr (Const n)      = showConst n
  showExpr (TrueExp)      = "true"
  showExpr (FalseExp)     = "false"
  showExpr (Str s)        = '\"' : (s ++ "\"")
  showExpr (JsonObject o) = showJsonObject (M.toList o)
  showExpr (JsonArray a)  = showJsonArray a
  showExpr _ = "WTF!"

  showJsonObject :: [(String, Expr)] -> String
  showJsonObject [] = "{}"
  showJsonObject xs = '{' : showJsonObject' xs
                      where showJsonObject' [(k,e)]     = ("\"" ++ (k ++ "\" : ")) ++ showExpr e ++ "}"
                            showJsonObject' ((k,e) : es)= ("\"" ++ (k ++ "\" : ")) ++ showExpr e ++ ", " ++ showJsonObject' es

  showJsonArray :: [Expr] -> String
  showJsonArray []  = "[]"
  showJsonArray xs  = "\\[" ++ showJsonArray' xs
                      where showJsonArray' [e]    = showExpr e ++ "]"
                            showJsonArray' (e:es) = showExpr e ++ ", " ++ showJsonArray' es
