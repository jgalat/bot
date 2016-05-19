module CommandAST where

  import qualified Data.Map as M

  type Var = String

  data Comm = Comm [(Var, Type)] [Statement]
            deriving Show

  data Type = Expected Type
            | Number
            | String
            | Bool
            | JSON
            deriving Show

  data Statement  = Assign Var Expr
                  | If Expr [Statement]
                  | IfElse Expr [Statement] [Statement]
                  | While Expr [Statement]
                  | Do [Statement] Expr
                  deriving Show

  data Expr = TrueExp
            | FalseExp
            | Var Var
            | Const Float
            | Str String
            | Not Expr
            | And Expr Expr
            | Or Expr Expr
            | Equals Expr Expr
            | Greater Expr Expr
            | Lower Expr Expr
            | GreaterEquals Expr Expr
            | LowerEquals Expr Expr
            | Plus Expr Expr
            | Minus Expr Expr
            | Negate Expr
            | Multiply Expr Expr
            | Divide Expr Expr
            | Index Expr Expr
            | Get Expr
            | Post Expr Expr
            | JsonExp JSON
            deriving Show

  data JSON = JsonObject (M.Map String JSON)
            | JsonArray [JSON]
            | JsonString String
            | JsonNumber Float
            | JsonBool Bool
            deriving Show
