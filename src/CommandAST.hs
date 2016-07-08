module CommandAST where

  import Data.Map
  import Environment

  type Var = String

  data Comm = Comm (Env Type) [(Var, Type)] [Statement]
            deriving Show

  data Type = Undefined
            | Number
            | String
            | Bool
            | JSON
            deriving (Show, Eq)

  data Statement  = Declaration Var Expr
                  | Assign Var Expr
                  | If Expr [Statement]
                  | IfElse Expr [Statement] [Statement]
                  | While Expr [Statement]
                  | Do [Statement] Expr
                  deriving Show

  data Expr = TrueExp
            | FalseExp
            | Var Var
            | Const Double
            | Str String
            | Not Expr
            | And Expr Expr
            | Or Expr Expr
            | Equals Expr Expr
            | Greater Expr Expr
            | Lower Expr Expr
            | GreaterEquals Expr Expr
            | LowerEquals Expr Expr
            | Negate Expr
            | Plus Expr Expr
            | Minus Expr Expr
            | Multiply Expr Expr
            | Divide Expr Expr
            | Index Expr Expr
            | Get Expr
            | Post Expr Expr
            | JsonObject (Map String Expr)
            | JsonArray [Expr]
            deriving Show
