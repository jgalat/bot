module CommandAST where

  import Map

  type Var = String

  data Comm = Comm [(Var, Type)] [Statement]
            deriving Show

  data Type = ArrayType
            | Number
            | String
            | Bool
            | JSON
            deriving (Show, Eq)

  data Statement  = Assign Var Expr
                  | If Expr [Statement]
                  | IfElse Expr [Statement] [Statement]
                  | While Expr [Statement]
                  | Do [Statement] Expr
                  | For Var Expr [Statement]
                  deriving Show

  data Expr = Null
            | TrueExp
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
            | JsonObject (Map Expr)
            | Array [Expr]
            deriving Show
