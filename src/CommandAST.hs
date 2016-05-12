module CommandAST where

  data Command  = Command String Comm
                | FailedCommand
                deriving Show

  type Var = String

  data Comm = Comm [Parameter] [Statement]
            deriving Show

  data Parameter  = Parameter Type Var
                  deriving Show

  data Type = Int
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

  data Expr = ExpTrue
            | ExpFalse
            | Var Var
            | Const Int
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
            | Get Expr
            | Post Expr Expr
            deriving Show
