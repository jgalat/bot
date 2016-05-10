module CommandAST where

  data Command = Command String Comm

  type Var = String

  data Comm = Comm [Parameter] [Statement]
            deriving Show

  data Parameter  = Parameter Type Var
                  deriving Show

  data Type = Unit
            deriving Show

  data Statement  = Assign Var Expr
                  | If Expr [Statement]
                  | IfElse Expr [Statement] [Statement]
                  | While Expr [Statement]
                  deriving Show

  type Expr = ()
