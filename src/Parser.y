{
module Parser (parseCommand, ParseResult) where

import Data.Char
import CommandAST
}

%monad { P } { thenP } { returnP }

%name parse_command command

%tokentype { Token }
%lexer { lexer } { TEOF }

%token
    '='         { TEquals }
    ':'         { TColon }
    '.'         { TDot }
    ','         { TComma }
    '('         { TOpen }
    ')'         { TClose }
    COMMAND     { TCommand }
    VAR         { TVar }
    IDENTIFIER  { TIdentifier $$ }
    IF          { TIf }
    ELSE        { TElse }
    WHILE       { TWhile }
    UNIT        { TUnit }
    TUNIT       { TTUnit }



%left '='

%%

command :: { Comm }
        : COMMAND '(' parameters ')' ':' stmts '.'  { Comm $3 $6 }

parameters    :: { [Parameter] }
              : list_of_parameters                { $1 }
              |                                   { [] }

list_of_parameters  :: { [Parameter] }
                    : single_parameter                            { [$1] }
                    | single_parameter  ',' list_of_parameters    { $1 : $3 }

single_parameter    :: { Parameter }
                    : type ':' IDENTIFIER         { Parameter $1 $3}

type    :: { Type }
        : TUNIT                                   { Unit }

stmts   :: { [Statement] }
        : stmt stmts                              { $1 : $2 }
        |                                         { [] }

stmt    :: { Statement }
        : '(' stmt ')'                            { $2 }
        | VAR IDENTIFIER '=' expr                 { Assign $2 $4 }
        | IF expr ':' stmts '.'                   { If $2 $4 }
        | IF expr ':' stmts ELSE ':' stmts '.'    { IfElse $2 $4 $7 }
        | WHILE expr ':' stmts '.'                { While $2 $4 }


expr    :: { () }
        : UNIT                                    { () }

{

data ParseResult a = Ok a | Failed String
                     deriving Show

type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token  = TIdentifier Var
            | TCommand
            | TVar
            | TIf
            | TElse
            | TWhile
            | TType
            | TUnit
            | TTUnit
            | TDot
            | TComma
            | TOpen
            | TClose
            | TColon
            | TEquals
            | TEOF
            deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    -- ('-':cs) -> cont THyphen cs
                    ('.':cs) -> cont TDot cs
                    (',':cs) -> cont TComma cs
                    ('(':cs) -> cont TOpen cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                                           ("command", rest)  -> cont TCommand rest
                                           ("var", rest)      -> cont TVar rest
                                           ("if", rest)       -> cont TIf rest
                                           ("else", rest)     -> cont TElse rest
                                           ("while", rest)    -> cont TWhile rest
                                           ("Unit", rest)     -> cont TTUnit rest
                                           ("unit", rest)     -> cont TUnit rest
                                           (var, rest)        -> cont (TIdentifier var) rest

parseCommand s = parse_command s 1
}
