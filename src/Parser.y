{
module Parser where

import Data.Char
import CommandAST
}

%monad { P } { thenP } { returnP }

%name parse_command command

%tokentype { Token }
%lexer { lexer } { TEOF }

%token
    ':'         { TColon }
    '.'         { TDot }
    ','         { TComma }
    '('         { TOpen }
    ')'         { TClose }
    TRUE        { TTrue }
    FALSE       { TFalse }
    '~'         { TNot }
    '&'         { TAnd }
    '|'         { TOr }
    '='         { TEquals }
    '>'         { TGreater }
    '<'         { TLower }
    '>='        { TGreaterEq }
    '<='        { TLowerEq }
    '+'         { TPlus }
    '-'         { TMinus }
    '*'         { TMul }
    '/'         { TDiv }
    '<<'        { TGet }
    '>>'        { TPost }
    STRING      { TString $$ }
    IDENTIFIER  { TIdentifier $$ }
    CONST       { TConst $$ }
    COMMAND     { TCommand }
    VAR         { TVar }
    IF          { TIf }
    ELSE        { TElse }
    WHILE       { TWhile }
    DO          { TDo }
    TYPEINT     { TTInt }
    TYPESTRING  { TTString }
    TYPEBOOL    { TTBool }
    TYPEJSON    { TTJson }



%left '='

%left '|'
%left '&'
%left '~'

%nonassoc '>' '<' '>=' '<=' '>>' '<<'
%left '+' '-'
%left '*' '/'
%left NEG

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
        : TYPEINT                                 { Int }
        | TYPESTRING                              { String }
        | TYPEBOOL                                { Bool }
        | TYPEJSON                                { JSON }

stmts   :: { [Statement] }
        : stmt stmts                              { $1 : $2 }
        |                                         { [] }

stmt    :: { Statement }
        : VAR IDENTIFIER '=' expr                 { Assign $2 $4 }
        | IF expr ':' stmts '.'                   { If $2 $4 }
        | IF expr ':' stmts ELSE ':' stmts '.'    { IfElse $2 $4 $7 }
        | WHILE expr ':' stmts '.'                { While $2 $4 }
        | DO ':' stmts '.' WHILE expr             { Do $3 $6 }

expr    :: { Expr }
        : '(' expr ')'                            { $2 }
        | TRUE                                    { ExpTrue }
        | FALSE                                   { ExpFalse }
        | IDENTIFIER                              { Var $1 }
        | CONST                                   { Const $1 }
        | STRING                                  { Str $1 }
        | '~' expr                                { Not $2 }
        | expr '&' expr                           { And $1 $3 }
        | expr '|' expr                           { Or $1 $3 }
        | expr '=' expr                           { Equals $1 $3 }
        | expr '>' expr                           { Greater $1 $3 }
        | expr '<' expr                           { Lower $1 $3 }
        | expr '>=' expr                          { GreaterEquals $1 $3 }
        | expr '<=' expr                          { LowerEquals $1 $3 }
        | expr '+' expr                           { Plus $1 $3 }
        | expr '-' expr                           { Minus $1 $3 }
        | '-' expr %prec NEG                      { Negate $2 }
        | expr '*' expr                           { Multiply $1 $3 }
        | expr '/' expr                           { Divide $1 $3 }
        | '<<' expr                               { Get $2 }
        | expr '>>' expr                          { Post $1 $3 }

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
            | TConst Int
            | TString String
            | TCommand
            | TVar
            | TIf
            | TElse
            | TWhile
            | TDo
            | TType
            | TTInt
            | TTString
            | TTBool
            | TTJson
            | TDot
            | TComma
            | TOpen
            | TClose
            | TColon
            | TTrue
            | TFalse
            | TNot
            | TAnd
            | TOr
            | TEquals
            | TGreater
            | TLower
            | TGreaterEq
            | TLowerEq
            | TPlus
            | TMinus
            | TMul
            | TDiv
            | TGet
            | TPost
            | TEOF
            deriving Show

----------------------------------
lexer cont s = case s of
                    []                          -> cont TEOF []
                    ('\n':cs)                   ->  \line -> lexer cont cs (line + 1)
                    (c:cs)
                        | isSpace c             -> lexer cont cs
                        | isAlpha c || c == '_' -> lexAlpha cont s
                        | isDigit c             -> lexNumber cont s
                    ('-':('-':cs))              -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('<':('<':cs))              -> cont TGet cs
                    ('>':('>':cs))              -> cont TPost cs
                    ('>':('=':cs))              -> cont TGreaterEq cs
                    ('<':('=':cs))              -> cont TLowerEq cs
                    ('\"':cs)                   -> lexString cont cs
                    ('~':cs)                    -> cont TNot cs
                    ('&':cs)                    -> cont TAnd cs
                    ('|':cs)                    -> cont TOr cs
                    ('>':cs)                    -> cont TGreater cs
                    ('<':cs)                    -> cont TLower cs
                    ('=':cs)                    -> cont TEquals cs
                    ('+':cs)                    -> cont TPlus cs
                    ('-':cs)                    -> cont TMinus cs
                    ('*':cs)                    -> cont TMul cs
                    ('/':cs)                    -> cont TDiv cs
                    ('.':cs)                    -> cont TDot cs
                    (',':cs)                    -> cont TComma cs
                    ('(':cs)                    -> cont TOpen cs
                    (')':cs)                    -> cont TClose cs
                    (':':cs)                    -> cont TColon cs
                    unknown                     -> \line -> Failed $ "Línea " ++ show line ++ ": No se puede reconocer " ++ (show $ take 10 unknown)++ "..."

lexAlpha :: (Token -> P a) -> P a
lexAlpha cont s = case span (\c -> isAlpha c || c == '_' ) s  of
                    ("command", rest)  -> cont TCommand rest
                    ("var", rest)      -> cont TVar rest
                    ("if", rest)       -> cont TIf rest
                    ("else", rest)     -> cont TElse rest
                    ("while", rest)    -> cont TWhile rest
                    ("do", rest)       -> cont TDo rest
                    ("true", rest)     -> cont TTrue rest
                    ("false", rest)    -> cont TFalse rest
                    ("Int", rest)      -> cont TTInt rest
                    ("String", rest)   -> cont TTString rest
                    ("Bool", rest)     -> cont TTBool rest
                    ("JSON", rest)     -> cont TTJson rest
                    (var, rest)        -> cont (TIdentifier var) rest

lexString :: (Token -> P a) -> P a
lexString cont s =  case span (/='\"') s of
                      (string, rest)  ->  case tail' rest of
                                            Just xs ->  cont (TString string) xs
                                            Nothing ->  \line -> Failed $ "Línea " ++ show line ++ ": Falta caracter \'\"\'"
                    where tail' [] = Nothing
                          tail' xs = Just $ tail xs

lexNumber :: (Token -> P a) -> P a
lexNumber cont s =  case span isDigit s of
                      (number, rest)  -> cont (TConst (read number)) rest

parseCommand s = parse_command s 1
}
