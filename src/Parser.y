{
module Parser where

import Data.Char
import Control.Exception (catch)
import qualified Data.Map as M

import CommandAST

}

%monad { P } { thenP } { returnP }

%name parse_command command
%name parse_json json
%name parse_request request

%tokentype { Token }
%lexer { lexer } { TEOF }

%token
    ':'         { TColon }
    ';'         { TSemiColon}
    '.'         { TDot }
    ','         { TComma }
    '('         { TParenthesesOpen }
    ')'         { TParenthesesClose }
    '['         { TBracketsOpen }
    ']'         { TBracketsClose }
    '{'         { TBracesOpen }
    '}'         { TBracesClose }
    TRUE        { TTrue }
    FALSE       { TFalse }
    '~'         { TNot }
    '&'         { TAnd }
    '|'         { TOr }
    '='         { TAssign }
    '=='        { TEquals}
    '>'         { TGreater }
    '<'         { TLower }
    '>='        { TGreaterEq }
    '<='        { TLowerEq }
    '+'         { TPlus }
    '-'         { TMinus }
    '*'         { TAsterisc }
    '/'         { TSlash }
    '<<'        { TGet }
    '>>'        { TPost }
    '!'         { TExclamation }
    STRING      { TString $$ }
    IDENTIFIER  { TIdentifier $$ }
    CONST       { TConst $$ }
    COMMAND     { TCommand }
    VAR         { TVar }
    IF          { TIf }
    ELSE        { TElse }
    WHILE       { TWhile }
    DO          { TDo }
    TYPENUMBER  { TTNumber }
    TYPESTRING  { TTString }
    TYPEBOOL    { TTBool }

%left '='

%left '|'
%left '&'
%left '~'

%nonassoc '>' '<' '>=' '<=' '>>' '<<' '=='
%left '+' '-'
%left '*' '/'
%left NEG
%left '!'

%%

command :: { Comm }
        : COMMAND '(' parameters ')' ':' stmts '.'  { Comm M.empty $3 $6 }

parameters    :: { [(Var, Type)] }
              : list_of_parameters                { $1 }
              |                                   { [] }

list_of_parameters  :: { [(Var, Type)] }
                    : single_parameter                            { [$1] }
                    | single_parameter  ',' list_of_parameters    { $1 : $3 }

single_parameter    :: { (Var, Type) }
                    : type IDENTIFIER             { ($2, $1) }

type    :: { Type }
        : TYPENUMBER                              { Number }
        | TYPESTRING                              { String }
        | TYPEBOOL                                { Bool }

stmts   :: { [Statement] }
        : stmt stmts                              { $1 : $2 }
        | stmt ';' stmts                          { $1 : $3 }
        |                                         { [] }

stmt    :: { Statement }
        : VAR IDENTIFIER '=' expr                 { Declaration $2 $4 }
        | IDENTIFIER '=' expr                     { Assign $1 $3 }
        | IF expr ':' stmts '.'                   { If $2 $4 }
        | IF expr ':' stmts ELSE ':' stmts '.'    { IfElse $2 $4 $7 }
        | WHILE expr ':' stmts '.'                { While $2 $4 }
        | DO ':' stmts '.' WHILE expr             { Do $3 $6 }

expr    :: { Expr }
        : '(' expr ')'                            { $2 }
        | value                                   { $1 }
        | IDENTIFIER                              { Var $1 }
        | '~' expr                                { Not $2 }
        | expr '&' expr                           { And $1 $3 }
        | expr '|' expr                           { Or $1 $3 }
        | expr '==' expr                          { Equals $1 $3 }
        | expr '>' expr                           { Greater $1 $3 }
        | expr '<' expr                           { Lower $1 $3 }
        | expr '>=' expr                          { GreaterEquals $1 $3 }
        | expr '<=' expr                          { LowerEquals $1 $3 }
        | expr '+' expr                           { Plus $1 $3 }
        | expr '-' expr                           { Minus $1 $3 }
        | '-' expr %prec NEG                      { Negate $2 }
        | expr '*' expr                           { Multiply $1 $3 }
        | expr '/' expr                           { Divide $1 $3 }
        | expr '!' expr                           { Index $1 $3 }
        | '<<' expr                               { Get $2 }
        | expr '>>' expr                          { Post $1 $3 }

value   :: { Expr }
        : json                                    { $1 }
        | TRUE                                    { TrueExp }
        | FALSE                                   { FalseExp }
        | CONST                                   { Const $1 }
        | STRING                                  { Str $1 }

json    :: { Expr }
        : object                                  { JsonObject (M.fromList $1) }
        | array                                   { JsonArray  $1 }

object  :: { [(String, Expr)] }
        : '{' '}'                                 { [] }
        | '{' pairs '}'                           { $2 }

pairs   :: { [(String, Expr)] }
        : pair                                    { [$1] }
        | pair ',' pairs                          { $1 : $3 }

pair    :: { (String, Expr) }
        : STRING ':' value                        { ($1, $3) }

array   :: { [Expr] }
        : '[' ']'                                 { [] }
        | '[' values ']'                          { $2 }

values  :: { [Expr] }
        : value                                   { [$1] }
        | value ',' values                        { $1 : $3 }

request :: { (String, [Expr]) }
        : '/'IDENTIFIER arguments                 { ($2, $3) }

arguments :: { [Expr] }
          : value arguments                       { $1 : $2 }
          | IDENTIFIER arguments                  { Str $1 : $2}
          |                                       { [] }
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
            | TConst Float
            | TString String
            | TCommand
            | TVar
            | TAssign
            | TIf
            | TElse
            | TWhile
            | TDo
            | TType
            | TTNumber
            | TTString
            | TTBool
            | TDot
            | TComma
            | TParenthesesOpen
            | TParenthesesClose
            | TBracketsOpen
            | TBracketsClose
            | TBracesOpen
            | TBracesClose
            | TColon
            | TSemiColon
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
            | TAsterisc
            | TSlash
            | TExclamation
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
                    ('=':('=':cs))              -> cont TEquals cs
                    ('\"':cs)                   -> lexString cont cs
                    ('~':cs)                    -> cont TNot cs
                    ('&':cs)                    -> cont TAnd cs
                    ('|':cs)                    -> cont TOr cs
                    ('>':cs)                    -> cont TGreater cs
                    ('<':cs)                    -> cont TLower cs
                    ('=':cs)                    -> cont TAssign cs
                    ('+':cs)                    -> cont TPlus cs
                    ('-':cs)                    -> cont TMinus cs
                    ('*':cs)                    -> cont TAsterisc cs
                    ('/':cs)                    -> cont TSlash cs
                    ('!':cs)                    -> cont TExclamation cs
                    ('.':cs)                    -> cont TDot cs
                    (',':cs)                    -> cont TComma cs
                    ('(':cs)                    -> cont TParenthesesOpen cs
                    (')':cs)                    -> cont TParenthesesClose cs
                    ('[':cs)                    -> cont TBracketsOpen cs
                    (']':cs)                    -> cont TBracketsClose cs
                    ('{':cs)                    -> cont TBracesOpen cs
                    ('}':cs)                    -> cont TBracesClose cs
                    (':':cs)                    -> cont TColon cs
                    (';':cs)                    -> cont TSemiColon cs
                    unknown                     -> \line -> Failed $ "Línea " ++ show line ++ ": No se puede reconocer " ++ (show $ take 10 unknown)++ "..."

lexAlpha :: (Token -> P a) -> P a
lexAlpha cont s = case span (\c -> isAlpha c || isDigit c || c == '_' ) s  of
                    ("command", rest)  -> cont TCommand rest
                    ("var", rest)      -> cont TVar rest
                    ("if", rest)       -> cont TIf rest
                    ("else", rest)     -> cont TElse rest
                    ("while", rest)    -> cont TWhile rest
                    ("do", rest)       -> cont TDo rest
                    ("true", rest)     -> cont TTrue rest
                    ("false", rest)    -> cont TFalse rest
                    ("Number", rest)   -> cont TTNumber rest
                    ("String", rest)   -> cont TTString rest
                    ("Bool", rest)     -> cont TTBool rest
                    (var, rest)        -> cont (TIdentifier var) rest

lexString :: (Token -> P a) -> P a
lexString cont s =  case span (/='\"') s of
                      (string, rest)  ->  case tail' rest of
                                            Just xs ->  cont (TString string) xs
                                            Nothing ->  \line -> Failed $ "Línea " ++ show line ++ ": Falta caracter \'\"\'"
                    where tail' []      = Nothing
                          tail' (_:xs)  = Just xs

lexNumber :: (Token -> P a) -> P a
lexNumber cont s =  case span (\x -> isDigit x || x == '.') s of
                      (number, rest)  ->  if (length $ filter (=='.') number) <= 1
                                          then cont (TConst (read number)) rest
                                          else \line -> Failed $ "Línea " ++ show line ++ ": Número..."

parseCommand s = parse_command s 1
parseJSON s = parse_json s 1
parseRequest s = parse_request s 1
}
