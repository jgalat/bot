{
module Parser where

import Data.Char
import Map

import CommandAST
}

%monad { P } { pThen } { pReturn }

%name parse_command command
%name parse_json value
%name parse_request request
%name parse_configuration configuration

%tokentype { Token }
%lexer { lexer } { TEOF }

%token
    ':'         { TColon }
    ';'         { TSemiColon}
    ','         { TComma }
    '('         { TParenthesesOpen }
    ')'         { TParenthesesClose }
    '['         { TBracketsOpen }
    ']'         { TBracketsClose }
    '{'         { TBracesOpen }
    '}'         { TBracesClose }
    TRUE        { TTrue }
    FALSE       { TFalse }
    NULL        { TNull }
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
    '.'         { TDot }
    INDENT      { TIndent }
    DEDENT      { TDedent }
    STRING      { TString $$ }
    IDENTIFIER  { TIdentifier $$ }
    CONST       { TConst $$ }
    COMMAND     { TCommand }
    IF          { TIf }
    ELSE        { TElse }
    WHILE       { TWhile }
    DO          { TDo }
    FOR         { TFor }
    IN          { TIn }
    TYPENUMBER  { TTNumber }
    TYPESTRING  { TTString }
    TYPEBOOL    { TTBool }
    TYPEJSON    { TTJson }
    TYPEARRAY   { TTArray }

%left '='

%left '|'
%left '&'
%left '~'

%nonassoc '>' '<' '>=' '<=' '>>' '<<' '=='
%left '+' '-'
%left '*' '/'
%left NEG
%left '.'

%%

command :: { Comm }
        : COMMAND '(' parameters ')' ':' INDENT stmts DEDENT  { Comm $3 $7 }

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
        | TYPEJSON                                { JSON }
        | TYPEARRAY                               { ArrayType }

stmts   :: { [Statement] }
        : stmt stmts                              { $1 : $2 }
        | stmt ';' stmts                          { $1 : $3 }
        |                                         { [] }

stmt    :: { Statement }
        : IDENTIFIER '=' expr                                           { Assign $1 $3 }
        | IF expr ':' INDENT stmts DEDENT                               { If $2 $5 }
        | IF expr ':' INDENT stmts DEDENT ELSE ':' INDENT stmts DEDENT  { IfElse $2 $5 $10 }
        | WHILE expr ':' INDENT stmts DEDENT                            { While $2 $5 }
        | DO ':' INDENT stmts DEDENT WHILE expr                         { Do $4 $7 }
        | FOR IDENTIFIER IN expr ':' INDENT stmts DEDENT                { For $2 $4 $7 }

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
        | expr '.' expr                           { Index $1 $3 }
        | '<<' expr                               { Get $2 }
        | expr '>>' expr                          { Post $1 $3 }

value   :: { Expr }
        : json                                    { $1 }
        | NULL                                    { Null }
        | TRUE                                    { TrueExp }
        | FALSE                                   { FalseExp }
        | CONST                                   { Const $1 }
        | STRING                                  { Str $1 }

json    :: { Expr }
        : object                                  { JsonObject (mapFromList $1) }
        | array                                   { Array  $1 }

object  :: { [(String, Expr)] }
        : '{' '}'                                 { [] }
        | '{' pairs '}'                           { $2 }

pairs   :: { [(String, Expr)] }
        : pair                                    { [$1] }
        | pair ',' pairs                          { $1 : $3 }

pair    :: { (String, Expr) }
        : STRING ':' expr                         { ($1, $3) }

array   :: { [Expr] }
        : '[' ']'                                 { [] }
        | '[' values ']'                          { $2 }

values  :: { [Expr] }
        : expr                                    { [$1] }
        | expr ',' values                         { $1 : $3 }

request :: { (String, [Expr]) }
        : '/'IDENTIFIER arguments                 { ($2, $3) }

arguments :: { [Expr] }
          : value arguments                       { $1 : $2 }
          | IDENTIFIER arguments                  { Str $1 : $2}
          |                                       { [] }

configuration :: { Map String }
              : settings                          { mapFromList $1 }
              |                                   {% pFail "Configuration file is empty." }

settings  :: { [(String, String)] }
          : setting                               { [$1] }
          | setting settings                      { $1 : $2 }

setting :: { (String, String) }
        : IDENTIFIER ':' STRING                  { ($1, $3) }

{
data ParseResult a = Ok a | Failed String
                     deriving Show

data ParseState = ParseState  { line            :: Int,
                                indentSensitive :: Bool,
                                levelStack      :: [Int]
                              } -- deriving Show

initParseState :: ParseState
initParseState = ParseState { line = 1,
                              indentSensitive = True,
                              levelStack = [0]
                            }

incLine :: ParseState -> ParseState
incLine s = s { line = line s + 1 }

sensitivityOn :: ParseState -> ParseState
sensitivityOn s = s { indentSensitive = True }

sensitivityOff :: ParseState -> ParseState
sensitivityOff s = s { indentSensitive = False }

pushLevel :: Int -> ParseState -> ParseState
pushLevel n s = s { levelStack = n : levelStack s }

popLevel :: ParseState -> ParseState
popLevel s = s { levelStack = tail (levelStack s) }

indentLevel :: ParseState -> Int
indentLevel s = head (levelStack s)

type P a = String -> ParseState -> ParseResult a

pReturn :: a -> P a
pReturn x = \_ _ -> Ok x

pThen :: P a -> (a -> P b) -> P b
m `pThen` f  = \s st -> case m s st of
                          Ok a     -> f a s st
                          Failed e -> Failed e

pFail :: String -> P a
pFail err = \_ _ -> Failed err

happyError :: P a
happyError = \s st -> Failed $ "Line " ++ show (line st) ++ ": Error parsing\n" ++ (take 20 s) ++ "... \n " -- ++ (show st)

data Token  = TIdentifier Var
            | TConst Double
            | TString String
            | TCommand
            | TAssign
            | TIf
            | TElse
            | TWhile
            | TDo
            | TFor
            | TIn
            | TType
            | TTNumber
            | TTString
            | TTBool
            | TTArray
            | TTJson
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
            | TNull
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
            | TDot
            | TGet
            | TPost
            | TIndent
            | TDedent
            | TEOF
            deriving Show

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
  []                          -> \st -> case levelStack st of
                                          [_]     -> cont TEOF [] st
                                          (_:_)   -> cont TDedent [] (popLevel st)
  ('\n':cs)                   -> \st' ->  let st = incLine st'
                                              (indentation, input) = span (==' ') cs
                                              currIndent = length indentation
                                              lastIndent = indentLevel st
                                          in  case input of
                                                    ('-':('-': _)) -> lexer cont input st
                                                    ('\n':_)       -> lexer cont input st
                                                    _  -> case compare currIndent lastIndent of
                                                              GT -> if indentSensitive st
                                                                    then cont TIndent input (pushLevel currIndent st)
                                                                    else lexer cont input st
                                                              EQ -> lexer cont input (sensitivityOn st)
                                                              LT -> cont TDedent input (popLevel (sensitivityOn st))
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
  ('=':cs)                    -> cont TAssign cs . sensitivityOff
  ('~':cs)                    -> cont TNot cs
  ('&':cs)                    -> cont TAnd cs
  ('|':cs)                    -> cont TOr cs
  ('>':cs)                    -> cont TGreater cs
  ('<':cs)                    -> cont TLower cs
  ('+':cs)                    -> cont TPlus cs
  ('-':cs)                    -> cont TMinus cs
  ('*':cs)                    -> cont TAsterisc cs
  ('/':cs)                    -> cont TSlash cs
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
  unknown                     -> \st -> Failed $ "Line " ++ show (line st) ++ ": Unrecognized " ++ (show $ take 10 unknown)++ "..."

lexAlpha :: (Token -> P a) -> P a
lexAlpha cont s = case span (\c -> isAlpha c || isDigit c || c == '_' ) s  of
                    ("command", rest)  -> cont TCommand rest
                    ("if", rest)       -> cont TIf rest
                    ("else", rest)     -> cont TElse rest
                    ("while", rest)    -> cont TWhile rest
                    ("do", rest)       -> cont TDo rest
                    ("for", rest)      -> cont TFor rest
                    ("in", rest)       -> cont TIn rest
                    ("true", rest)     -> cont TTrue rest
                    ("false", rest)    -> cont TFalse rest
                    ("null", rest)     -> cont TNull rest
                    ("Number", rest)   -> cont TTNumber rest
                    ("String", rest)   -> cont TTString rest
                    ("Bool", rest)     -> cont TTBool rest
                    ("JSON", rest)     -> cont TTJson rest
                    ("Array", rest)    -> cont TTArray rest
                    (var, rest)        -> cont (TIdentifier var) rest

lexString :: (Token -> P a) -> P a
lexString cont s =  let (string, rest) = getString [] s
                    in case rest of
                        []        -> \st -> Failed $ "Line " ++ show (line st) ++ ": Error parsing string \"" ++ string ++ "\""
                        ('\"':xs) -> cont (TString string) xs
                    where getString str [] = ("", [])
                          getString str ('\"': xs) = (rev str, '\"':xs)
                          getString str ('\\':(x:xs)) = case x of
                                                          '\\' -> getString (x:str) xs
                                                          '\"' -> getString (x:str) xs
                                                          '\'' -> getString (x:str) xs
                                                          '/'  -> getString (x:str) xs
                                                          'n'  -> getString ('\n':str) xs
                                                          't'  -> getString ('\t':str) xs
                                                          'r'  -> getString ('\r':str) xs
                                                          _    -> getString ('\\':(x:str)) xs
                          getString str (x:xs) = getString (x:str) xs
                          rev []     = []
                          rev (x:xs) = rev' [x] xs
                          rev' s []     = s
                          rev' s (x:xs) = rev' (x:s) xs

lexNumber :: (Token -> P a) -> P a
lexNumber cont s =  case span (\x -> isDigit x || x == '.') s of
                      (number, rest)  ->  if (length $ filter (=='.') number) <= 1
                                          then cont (TConst (read number)) rest
                                          else \st -> Failed $ "Line " ++ show (line st) ++ ": Error parsing number \"" ++ number ++ "\""

parseCommand s = parse_command s initParseState
parseJSON s = parse_json s initParseState
parseRequest s = parse_request s initParseState
parseConfiguration s = parse_configuration s initParseState
}
