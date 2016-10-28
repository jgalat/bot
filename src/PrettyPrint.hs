module PrettyPrint where

  import Map
  import CommandAST

  import Text.PrettyPrint.HughesPJ hiding (Str)

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

  showExprJSONValid :: Expr -> String
  showExprJSONValid Null           = "null"
  showExprJSONValid (Const n)      = showConst n
  showExprJSONValid TrueExp        = "true"
  showExprJSONValid FalseExp       = "false"
  showExprJSONValid (Str s)        = '\"' : (s ++ "\"")
  showExprJSONValid (JsonObject o) = showJsonObjectJSONValid (mapToList o)
  showExprJSONValid (Array a)      = showArrayJSONValid a
  showExprJSONValid _ = "WTF!" -- Shouldn't happen ever

  showJsonObjectJSONValid :: [(String, Expr)] -> String
  showJsonObjectJSONValid [] = "{}"
  showJsonObjectJSONValid xs = '{' : sho xs
                                where sho [(k,e)]     = ("\"" ++ (k ++ "\" : ")) ++ showExprJSONValid e ++ "}"
                                      sho ((k,e) : es)= ("\"" ++ (k ++ "\" : ")) ++ showExprJSONValid e ++ ", " ++ sho es

  showArrayJSONValid :: [Expr] -> String
  showArrayJSONValid []  = "[]"
  showArrayJSONValid xs  = "\\[" ++ sa xs
                            where sa [e]    = showExprJSONValid e ++ "]"
                                  sa (e:es) = showExprJSONValid e ++ ", " ++ sa es

--
-- Pretty Print
--

  precOr, precAnd, precNot, precOpsComp, precPlusMinus, precMulDiv, precNeg :: Int
  precOr = 1
  precAnd = 2
  precNot = 3
  precOpsComp = 4
  precPlusMinus = 5
  precMulDiv = 6
  precNeg = 7
  precIndex = 8

  ppExpr :: Expr -> Doc
  ppExpr = ppExpr' 0

  ppExpr' :: Int -> Expr -> Doc
  ppExpr' _ Null          = text "null"
  ppExpr' _ TrueExp       = text "true"
  ppExpr' _ FalseExp      = text "false"
  ppExpr' _ (Var v)       = text v
  ppExpr' _ (Const n)     = text (showConst n)
  ppExpr' _ (Str s)       = text (show s)
  ppExpr' p (Not e)       = maybeParens (p > precNot) $
                            text "~" <> ppExpr' precNot e
  ppExpr' p (And e1 e2)   = maybeParens (p > precAnd) $
                            ppExpr' precAnd e1 <+> text "&" <+> ppExpr' precAnd e2
  ppExpr' p (Or e1 e2)    = maybeParens (p > precOr) $
                            ppExpr' precOr e1 <+> text "|" <+> ppExpr' precOr e2
  ppExpr' p (Equals e1 e2)= maybeParens (p > precOpsComp) $
                            ppExpr' precOpsComp e1 <+> text "==" <+> ppExpr' precOpsComp e2
  ppExpr' p (Greater e1 e2) = maybeParens (p > precOpsComp) $
                              ppExpr' precOpsComp e1 <+> text ">" <+> ppExpr' precOpsComp e2
  ppExpr' p (Lower e1 e2) = maybeParens (p > precOpsComp) $
                            ppExpr' precOpsComp e1 <+> text "<" <+> ppExpr' precOpsComp e2
  ppExpr' p (GreaterEquals e1 e2) = maybeParens (p > precOpsComp) $
                                    ppExpr' precOpsComp e1 <+> text ">=" <+> ppExpr' precOpsComp e2
  ppExpr' p (LowerEquals e1 e2) = maybeParens (p > precOpsComp) $
                                  ppExpr' precOpsComp e1 <+> text "<=" <+> ppExpr' precOpsComp e2
  ppExpr' p (Negate e)    = maybeParens (p > precNeg) $
                            text "-" <>  ppExpr' precNeg e
  ppExpr' p (Plus e1 e2)  = maybeParens (p > precPlusMinus) $
                            ppExpr' precPlusMinus e1 <+> text "+" <+> ppExpr' precPlusMinus e2
  ppExpr' p (Minus e1 e2) = maybeParens (p > precPlusMinus) $
                            ppExpr' precPlusMinus e1 <+> text "-" <+> ppExpr' precPlusMinus e2
  ppExpr' p (Multiply e1 e2)= maybeParens (p > precMulDiv) $
                              ppExpr' precMulDiv e1 <+> text "*" <+> ppExpr' precMulDiv e2
  ppExpr' p (Divide e1 e2)  = maybeParens (p > precMulDiv) $
                              ppExpr' precMulDiv e1 <+> text "/" <+> ppExpr' precMulDiv e2
  ppExpr' p (Index e1 e2) = maybeParens (p > precIndex) $
                            ppExpr' precIndex e1 <> text "." <> ppExpr' precIndex e2
  ppExpr' p (Get e)       = maybeParens (p > precOpsComp) $
                            text "<<" <+> ppExpr' precOpsComp e
  ppExpr' p (Post e1 e2)  = maybeParens (p > precOpsComp) $
                            ppExpr' precOpsComp e1 <+> text ">>" <+> ppExpr' precOpsComp e2
  ppExpr' _ (JsonObject o)= case mapToList o of
                              []  -> lbrace <> rbrace
                              kes -> vcat [ lbrace
                                          , ppKeyExprs kes
                                          , rbrace
                                          ]
  ppExpr' _ (Array a)     = lbrack <> hsep (punctuate comma (map ppExpr a)) <> rbrack

  ppKeyExprs :: [(String, Expr)] -> Doc
  ppKeyExprs kes = nest 2 (vcat (punctuate comma (map ppKeyExpr kes)))

  ppKeyExpr :: (String, Expr) -> Doc
  ppKeyExpr (s, e) = ppExpr (Str s) <+> colon <+> ppExpr e

  ppStatements :: [Statement] -> Doc
  ppStatements ss = nest 2 (vcat (map ppStatement ss))

  ppStatement :: Statement -> Doc
  ppStatement (Assign v e) = text v <+> equals <+> ppExpr e
  ppStatement (If e ss)    = vcat [ text "if" <+> ppExpr e <+> colon
                                  , ppStatements ss
                                  ]
  ppStatement (IfElse e ss1 ss2) = vcat [ text "if" <+> ppExpr e <+> colon
                                        , ppStatements ss1
                                        , text "else:"
                                        , ppStatements ss2
                                        ]
  ppStatement (While e ss) = vcat [ text "while" <+> ppExpr e <+> colon
                                  , ppStatements ss
                                  ]
  ppStatement (Do ss e)    = vcat [ text "do:"
                                  , ppStatements ss
                                  , text "while" <+> ppExpr e
                                  ]
  ppStatement (For v e ss) = vcat [ text "for" <+> text v <+> text "in" <+> ppExpr e <+> colon
                                  , ppStatements ss
                                  ]

  ppType :: Type -> Doc
  ppType ArrayType  = text "Array"
  ppType Number     = text "Number"
  ppType String     = text "String"
  ppType Bool       = text "Bool"
  ppType JSON       = text "JSON"

  ppArgs :: [(Var, Type)] -> Doc
  ppArgs vts = parens (hsep (punctuate comma (map ppArg vts)))

  ppArg :: (Var, Type) -> Doc
  ppArg (v, t) = ppType t <+> text v

  ppComm :: Comm -> Doc
  ppComm (Comm vts ss) = vcat [ text "command" <+> ppArgs vts <+> colon
                              , ppStatements ss
                              ]

  pp :: Comm -> String
  pp = render . ppComm
