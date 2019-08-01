module Translator ( translate
                  ) where

import Text.Parsec (ParseError)
import Data.List (intercalate)

import Expressions

translate :: Either ParseError Expr -> Maybe String
translate (Left _) = Nothing
translate (Right e) = Just $ between cppStartCode cppEndCode (toCpp e)

cppStartCode :: String
cppStartCode = unlines [ "#include <iostream>"
                       , "#include <map>"
                       , "#include <string>"
                       , "auto main() -> int"
                       , "{"]

cppEndCode = "\n}"

toCpp :: Expr -> String
toCpp (StringLit x)                          = stringLit x
toCpp (NumLit x)                             = show x
toCpp (Var x)                                = x
toCpp (Parens x)                             = parens (toCpp x)
toCpp (VarDecl x)                            = "auto " ++ x
toCpp (RetStatement x)                       = "return " ++ toCpp x
toCpp (FuncApp name params)                  = funcApp name params
toCpp (FuncDef name params body)             = funcDef name params body
toCpp (ModuleDef name exports body)          = moduleDef name exports body
toCpp (IfElseStatement cond ifBody elseBody) = ifElseStatement cond ifBody elseBody
toCpp (WhileStatement cond body)             = whileStatement cond body

funcApp :: String -> [Expr] -> String
funcApp "+"  params = op "+"        params
funcApp "-"  params = op "-"        params
funcApp "*"  params = op "*"        params
funcApp "/"  params = op "/"        params
funcApp "<"  params = op "<"        params
funcApp ">"  params = op ">"        params
funcApp "==" params = op "=="       params
funcApp "&&" params = op "&&"       params
funcApp "||" params = op "||"       params
funcApp "="  params = op "="        params
funcApp name params = funcHead name params

op :: String -> [Expr] -> String
op name (p:ps) = toCpp p ++ spaces name ++ concatMap toCpp ps

whileStatement :: Expr -> [Expr] -> String
whileStatement cond body =  "while "
                         ++ parens (toCpp cond)
                         ++ brackets (concatMap ((++ ";").toCpp) body)

ifElseStatement :: Expr -> [Expr] -> Maybe [Expr] -> String
ifElseStatement cond ifBody (Just elseBody) = ifStatement cond ifBody ++ elseStatement elseBody
ifElseStatement cond ifBody Nothing         = ifStatement cond ifBody

ifStatement :: Expr -> [Expr] -> String
ifStatement cond body = "if "
                      ++ parens (toCpp cond)
                      ++ brackets (concatMap ((++ ";").toCpp) body)

elseStatement :: [Expr] -> String
elseStatement body =  "else "
                   ++ brackets (concatMap ((++ ";").toCpp) body)

moduleDef :: String -> [Expr] -> [Expr] -> String
moduleDef name exports body = mHead ++ concatMap ((++ ";").toCpp) body ++ mFoot
    where
        mHead = " " --"#ifndef " ++ name ++ "_H\n" ++ "#define " ++ name ++ "_H\n"
        mFoot = " " --"\n#endif"

funcDef :: String -> [Expr] -> [Expr] -> String
funcDef name params body =  "auto " ++ name ++ " = [=]"
                         ++ parens (intercalate "," (map (("auto " ++).toCpp) params))
                         ++ funcBody body

funcHead :: String -> [Expr] -> String
funcHead name params = name ++ parens (intercalate "," (map toCpp params))

funcBody :: [Expr] -> String
funcBody exprs = brackets $ concatMap (++ ";") (map toCpp exprs)

between :: String -> String -> String -> String
between open close s = open ++ s ++ close

parens :: String -> String
parens = between "(" ")"

quotes :: String -> String
quotes = between "\"" "\""

brackets :: String -> String
brackets = between "{" "}"

spaces :: String -> String
spaces = between " " " "

stringLit :: String -> String
stringLit = between "\"" "\"s"
