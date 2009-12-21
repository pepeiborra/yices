-- vim:sw=2:ts=2:expandtab:autoindent
{- |
   Module      :  Math.SMT.Yices.Syntax
   Copyright   :  (c) 2009 by Ki Yung Ahn
   License     :  BSD3

   Maintainer  :  Ahn, Ki Yung <kya@pdx.edu>
   Stability   :  provisional
   Portability :  portable

   Haskell data type definition for the yices syntax.  Yet incomplete since
   it does not include bit vectors.
   See <http://yices.csl.sri.com/language.shtml> for details.
 -}
module Math.SMT.Yices.Syntax ( TypY(..) , ExpY(..) , CmdY(..) ) where

import Char
import List
import Ratio

-- | yices types
data TypY
  = VarT String
  | SUBTYPE (String,TypY) ExpY
  | SUBRANGE ExpY ExpY
  | ARR [TypY]
  | TUP [TypY]
  | REC [(String,TypY)]
  | DEP (String,TypY)
  | DATATYPE [(String,[(String,TypY)])]
  | SCALAR [String]
  -- BITVECTOR Integer

-- | yices expressions
data ExpY
  = VarE String
  | LitB Bool
  | LitI Integer
  | LitR Rational
  | AND [ExpY]
  | OR [ExpY]
  | NOT ExpY
  | ExpY :=> ExpY
  | ExpY := ExpY
  | ExpY :/= ExpY
  | ExpY :< ExpY
  | ExpY :<= ExpY
  | ExpY :> ExpY
  | ExpY :>= ExpY
  | ExpY :+: ExpY
  | ExpY :-: ExpY
  | ExpY :*: ExpY
  | ExpY :/: ExpY
  | DIV ExpY ExpY
  | MOD ExpY ExpY
  | IF ExpY ExpY ExpY
  | ITE ExpY ExpY ExpY
  | LET [((String,Maybe TypY),ExpY)] ExpY
  -- quantifires
  | FORALL [(String,TypY)] ExpY
  | EXISTS [(String,TypY)] ExpY
  -- functions
  | APP ExpY [ExpY]
  | UPDATE_F ExpY [ExpY] ExpY
  | LAMBDA [(String,TypY)] ExpY
  -- tuples
  | MKTUP [ExpY]
  | SELECT_T ExpY Integer
  | UPDATE_T ExpY Integer ExpY
  -- records
  | MKREC [(String,ExpY)]
  | SELECT_R ExpY String
  | UPDATE_R ExpY String ExpY
  -- bitvectors -- TODO

-- | yices declarations and commands
data CmdY
  = DEFTYP String (Maybe TypY)
  | DEFINE (String,TypY) (Maybe ExpY)
  | ASSERT ExpY
  | ASSERT_P ExpY (Maybe Integer)
  | RETRACT Integer
  | CHECK
  | MAXSAT
  | SETE Bool
  | SETV Integer
  | SETAO Bool
  | PUSH
  | POP
  | ECHO String
  | INCLUDE String
  | RESET
  | STATUS
  | DUMP
  | EXIT

paren s = "("++s++")"

showListSepByWith showFun sep = concat . intersperse sep . map showFun

showListSepBy :: (Show a) => String -> [a] -> String
showListSepBy = showListSepByWith show

showStringsSepBy = showListSepByWith id

showIdTyp (tname,t) = tname++"::"++show t

showIdVal (fname,e) = fname++"::"++show e

showCtorDef (c,[])    = c
showCtorDef (c,idtyps) = paren $ c++" "++showListSepByWith showIdTyp " " idtyps

showBinding ((x,Just t),e) = paren $ showIdTyp(x,t) ++ " " ++ show e
showBinding ((x,Nothing),e) = paren $ x ++ " " ++ show e

instance Show TypY where
  show (VarT tname) = tname
  show (SUBTYPE idty e) = paren $ "subtype " ++ showIdTyp idty ++ " " ++ show e
  show (SUBRANGE e1 e2) = paren $ "subrange " ++ show e1 ++ " " ++ show e2
  show (ARR ts) = paren $ "-> " ++ showListSepBy " " ts
  show (TUP ts) = paren $ "tuple " ++ showListSepBy " " ts
  show (REC idtyps) = paren $ "record " ++ showListSepByWith showIdTyp " " idtyps
  show (DEP idty) = showIdTyp idty
  show (DATATYPE ctordefs) =
         paren $ "datatype " ++ showListSepByWith showCtorDef " " ctordefs
  show (SCALAR tnames) = paren $ "scalar " ++ showStringsSepBy " " tnames
  -- show (BITVECTOR n) = paren $ "bitvector " ++ show n

instance Show ExpY where
  show (VarE x) = x
  show (LitB True) = "true"
  show (LitB False) = "false"
  show (LitI n) = show n
  show (LitR r) = show (numerator r) ++ "/" ++ show (denominator r)
  show (AND es) = paren $ "and " ++ showListSepBy " " es
  show (OR es) = paren $ "or " ++ showListSepBy " " es
  show (NOT e) = paren $ "not " ++ show e
  show (e1 :=> e2) = paren $ "=> " ++ show e1 ++ " " ++ show e2
  show (e1 := e2) = paren $ "= " ++ show e1 ++ " " ++ show e2
  show (e1 :/= e2) = paren $ "/= " ++ show e1 ++ " " ++ show e2
  show (e1 :< e2) = paren $ "< " ++ show e1 ++ " " ++ show e2
  show (e1 :<= e2) = paren $ "<= " ++ show e1 ++ " " ++ show e2
  show (e1 :> e2) = paren $ "> " ++ show e1 ++ " " ++ show e2
  show (e1 :>= e2) = paren $ ">= " ++ show e1 ++ " " ++ show e2
  show (e1 :+: e2) = paren $ "+ " ++ show e1 ++ " " ++ show e2
  show (e1 :-: e2) = paren $ "- " ++ show e1 ++ " " ++ show e2
  show (e1 :*: e2) = paren $ "* " ++ show e1 ++ " " ++ show e2
  show (e1 :/: e2) = paren $ "/ " ++ show e1 ++ " " ++ show e2
  show (DIV e1 e2) = paren $ "div " ++ show e1 ++ " " ++ show e2
  show (MOD e1 e2) = paren $ "mod " ++ show e1 ++ " " ++ show e2
  show (IF eb et ef) = paren $ "if " ++ showListSepBy " " [eb,et,ef]
  show (ITE eb et ef) = paren $ "ite " ++ showListSepBy " " [eb,et,ef]
  show (LET bindings e) = paren $ "let " ++ (paren $ showListSepByWith showBinding " " bindings) ++ " "++ show e
  -- quantifires
  show (FORALL idtyps e) = paren $ "forall "
         ++ (paren $ showListSepByWith showIdTyp " " idtyps) ++ " " ++ show e
  show (EXISTS idtyps e) = paren $ "exists "
         ++ (paren $ showListSepByWith showIdTyp " " idtyps) ++ " " ++ show e
  -- functions
  show (APP e es) = paren $ showListSepBy " " (e:es)
  show (UPDATE_F e es v) = paren $ "update "
         ++ show e ++ " " ++ (paren $ showListSepBy " " es) ++ show v
  show (LAMBDA idtyps e) = paren $ "lambda "
         ++ (paren $ showListSepByWith showIdTyp " " idtyps) ++ show e
  -- tuples
  show (MKTUP es) = paren $ "mk-tuple " ++ showListSepBy " " es
  show (SELECT_T e i) = paren $ "select " ++ show e ++ " " ++ show i
  show (UPDATE_T e i v) = paren $ "update "
         ++ show e ++ " " ++ show i ++ " " ++ show v
  -- records
  show (MKREC idvals) = paren $ "mk-record "
         ++ showListSepByWith showIdVal " " idvals
  show (SELECT_R e s) = paren $ "select " ++ show e ++ " " ++ s
  show (UPDATE_R  e s v) = paren $ "update "
         ++ show e ++ " " ++ s ++ " " ++ show v
  -- bitvectors -- TODO

instance Show CmdY where
  show (DEFTYP tname Nothing) = paren $ "define-type " ++ tname
  show (DEFTYP tname (Just t)) = paren $ "define-type "++ tname ++ " " ++ show t
  show (DEFINE idty Nothing) = paren $ "define " ++ showIdTyp idty
  show (DEFINE idty (Just e)) = paren $ "define " ++ showIdTyp idty ++ show e
  show (ASSERT e) = paren $ "assert " ++ show e
  show (ASSERT_P e Nothing) = paren $ "assert+ " ++ show e
  show (ASSERT_P e (Just w)) = paren $ "assert+ " ++ show e ++ " " ++ show w
  show (RETRACT i) = paren $ "retract " ++ show i
  show (CHECK) = paren "check"
  show (MAXSAT) = paren "max-sat"
  show (SETE b) = paren $ "set-evidence! " ++ show (LitB b)
  show (SETV k) = paren $ "set-verbosity! " ++ show k
  show (SETAO b) = paren $ "set-arith-only! " ++ show (LitB b)
  show (PUSH) = paren "push"
  show (POP) = paren "pop"
  show (ECHO s) = paren $ "echo " ++ show s -- not exact since Haskell string
  show (INCLUDE s) = paren $ "include " ++ show s -- not exact same reason
  show (RESET) = paren "reset"
  show (STATUS) = paren "status"
  show (DUMP) = paren "dump-context"
  show (EXIT) = paren "exit"

