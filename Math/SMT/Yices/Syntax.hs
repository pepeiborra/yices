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
import Data.ByteString.Lazy.Char8 (unpack)
import List
import Ratio
import Text.Show.ByteString
import Prelude hiding (Show(..))
import qualified Prelude

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

instance Prelude.Show TypY where show = unpack . show
instance Prelude.Show ExpY where show = unpack . show
instance Prelude.Show CmdY where show = unpack . show

paren = showpParen True
space = putAscii ' '

showListSepByWith showFun sep = ignore . sequence . intersperse (putAscii sep) . map showFun
  where
    ignore m = m >> return ()

showListSepBy :: (Show a) => Char -> [a] -> Put
showListSepBy = showListSepByWith showp

showStringsSepBy = showListSepByWith putAsciiStr

showIdTyp (tname,t) = putAsciiStr tname >> putAsciiStr "::" >> showp t

showIdVal (fname,e) = putAsciiStr fname >> putAsciiStr "::" >> showp e

showCtorDef (c,[])    = putAsciiStr c
showCtorDef (c,idtyps) = paren $ putAsciiStr c >> space >> showListSepByWith showIdTyp ' ' idtyps

showBinding ((x,Just t),e) = paren $ showIdTyp(x,t) >> space >> showp e
showBinding ((x,Nothing),e) = paren $ putAsciiStr x >> space >> showp e

instance Show TypY where
  showp = showPTyp

showPTyp(VarT tname) = putAsciiStr tname
showPTyp(SUBTYPE idty e) = paren $ putAsciiStr "subtype " >> showIdTyp idty >> space >> showp e
showPTyp(SUBRANGE e1 e2) = paren $ putAsciiStr "subrange " >> showp e1 >> space >> showp e2
showPTyp(ARR ts) = paren $ putAsciiStr "-> " >> showListSepBy ' ' ts
showPTyp(TUP ts) = paren $ putAsciiStr "tuple " >> showListSepBy ' ' ts
showPTyp(REC idtyps) = paren $ putAsciiStr "record " >> showListSepByWith showIdTyp ' ' idtyps
showPTyp(DEP idty) = showIdTyp idty
showPTyp(DATATYPE ctordefs) =
         paren $ putAsciiStr "datatype " >> showListSepByWith showCtorDef ' ' ctordefs
showPTyp(SCALAR tnames) = paren $ putAsciiStr "scalar " >> showStringsSepBy ' ' tnames
  -- showp (BITVECTOR n) = paren $ "bitvector " ++ show n

instance Show ExpY where
  showp = showPExp

showPExp(VarE x) = putAsciiStr x
showPExp(LitB True) = putAsciiStr "true"
showPExp(LitB False) = putAsciiStr "false"
showPExp(LitI n) = showp n
showPExp(LitR r) = showp (numerator r) >> putAscii '/' >> showp (denominator r)
showPExp(AND es) = paren $ putAsciiStr "and " >> showListSepBy ' ' es
showPExp(OR es) = paren $ putAsciiStr "or " >> showListSepBy ' ' es
showPExp(NOT e) = paren $putAsciiStr "not " >> showp e
showPExp(e1 :=> e2) = paren $putAsciiStr "=> " >> showp e1 >> space >> showp e2
showPExp(e1 := e2) = paren $ putAsciiStr "= " >> showp e1 >> space >> showp e2
showPExp(e1 :/= e2) = paren $ putAsciiStr "/= " >> showp e1 >> space >> showp e2
showPExp(e1 :< e2) = paren $ putAsciiStr "< " >> showp e1 >> space >> showp e2
showPExp(e1 :<= e2) = paren $ putAsciiStr "<= " >> showp e1 >> space >> showp e2
showPExp(e1 :> e2) = paren $ putAsciiStr "> " >> showp e1 >> space >> showp e2
showPExp(e1 :>= e2) = paren $ putAsciiStr ">= " >> showp e1 >> space >> showp e2
showPExp(e1 :+: e2) = paren $ putAsciiStr "+ " >> showp e1 >> space >> showp e2
showPExp(e1 :-: e2) = paren $ putAsciiStr "- " >> showp e1 >> space >> showp e2
showPExp(e1 :*: e2) = paren $ putAsciiStr "* " >> showp e1 >> space >> showp e2
showPExp(e1 :/: e2) = paren $ putAsciiStr "/ " >> showp e1 >> space >> showp e2
showPExp(DIV e1 e2) = paren $ putAsciiStr "div " >> showp e1 >> space >> showp e2
showPExp(MOD e1 e2) = paren $ putAsciiStr "mod " >> showp e1 >> space >> showp e2
showPExp(IF eb et ef) = paren $ putAsciiStr "if " >> showListSepBy ' ' [eb,et,ef]
showPExp(ITE eb et ef) = paren $ putAsciiStr "ite " >> showListSepBy ' ' [eb,et,ef]
showPExp(LET bindings e) = paren $ putAsciiStr "let " >> (paren $ showListSepByWith showBinding ' ' bindings) >> space >> showp e
  -- quantifires
showPExp(FORALL idtyps e) = paren $ putAsciiStr "forall "
         >> (paren $ showListSepByWith showIdTyp ' ' idtyps) >> space >> showp e
showPExp(EXISTS idtyps e) = paren $ putAsciiStr "exists "
         >> (paren $ showListSepByWith showIdTyp ' ' idtyps) >> space >> showp e
  -- functions
showPExp(APP e es) = paren $ showListSepBy ' ' (e:es)
showPExp(UPDATE_F e es v) = paren $ putAsciiStr "update "
         >> showp e >> space >> (paren $ showListSepBy ' ' es) >> showp v
showPExp(LAMBDA idtyps e) = paren $ putAsciiStr "lambda "
         >> (paren $ showListSepByWith showIdTyp ' ' idtyps) >> showp e
  -- tuples
showPExp(MKTUP es) = paren $ putAsciiStr "mk-tuple " >> showListSepBy ' ' es
showPExp(SELECT_T e i) = paren $ putAsciiStr "select " >> showp e >> space >> showp i
showPExp(UPDATE_T e i v) = paren $ putAsciiStr "update "
         >> showp e >> space >> showp i >> space >> showp v
  -- records
showPExp(MKREC idvals) = paren $ putAsciiStr "mk-record "
         >> showListSepByWith showIdVal ' ' idvals
showPExp(SELECT_R e s) = paren $ putAsciiStr "select " >> showp e >> space >> putAsciiStr s
showPExp(UPDATE_R  e s v) = paren $ putAsciiStr "update "
         >> showp e >> space >> putAsciiStr s >> space >> showp v
  -- bitvectors -- TODO

instance Show CmdY where
  showp = showPCmd

showPCmd(DEFTYP tname Nothing) = paren $ putAsciiStr "define-type " >> putAsciiStr tname
showPCmd(DEFTYP tname (Just t)) = paren $ putAsciiStr "define-type ">> putAsciiStr tname >> space >> showp t
showPCmd(DEFINE idty Nothing) = paren $ putAsciiStr "define " >> showIdTyp idty
showPCmd(DEFINE idty (Just e)) = paren $ putAsciiStr "define " >> showIdTyp idty >> space >> showp e
showPCmd(ASSERT e) = paren $ putAsciiStr "assert " >> showp e
showPCmd(ASSERT_P e Nothing) = paren $ putAsciiStr "assert+ " >> showp e
showPCmd(ASSERT_P e (Just w)) = paren $ putAsciiStr "assert+ " >> showp e >> space >> showp w
showPCmd(RETRACT i) = paren $ putAsciiStr "retract " >> showp i
showPCmd(CHECK) = paren $ putAsciiStr "check"
showPCmd(MAXSAT) = paren $ putAsciiStr "max-sat"
showPCmd(SETE b) = paren $ putAsciiStr "set-evidence! " >> showp (LitB b)
showPCmd(SETV k) = paren $ putAsciiStr "set-verbosity! " >> showp k
showPCmd(SETAO b) = paren $ putAsciiStr "set-arith-only! " >> showp (LitB b)
showPCmd(PUSH) = paren $ putAsciiStr "push"
showPCmd(POP) = paren $ putAsciiStr "pop"
showPCmd(ECHO s) = paren $ putAsciiStr "echo " >> showp s -- not exact since Haskell string
showPCmd(INCLUDE s) = paren $ putAsciiStr "include " >> showp s -- not exact same reason
showPCmd(RESET) = paren $ putAsciiStr "reset"
showPCmd(STATUS) = paren $ putAsciiStr "status"
showPCmd(DUMP) = paren $ putAsciiStr "dump-context"
showPCmd(EXIT) = paren $ putAsciiStr "exit"

