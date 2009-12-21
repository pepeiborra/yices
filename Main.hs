-- vim:sw=2:ts=2:expandtab:autoindent

module Main where 

-- example of generating three test cases of 6 integer variables
-- l1, u1, i, l2, u2, j sush that l1 < i <= u1 and l2 < j <= u2.

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Pipe
import List
import Monad
-- import Random

yicesPath = "/home/kyagrd/ames/yices-1.0.21/bin/yices" -- your yices path

main =
  do yp@(Just hin, Just hout, Nothing, p) <- createYicesPipe yicesPath []
     runCmdsY yp (defs ++ ctrs)

     -- gr <- getStdGen
     -- let (rn,gr') = next gr

     Sat ss <- checkY yp
     print ss

     runCmdsY yp [ASSERT_P (NOT $ ss!!0) Nothing]
     Sat ss <- checkY yp
     print ss

     runCmdsY yp [ASSERT_P (NOT $ ss!!0) Nothing]
     Sat ss <- checkY yp
     print ss

     exitY yp


defs = map (\x -> DEFINE (x,int) Nothing) ["l1","u1","i","l2","u2","j"]

ctrs = map ASSERT [ l1:<u1, l1:<=i, i:<=u1, l2:<u2, l2:<=j, j:<=u2 ]
     -- ++ map (\e -> ASSERT_P e Nothing) [ i:<j, j:<i ]
     where
       l1 = VarE "l1"
       u1 = VarE "u1"
       i = VarE "i"
       l2 = VarE "l2"
       u2 = VarE "u2"
       j = VarE "j"

int = VarT "int"
nat = VarT "nat"
bool = VarT "bool"
real = VarT "real"

true = LitB True
false = LitB False

