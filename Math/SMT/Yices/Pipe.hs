-- vim:sw=2:ts=2:expandtab:autoindent

{- |
   Module      :  Math.SMT.Yices.Pipe
   Copyright   :  (c) 2009 by Ki Yung Ahn
   License     :  BSD3

   Maintainer  :  Ahn, Ki Yung <kya@pdx.edu>
   Stability   :  provisional
   Portability :  portable

   Inter-process communication to Yices through pipe.
 -}
module Math.SMT.Yices.Pipe (
  YicesIPC, ResY(..), createYicesPipe,
  runCmdsY', runCmdsY, checkY, exitY, flushY
 ) where

import Math.SMT.Yices.Syntax
import Math.SMT.Yices.Parser
import List
import Monad
import System.IO
import System.Process

-- | type abbrevation for IPC handle quadruple
type YicesIPC = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | To read in the result of the (check) command
data ResY
  = Sat [ExpY]
  | Unknown [ExpY]
  | UnSat [Integer]
  | InCon [String]
 deriving Show

-- | Start yices on a given path with given options.
-- The first argumnet yPath is the path binary file of yices
-- (e.g. /home/kyagrd/yices-1.0.21/bin/yices).
-- By default -i and -e options are already present, and
-- yOpts argument appens more options
createYicesPipe :: FilePath -> [String] -> IO YicesIPC
createYicesPipe yPath yOpts = createProcess $
      (proc yPath $ "-i":"-e":yOpts){std_in=CreatePipe,std_out=CreatePipe}

_BEGIN_OUTPUT = "_![<[BEGIN_OUTPUT]>]!_"
_END_OUTPUT = "_![<[END_OUTPUT]>]!_"

runCmdStringY yp s = runCmdStringY' yp s >> flushY yp

runCmdStringY' (Just hin, _, _, _) = hPutStrLn hin

-- | send yices commands and flush
runCmdsY :: YicesIPC -> [CmdY] -> IO ()
runCmdsY yp cmds = runCmdsY' yp cmds >> flushY yp
 
-- | send yices commands without flushing
runCmdsY' :: YicesIPC -> [CmdY] -> IO ()
runCmdsY' (Just hin, _, _, _) = mapM_ (hPutStrLn hin . show)

-- | send exit command and flush
exitY :: YicesIPC -> IO ()
exitY (Just hin, _, _, _) = hPutStrLn hin (show EXIT) >> hFlush hin

-- | flush the input pipe to yices (needed after actions like runCmdsY')
flushY :: YicesIPC -> IO ()
flushY (Just hin, _, _, _) = hFlush hin

-- | send check command and reads the result
checkY :: YicesIPC -> IO ResY
checkY yp@(_, Just hout, _, _) =
  do runCmdsY yp [ECHO('\n':_BEGIN_OUTPUT), CHECK, ECHO('\n':_END_OUTPUT)]
     (s:ss) <- hGetOutLines hout
     return $
       case s of
         "sat"    -> Sat (map parseExpY ss)
         "unknown"-> Unknown (map parseExpY ss)
         "unsat" -> UnSat (map read.words.tail.dropWhile (/=':').head $ ss)
         _       -> InCon (s:ss)

stripYicesPrompt line | yprompt `isPrefixOf` line = drop (length yprompt) line
                      | otherwise                 = line
                      where yprompt = "yices > "

hGetOutLines h = liftM ( filter (not . null) . map stripYicesPrompt .
                         tail . dropWhile (/=_BEGIN_OUTPUT) )
                       (hGetLinesWhile (/= _END_OUTPUT) h)

hGetLinesWhile p h = do line <- hGetLine h
                        if p line
                           then liftM (line:) (hGetLinesWhile p h)
                           else return []

{-
hGetReadyString1 h = liftM2 (:) (hGetChar h) (hGetReadyString h)

hGetReadyString h =
  do ready <- hReady h
     if ready then liftM2 (:) (hGetChar h) (hGetReadyString h) else return []
-}
