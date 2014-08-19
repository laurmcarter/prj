{-# LANGUAGE GADTs #-}

module Robot where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Monoid
import System.Random

-- Data {{{

type Name = String

data BoolE
  = Lit Bool
  | Var Name
  | (:||:) BoolE BoolE
  | (:&&:) BoolE BoolE
  | Not BoolE
  deriving (Eq,Show)

data Program a where
  Move      :: Program ()
  TurnLeft  :: Program ()
  TurnRight :: Program ()
  Sensor    :: Program BoolE
  Cond      :: Program BoolE
               -> Program ()
               -> Program ()
               -> Program ()
  While     :: Program BoolE
               -> Program ()
               -> Program ()
  Return    :: a -> Program a
  Bind      :: Program a
               -> (a -> Program b)
               -> Program b

instance Monad Program where
  return = Return
  (>>=)  = Bind

data Prg
  = PMove
  | PTurnLeft
  | PTurnRight
  | PSensor Name
  | PCond BoolE Prg Prg
  | PWhile Name Prg Prg
  | PSeq Prg Prg
  | PSkip
  | PAssign Name BoolE
  deriving (Eq,Show)

true = Lit True
false = Lit False

sensor = Sensor
turnLeft = TurnLeft
turnRight = TurnRight
move = Move
while = While
cond = Cond

-- }}}

-- Render {{{

display :: Prg -> IO ()
display = putStrLn . render

render :: Prg -> String
render prg = case prg of
  PMove           -> "Move"
  PTurnLeft       -> "Turn Left"
  PTurnRight      -> "Turn Right"
  PSensor nm      -> nm ++ " = Sensor"
  PCond b p1 p2   -> unlines'
                       [ "if " ++ renderB b ++ " {"
                       , indent $ render p1
                       , "} else {"
                       , indent $ render p2
                       , "}"
                       ]
  PWhile x loop p -> unlines'
                       [ "while ("
                       , indent $ render loop
                       , ") {"
                       , indent $ render p
                       , "}"
                       ]
  PSeq p1 p2      -> unlines'
                       [ render p1
                       , render p2
                       ]
  PSkip           -> ""
  PAssign x b     -> x ++ " = " ++ renderB b
  where
  indent = intercalate "\n" . map ("  " ++) . lines
  unlines' = intercalate "\n" . filter (any (not . isSpace))

renderB :: BoolE -> String
renderB bl = case bl of
  Lit b        -> show b
  Var x        -> x
  be1 :||: be2 -> concat [ "(" , renderB be1 , " || " , renderB be2 ]
  be1 :&&: be2 -> concat [ "(" , renderB be1 , " && " , renderB be2 ]
  Not be       -> "not " ++ renderB be

-- }}}

-- Compile {{{

compile :: Program a -> M a
compile Move      = tell PMove
compile TurnLeft  = tell PTurnLeft
compile TurnRight = tell PTurnRight
compile Sensor    = do
  x <- newName
  tell $ PSensor x
  return $ Var x
compile (Cond b p1 p2) = do
  p1' <- ignore $ hear $ compile p1
  p2' <- ignore $ hear $ compile p2
  b' <- compile b
  tell $ PCond b' p1' p2'
compile (While wp act) = do
  x <- newName
  (b, wp') <- ignore $ listen $ compile wp
  act'     <- ignore $ hear $ compile act
  let wact = wp' <> PAssign x b
  tell $ PWhile x wact act'
compile (Return a) = return a
compile (Bind pa f) = do
  a <- compile pa
  compile $ f a

runCompile :: Program a -> Prg
runCompile = flip evalState 0 . execWriterT . compile

-- }}}

spiralIn :: Int -> Program ()
spiralIn 0 = return ()
spiralIn n = do
  replicateM_ 2 $ do
    replicateM_ n move
    turnLeft
  spiralIn (n - 1)

followWall :: Program ()
followWall =
  while (return true) $
    cond checkLeft
      move $
      do turnLeft
         move

checkLeft :: Program BoolE
checkLeft = do
  turnLeft
  s <- sensor
  turnRight
  return s

-- Monad {{{

type M = WriterT Prg (State Int)

instance Monoid Prg where
  mempty = PSkip
  mappend PSkip p = p
  mappend p PSkip = p
  mappend p1 p2   = p1 `PSeq` p2

newName :: M Name
newName = do
  s <- lift $ gets show
  lift $ modify (+1)
  return $ "v" ++ s

ignore :: (Monad m, Monoid w) => WriterT w m a -> WriterT w m a
ignore = censor $ const mempty

hear :: (Monad m, Monoid w) => WriterT w m a -> WriterT w m w
hear m = do
  (_,w) <- listen m
  return w

-- }}}

