{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Interpreter.Dependent.Initial where

import Control.Applicative
import Control.Lens hiding (Context,index)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.Exts (IsString(..))
import Data.Char
import Data.List (find)
import Text.Read hiding (lift,get)
import Control.Exception (AsyncException(UserInterrupt))
import System.Console.Haskeline
import System.Console.Haskeline.MonadException

import Debug.Trace

class Pretty t where
  prettyPrec :: Int -> t -> ShowS

pretty :: Pretty t => t -> String
pretty t = prettyPrec 0 t ""

data TermInf
  = TermChk ::: TermChk
  | Star
  | Pi TermChk TermChk
  | Bound Int
  | Free Name
  | TermInf :@: TermChk
  deriving (Eq,Show)
infixl 0 :::
infixl 0 :@:

instance Pretty TermInf where
  prettyPrec d = \case
    e ::: t  -> showParen (d > 10)
      $ prettyPrec 0 e
      . showString " :: "
      . prettyPrec 0 t
    Star     -> showString "*"
    Pi p p'  -> showParen (d > 10)
      $ showString "Π "
      . prettyPrec 11 p
      . showString ". "
      . prettyPrec 0 p'
    Bound i  -> shows i
    Free x   -> prettyPrec 0 x
    e :@: e' -> showParen (d > 10)
      $ prettyPrec 0 e
      . showString " "
      . prettyPrec 11 e'

instance IsString TermInf where
  fromString = Free . fromString

instance Read TermInf where
  readsPrec d r = 
       readParen (d > 10)
       (\r ->
         [ (e ::: typ,u)
         | (e,s) <- readsPrec 11 r
         , ("::",t) <- lex s
         , (typ,u) <- reads t
         ]) r
    ++ readParen False
       (\r ->
         [ (Star,s)
         | ("*",s) <- lex r
         ]) r
    ++ readParen (d > 10)
       (\r ->
         [ (Pi p p',v)
         | ("Π",s) <- lex r
         , (p,t)   <- readsPrec 11 s
         , (".",u) <- lex t
         , (p',v)  <- reads u
         ]) r
    ++ readParen (d > 10)
       (\r ->
         [ (Pi p p',u)
         | (p,s)    <- readsPrec 11 r
         , ("->",t) <- lex s
         , (p',u)   <- reads t
         ]) r
    ++ readParen False
       (\r ->
         [ (Bound i,s)
         | (i,s) <- reads r
         ]) r
    ++ readParen False
       (\r ->
         [ (Free x,s)
         | null $ (reads :: ReadS Int) r
         , (x,s) <- reads r
         ]) r
    ++ readParen (d > 10)
       (\r ->
         [ (e :@: e',t)
         | (e,s)  <- readsPrec 11 r
         , (e',t) <- reads s
         ]) r

readInf :: ReadS TermInf
readInf = reads

data TermChk
  = Inf TermInf
  | Lam TermChk
  deriving (Eq,Show)

instance Pretty TermChk where
  prettyPrec d = \case
    Inf e -> prettyPrec d e
    Lam e -> showParen (d > 10)
      $ showString "λ "
      . prettyPrec 11 e

instance Read TermChk where
  readsPrec d r =
       readParen (d > 10)
       (\r ->
         [ (Lam e,t)
         | ("λ",s) <- lex r
         , null $ (readsPrec d :: ReadS TermInf) r
         , (e,t)   <- readsPrec 11 s
         ]) r
    ++ [ (Inf e,s)
       | (e,s) <- readsPrec d r
       ]

readChk :: ReadS TermChk
readChk = reads

instance IsString TermChk where
  fromString = Inf . fromString

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Eq,Show)

instance Pretty Name where
  prettyPrec d = \case
    Global x -> showString x
    Local  i -> shows i
    Quote  i -> shows i

instance IsString Name where
  fromString = Global

instance Read Name where
  readsPrec d = readParen False
    $ \r ->
       [ (Global x,s)
       | (x,s) <- lex r
       , isIdent x
       ]
    ++ [ (Local i,s)
       | (i,s) <- reads r
       ]

readName :: ReadS Name
readName = reads

reserved :: String -> Bool
reserved = flip elem
  [ "λ"
  , "::"
  , "Π"
  , "->"
  ]

isIdent :: String -> Bool
isIdent s =
     not (reserved s)
  && not (null s)
  && isLower
     (head s)
  && all ((||) <$> isAlphaNum <*> (`elem` "'_"))
     (tail s)

type Type = Value

data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VNeutral Neutral

instance Pretty Value where
  prettyPrec d = prettyPrec d . quote0

instance IsString Value where
  fromString = VNeutral . fromString

data Neutral
  = NFree Name
  | NApp Neutral Value

instance IsString Neutral where
  fromString = NFree . fromString

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

-- Evaluation {{{

{-
type Env = [Value]
-}

evalInf_ :: TermInf -> Value
evalInf_ = evalInf0 []

evalInf0 :: Env -> TermInf -> Value
evalInf0 = evalInf 0

evalInf :: Int -> Env -> TermInf -> Value
evalInf i c = go
  where
  chk i = evalChk i c
  go = \case
    e ::: _  -> chk i e
    Star     -> VStar
    Pi p p'  -> VPi (chk i p)
      $ \x -> evalChk (i+1) ((Local i,x) : c) p'
    Free x   -> case lookup x c of
      Just v -> v
      _      -> vfree x
    Bound j  -> case lookup (Local j) c of
      Just v -> v
      _      -> error "unbound var"
    e :@: e' -> vapp (go e) (chk i e')

evalChk_ :: TermChk -> Value
evalChk_ = evalChk0 []

evalChk0 :: Env -> TermChk -> Value
evalChk0 = evalChk 0

evalChk :: Int -> Env -> TermChk -> Value
evalChk i c = \case
  Inf e -> evalInf i c e
  Lam e -> VLam $ \x -> evalChk (i+1) ((Local i,x) : c) e

vapp :: Value -> Value -> Value
vapp v1 v2 = case v1 of
  VLam f     -> f v2
  VNeutral n -> VNeutral $ NApp n v2
  _          -> error "bad application"

-- }}}

type Context = [(Name,Type)]
type Result = Either String

index :: Int -> [a] -> Maybe a
index i as
  | i < 0 || i >= length as
  = Nothing
  | otherwise
  = Just $ as !! i

throwError :: String -> Result a
throwError = Left

unbound :: Name -> Result a
unbound x = throwError
  $ "Unbound identifier: "
 ++ show x

badApp :: Result a
badApp = throwError "Illegal application"

badType :: Pretty t => t -> Type -> Type -> Result a
badType t exp act = throwError
  $ "Couldn't match expected type ("
 ++ pretty exp
 ++ ") with actual type ("
 ++ pretty act
 ++ ") of term "
 ++ pretty t

badType' :: Type -> Result a
badType' act = throwError
  $ "Type mismatch: "
 ++ pretty act

typeInf0 :: Env -> Context -> TermInf -> Result Type
typeInf0 = typeInf 0

typeInf :: Int -> Env -> Context -> TermInf -> Result Type
typeInf i env cxt expr = case expr of
  e ::: p -> do
    typeChk i env cxt p VStar
    let t = evalChk0 env p
    typeChk i env cxt e t
    return t
  Star -> return VStar
  Pi p p' -> do
    typeChk i env cxt p VStar
    let t = evalChk0 env p
    typeChk (i+1) env ((Local i, t) : cxt)
      (substChk 0 env cxt (Free (Local i)) p')
      VStar
    return VStar
  Free x -> case lookup x cxt of
    Just t -> return t
    _      -> unbound x
  e :@: e' -> do
    typ <- typeInf i env cxt e
    case typ of
      VPi t t' -> do
        typeChk i env cxt e' t
        return $ t' $ evalChk0 env e'
      _ -> badApp
  _ -> throwError "Encountered bound var?"

typeChk :: Int -> Env -> Context -> TermChk -> Type -> Result ()
typeChk i env cxt expr v = case expr of
  Inf e -> do
    v' <- typeInf i env cxt e
    unless (quote0 v == quote0 v') $ badType e v v'
  Lam e -> case v of
    VPi t t' ->
      typeChk (i + 1) env ((Local i,t) : cxt)
        (substChk 0 env cxt (Free (Local i)) e)
        (t' $ vfree $ Local i)
    _ -> badType' v

substInf :: Int -> Env -> Context -> TermInf -> TermInf -> TermInf
substInf i env cxt r = go
  where
  go = \case
    e ::: t  -> substChk i env cxt r e ::: t
    Star     -> Star
    Pi t t'  -> Pi (substChk i env cxt r t) (substChk (i+1) env cxt r t')
    Bound j  -> if i == j then r else Bound j
    Free  y  -> case (lookup y env,lookup y cxt) of
      (Just v,Just t) -> quote0 v ::: quote0 t
      _               -> Free y
    e :@: e' -> go e :@: substChk i env cxt r e'

substChk :: Int -> Env -> Context -> TermInf -> TermChk -> TermChk
substChk i env cxt r = \case
  Inf e -> Inf $ substInf i env cxt r e
  Lam e -> Lam $ substChk (i+1) env cxt r e

-- }}}

-- Quotation {{{

quote0 :: Value -> TermChk
quote0 = quote 0

quote :: Int -> Value -> TermChk
quote i val = case val of
  VLam f     -> Lam $ quote (i+1) $ f $ vfree $ Quote i
  VStar      -> Inf Star
  VPi v f    -> Inf $ Pi (quote i v)
    $ quote (i+1) $ f $ vfree $ Quote i
  VNeutral n -> Inf $ neutralQuote i n

neutralQuote :: Int -> Neutral -> TermInf
neutralQuote i neut = case neut of
  NFree x  -> boundfree i x
  NApp n v -> neutralQuote i n :@: quote i v

boundfree :: Int -> Name -> TermInf
boundfree i nm = case nm of
  Quote k -> Bound $ i - k - 1
  _       -> Free nm

-- }}}

-- Repl Types {{{

data Input
  = Cmd Command
  | Trm TermInf

readInput :: ReadS Input
readInput = reads

instance Read Input where
  readsPrec d r =
       [ (Cmd c,s)
       | (':':s1,s2) <- lex r
       , (c,s)       <- reads $ s1++s2
       ]
    ++ [ (Trm e,s)
       | null [ s | (':':_,s) <- lex r ]
       , (e,s) <- reads r
       ]

newtype Assump = Assump
  { getAssump :: (Name,Type)
  }

instance Show Assump where
  showsPrec d (Assump (n,t)) =
    shows (n,quote0 t)

instance Read Assump where
  readsPrec d =
    readParen (d > 10) $ \r ->
      [ (Assump (Global x,evalInf_ e),u)
      | (x,s)    <- lex r
      , isIdent x
      , ("::",t) <- lex s
      , (e,u)    <- reads t
      ]
  readList r =
       [ ([a],s)
       | (a,s) <- readsPrec 11 r
       , null $ (readList :: ReadS [Assump]) s
       ]
    ++ [ (i:is,t)
       | (i,s)  <- reads r
       , (is,t) <- readList s
       ]

data Command
  = Assume [Assump]
  | Let  Name TermInf
  | Type TermInf
  | Exit
  deriving (Show)

instance Read Command where
  readsPrec d r =
       [ (Assume is,t)
       | ("assume",s) <- lex r
       , (is,t) <- reads s
       ]
    ++ [ (Let x e,v)
       | ("let",s) <- lex r
       , (x,t)     <- reads s
       , ("=",u)   <- lex t
       , (e,v)     <- reads u
       ]
    ++ [ (Type e,t)
       | ("type",s) <- lex r
       , (e,t)      <- reads s
       ]
    ++ [ (Exit,s)
       | s <- concat
         [ [ s | ("exit",s) <- lex r ]
         , [ s | ("quit",s) <- lex r ]
         ]
       ]

readCommand :: ReadS Command
readCommand = reads

-- }}}

type Env = [(Name,Value)]

data RS = RS
  { _replEnv    :: Env
  , _replCxt    :: Context
  , _replPrompt :: String
  }

makeLenses ''RS

newtype Repl a = Repl
  { unRepl :: InputT (StateT RS IO) a
  } deriving (Functor,Applicative,Monad,MonadIO,MonadException)

instance MonadState RS Repl where
  get   = Repl $ lift $ get
  put   = Repl . lift . put

freshNames :: [String]
freshNames = concat
  [ [ n++i
    | n <- ns
    ]
  | i <- "" : map show [(0 :: Integer)..]
  ]
  where
  ns = map (:[]) ['a'..'z']

initRS :: RS
initRS = RS
  { _replEnv    = []
  , _replCxt    = []
  , _replPrompt = "» "
  }

io :: IO a -> Repl a
io = liftIO

input :: Repl (Maybe String)
input = do
  p <- use replPrompt
  Repl $ getInputLine p

output :: String -> Repl ()
output = Repl . outputStr

outputLn :: String -> Repl ()
outputLn = Repl . outputStrLn

runRepl :: Repl () -> IO ()
runRepl =
    flip evalStateT initRS
  . runInputT settings
  . unRepl
  . handle closer
  where
  settings = defaultSettings
    & setComplete noCompletion
  closer = \case
    UserInterrupt -> exitRepl
    e             -> throwIO e

exitRepl :: Repl ()
exitRepl = outputLn "Goodbye."

repl :: IO ()
repl = runRepl loop
  where
  loop :: Repl ()
  loop = do
    ml <- input
    case ml of
      Nothing -> return ()
      Just s | all isSpace s -> loop
      Just ln -> case readMaybe ln of
        Just (Cmd c) -> handleCmd c
        Just (Trm t) -> handleTrm t >> loop
        _            -> noParse ln  >> loop
  noParse l = outputLn $ "No parse: " ++ l
  --------
  handleCmd = \case
    Assume c' -> do
      replCxt %= (++ map getAssump c')
      loop
    Let x t   -> do
      mt <- inferType t
      whenJust mt $ \typ -> do
        val <- evalTerm t
        replCxt %= ((x,typ) :)
        replEnv %= ((x,val) :)
      loop
    Type t    -> do
      mt <- inferType t
      whenJust mt $ \typ ->
        outputLn $ "  " ++ pretty t ++ " :: " ++ pretty typ
      loop
    Exit      -> exitRepl
  --------
  handleTrm t = do
    mt <- inferType t
    whenJust mt $ \typ -> do
      val <- evalTerm t
      outputLn $ "  "
        ++ pretty val
        ++ " :: "
        ++ pretty typ

evalTerm :: TermInf -> Repl Value
evalTerm t = do
  env <- use replEnv
  return $ evalInf0 env t

inferType :: TermInf -> Repl (Maybe Type)
inferType t = do
  env <- use replEnv
  cxt <- use replCxt
  case typeInf0 env cxt t of
    Right typ -> return $ Just typ
    Left  err -> do
      outputLn $ "Typecheck Error: " ++ err
      return Nothing

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

-- Examples {{{

id'    = Lam $ Inf $ Bound 0
const' = Lam $ Lam $ Inf $ Bound 1

{-
t1, t2 :: TermInf
t1 = (id' ::: "a" :-> "a") :@: "y"
t2 = (const' ::: ("b" :-> "b") :-> "a" :-> ("b" :-> "b"))
  :@: id' :@: "y"
-}

{-
env1, env2 :: Context
env1 =
  [ HasType "y" "a"
  , HasKind "a" Star
  ]
env2 = HasKind "b" Star : env1
-}

-- }}}

