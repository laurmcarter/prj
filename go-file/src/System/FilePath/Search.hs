
module System.FilePath.Search where

import System.FilePath.Search.Results

import Control.Concurrent
import Control.Exception
import Control.Monad.State.Strict
import Data.Char (toLower)
import System.Directory
import System.Environment hiding (setEnv)
import System.Exit
import System.FilePath
import System.IO
import System.Process

type Pattern = String
type Flags = [(String,String)]

-- Prog structure {{{

patternSearch :: (Pattern -> FilePath -> IO String)
  -> Pattern -> FilePath -> Bool -> IO ()
patternSearch mkCmd pat out prompt = do
  paths <- ascSearchPaths
  res   <- newMVar []
  done  <- newEmptyMVar
  tId   <- forkIO $
    runSearch mkCmd prompt
      pat paths res done
  let finish = finishResults out
           .   fromList
           .   zip [0..]
           =<< takeMVar res
  handle (\UserInterrupt ->
    throwTo tId UserInterrupt >> finish
    ) $ do
      takeMVar done
      finish

runSearch :: (Pattern -> FilePath -> IO String)
  -> Bool -> Pattern -> [FilePath]
  -> MVar [FilePath] -> MVar ()
  -> IO ()
runSearch mkCmd prompt pat paths res done =
    flip evalStateT res
  $ go paths
  where
  allDone :: M ()
  allDone = lift $ putMVar done ()
  go :: [FilePath] -> M ()
  go [] = allDone
  go (dir : ps) = do
    cmd <- lift $ mkCmd pat dir
    search cmd
    if prompt && not (null ps)
      then do
        cont <- lift promptCont
        if cont
          then go ps
          else allDone
      else go ps

search :: String -> M ()
search cmd = do
  (_,Just hOut,Just hErr,ph) <- lift $
    createProcess
      (shell cmd)
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  void $ lift $ forkIO $ do
    ec <- waitForProcess ph
    case ec of
      ExitSuccess   -> return ()
      ExitFailure _ -> do
        putStrLn "command failure:"
        hPutStrLn stderr =<< hGetContents hErr
        exitWith ec
  readCmdResults hOut

readCmdResults :: Handle -> M ()
readCmdResults h = lift (hIsEOF h) >>= \e ->
  when (not e) $ do
    f <- lift $ hGetLine h
    n <- curNumResults
    let n' = show n
    lift $ putStrLn $ pad 4 n' ++ f
    mvar $ flip modifyMVar_ $ \fs ->
      return $ fs ++ [f]

curNumResults :: M Int
curNumResults = mvar $ fmap length . readMVar

type M = StateT (MVar [FilePath]) IO

pad :: Int -> String -> String
pad i s = s ++ replicate (i - length s) ' '

mvar :: (MVar [FilePath] -> IO b) -> M b
mvar f = lift . f =<< get

-- }}}

-- IO Helpers {{{

usage :: Flags -> IO ()
usage fs = do
  prg <- getProgName
  putStrLn $ "Usage: " ++ prg ++ " " ++ flags ++ " <target>" ++ desc
  where
  flags = unwords $ map ((\s -> "["++s++"]") . fst) fs
  mx    = maximum $ map (length . fst) fs
  desc  = if null fs
    then ""
    else ('\n' :) $ unlines
      $ map (\(f,d) -> "  " ++ pad (mx + 2) f ++ d) fs

getTarget :: Flags -> IO Pattern
getTarget fs = do
  as <- getArgs
  case as of
    [f] -> return f
    _   -> usage fs >> exitFailure

-- }}}
 
-- Searching {{{

promptCont :: IO Bool
promptCont = do
  putStrLn "Continue? [Y,n]"
  l <- getLine
  return $ case map toLower l of
    ""    -> True
    'y':_ -> True
    _     -> False

-- | Produces a list of directories, starting with the cwd and
--   going incrementally up to the HOME directory.
ascSearchPaths :: IO [FilePath]
ascSearchPaths = do
  cur  <- canonicalizePath =<< getCurrentDirectory
  home <- canonicalizePath =<< getEnv "HOME"
  return
    $ map (normalise . (home </>))
    $ splitDirectories
    $ makeRelative home cur

-- }}}

-- Finishing {{{

-- | Takes the filepath for the generated bash script, and the results.
--   Prints a summary and writes out the script.
finishResults :: FilePath -> Results -> IO ()
finishResults tmp rs = do
  suc <- summarize tmp rs
  p   <- getPermissions tmp
  setPermissions tmp $ p { executable = True }
  if suc then exitSuccess else exitFailure

-- | Returns success (True) or failure (False)
summarize :: FilePath -> Results -> IO Bool
summarize tmp r = case r of
  None      -> do putStrLn "No results."
                  return False
  One    p  -> do putStrLn $ "Only 1 result."
                  writeFile tmp $ toBash $ Left p
                  return True
  Many n ps -> do putStrLn $ show n ++ " results."
                  writeFile tmp $ toBash $ Right ps
                  return True

-- }}}

