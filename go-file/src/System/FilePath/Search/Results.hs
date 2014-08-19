
module System.FilePath.Search.Results
  ( Results (..)
  , fromList
  , render1
  , render
  , toBash
  ) where

data Results
  = None
  | One FilePath
  | Many Int [(Int,FilePath)]
  deriving (Eq,Show)

fromList :: [(Int,FilePath)] -> Results
fromList []      = None
fromList [(_,s)] = One s
fromList fs      = Many (length fs) fs

render1 :: (Int,FilePath) -> String
render1 (i,p) = "  " ++ n ++ ": " ++ p
  where
  i' = show i
  n  = i' ++ replicate (4 - length i') ' '

render :: Results -> String
render r = case r of
  None     -> "No results."
  One _    -> "Only 1 result."
  Many n _ -> show n ++ " results."

toBash :: Either FilePath [(Int,FilePath)] -> String
toBash e = case e of
  Left p   -> oneFunction  p
  Right ps -> manyFunction ps

oneFunction :: FilePath -> String
oneFunction r = unlines $
  [ "#!/bin/bash"
  , ""
  , "echo " ++ show r
  , ""
  ]

manyFunction :: [(Int,FilePath)] -> String
manyFunction rs = unlines $
  [ "#!/bin/bash"
  , ""
  ] ++ resultsFn ++
  [ ""
  , "[[ -n \"$1\" ]] || { results; exit 0; }"
  , ""
  , "case \"$1\" in"
  ] ++ map (printCase 2) rs ++
  [ "  *) echo \"Result index out of bounds: $1\"; exit 1 ;;"
  , "esac"
  , ""
  ]
  where
  resultsFn :: [String]
  resultsFn =
    [ "function results {"
    ] ++ map (printResult 2) rs ++
    [ "}"
    ]
  printResult = print1 ("echo \"" , " : "       , "\""   )
  printCase   = print1 (""        , ") echo \"" , "\" ;;")
  print1 :: (String,String,String) -> Int -> (Int,FilePath) -> String
  print1 (b,m,e) n (i,p) = replicate n ' ' ++ b ++ show i ++ m ++ p ++ e

