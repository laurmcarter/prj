
module TH where

import Control.Lens
import Data.Char (toLower)
import Language.Haskell.TH

stripApost :: String -> String
stripApost = reverse . dropWhile (== '\'') . reverse

isoRules' :: LensRules
isoRules' = isoRules
  & lensIso .~ (Just . map toLower . stripApost)

makeIso' :: Name -> Q [Dec]
makeIso' = makeLensesWith isoRules'

