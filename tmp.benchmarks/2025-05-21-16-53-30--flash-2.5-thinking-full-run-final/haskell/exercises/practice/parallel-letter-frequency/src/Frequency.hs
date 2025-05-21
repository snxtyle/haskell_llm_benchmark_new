module Frequency (frequency) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, toLower)
import Control.Parallel.Strategies (parList, rdeepseq, withStrategy)

-- Helper function to count letter frequencies in a single Text.
-- It iterates through the text, converts characters to lowercase,
-- and counts only alphabetic characters.
countLetters :: Text -> M.Map Char Int
countLetters = T.foldl' (\acc c ->
    let lc = toLower c
    in if isAlpha lc
       then M.insertWith (+) lc 1 acc
       else acc
    ) M.empty

-- Main function to calculate total letter frequencies in a list of texts
-- using parallelism.
-- The 'nWorkers' parameter is typically used to configure the Haskell RTS
-- (e.g., by running the program with `+RTS -N<nWorkers> -RTS`).
-- The `parList rdeepseq` strategy will utilize the available capabilities
-- as set by the RTS to parallelize the computation of individual text frequencies.
frequency :: Int -> [Text] -> Map Char Int
frequency _ texts =
    let
        -- Calculate frequencies for each text in parallel.
        -- `map countLetters texts` produces a list of maps.
        -- `withStrategy (parList rdeepseq)` evaluates this list of maps
        -- in parallel, ensuring each map is fully evaluated (to normal form)
        -- before being combined.
        individualFrequencies = withStrategy (parList rdeepseq) (map countLetters texts)

        -- Combine all individual frequency maps into a single total map.
        -- `M.unionWith (+)` merges two maps by summing values for common keys.
        totalFrequency = foldl (M.unionWith (+)) M.empty individualFrequencies
    in
        totalFrequency
