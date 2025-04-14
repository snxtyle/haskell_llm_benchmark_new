module Bob (responseFor) where

responseFor :: String -> String
responseFor input
  | null (trim input)          = "Fine. Be that way!"
  | isYelling input && isQuestion input = "Calm down, I know what I'm doing!"
  | isYelling input            = "Whoa, chill out!"
  | isQuestion input           = "Sure."
  | otherwise                  = "Whatever."
  where
    isQuestion = endsWith '?' . trim
    isYelling text = let letters = filter (\c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') text
                     in not (null letters) && all (\c -> c >= 'A' && c <= 'Z') letters
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    endsWith c str = not (null str) && last str == c
