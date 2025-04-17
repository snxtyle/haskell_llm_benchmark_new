module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go xs []
  where
    go [] stack = null stack
    go (c:cs) stack
      | c `elem` "([{"  = go cs (c : stack)
      | c == ')'        = case stack of
                             ('(' : rest) -> go cs rest
                             _            -> False
      | c == ']'        = case stack of
                             ('[' : rest) -> go cs rest
                             _            -> False
      | c == '}'        = case stack of
                             ('{' : rest) -> go cs rest
                             _            -> False
      | otherwise       = go cs stack
