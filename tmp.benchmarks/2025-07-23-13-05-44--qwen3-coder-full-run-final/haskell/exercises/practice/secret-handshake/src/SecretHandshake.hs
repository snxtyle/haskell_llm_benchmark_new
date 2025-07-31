module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = 
  let actions = []
      actions' = if n .&. 1 /= 0 then actions ++ ["wink"] else actions
      actions'' = if n .&. 2 /= 0 then actions' ++ ["double blink"] else actions'
      actions''' = if n .&. 4 /= 0 then actions'' ++ ["close your eyes"] else actions''
      actions'''' = if n .&. 8 /= 0 then actions''' ++ ["jump"] else actions'''
  in if n .&. 16 /= 0 then reverse actions'''' else actions''''
