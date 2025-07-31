module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, lift)
import Data.Char (chr, ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (randomRIO)
import qualified Data.Set as S

-- Robot is a mutable reference holding an optional name
newtype Robot = Robot (IORef (Maybe String))

-- RunState tracks the set of names currently assigned
newtype RunState = RunState { assigned :: S.Set String }

initialState :: RunState
initialState = RunState S.empty

mkRobot :: StateT RunState IO Robot
mkRobot = lift $ Robot <$> newIORef Nothing

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  mName <- lift $ readIORef ref
  case mName of
    Nothing   -> pure ()
    Just name -> do
      rs <- get
      let newSet = S.delete name (assigned rs)
      put (RunState newSet)
      lift $ writeIORef ref Nothing

robotName :: Robot -> StateT RunState IO String
robotName (Robot ref) = do
  mName <- lift $ readIORef ref
  case mName of
    Just n  -> pure n
    Nothing -> do
      n <- assignUniqueName
      lift $ writeIORef ref (Just n)
      pure n

-- Helper used within StateT to ensure uniqueness
assignUniqueName :: StateT RunState IO String
assignUniqueName = do
  rs <- get
  n <- lift genNameIO
  if n `S.member` assigned rs
     then assignUniqueName
     else do
       put (RunState (S.insert n (assigned rs)))
       pure n

-- Generate a random name: two uppercase letters followed by three digits
genNameIO :: IO String
genNameIO = do
  a <- randUpper
  b <- randUpper
  d1 <- randDigit
  d2 <- randDigit
  d3 <- randDigit
  pure [a, b, d1, d2, d3]

randUpper :: IO Char
randUpper = do
  i <- randomRIO (0, 25)
  pure $ chr (ord 'A' + i)

randDigit :: IO Char
randDigit = do
  i <- randomRIO (0, 9)
  pure $ chr (ord '0' + i)
