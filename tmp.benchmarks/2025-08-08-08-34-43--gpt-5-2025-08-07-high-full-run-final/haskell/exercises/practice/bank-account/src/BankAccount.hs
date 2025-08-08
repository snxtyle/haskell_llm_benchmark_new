module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar, modifyMVar_)

-- BankAccount holds a mutable, thread-safe Maybe balance:
--   Just n = open with balance n
--   Nothing = closed
newtype BankAccount = BankAccount (MVar (Maybe Integer))

openAccount :: IO BankAccount
openAccount = BankAccount <$> newMVar (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount mv) =
  -- Set the account to closed; idempotent and thread-safe.
  modifyMVar_ mv (const (pure Nothing))

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount mv) = withMVar mv pure

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount mv) amount =
  modifyMVar mv $ \mBal ->
    case mBal of
      Nothing -> pure (Nothing, Nothing)
      Just bal ->
        let newBal = bal + amount
        in pure (Just newBal, Just newBal)
