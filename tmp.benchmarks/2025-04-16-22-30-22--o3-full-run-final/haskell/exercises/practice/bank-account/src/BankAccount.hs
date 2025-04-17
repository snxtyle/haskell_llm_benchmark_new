module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)

-- | A bank account is represented by a TVar holding
--   'Nothing' for a closed account and
--   'Just balance' for an open account.
newtype BankAccount = BankAccount (TVar (Maybe Integer))

-- | Create a new bank account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = do
    tv <- newTVarIO (Just 0)
    return (BankAccount tv)

-- | Close the account. Further operations will fail.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tv) =
    atomically $ writeTVar tv Nothing

-- | Get the current balance, or 'Nothing' if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tv) =
    atomically $ readTVar tv

-- | Increment the balance by the given amount (may be negative).
--   Returns 'Nothing' if the account is closed; otherwise returns
--   the new balance.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tv) amount =
    atomically $ do
        mb <- readTVar tv
        case mb of
            Nothing -> return Nothing          -- account closed
            Just b  -> do
                let newBal = b + amount
                writeTVar tv (Just newBal)
                return (Just newBal)
