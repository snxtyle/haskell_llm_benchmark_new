module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

-- BankAccount holds a TVar containing Maybe Integer.
-- Just balance means the account is open with the given balance.
-- Nothing means the account is closed.
data BankAccount = BankAccount (TVar (Maybe Integer))

-- Closes the account. Operations will fail after this.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ writeTVar tvar Nothing

-- Gets the current balance if the account is open.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = atomically $ readTVar tvar

-- Increments the balance by the given amount (can be negative for withdrawal).
-- Returns the new balance if the account is open, otherwise Nothing.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    mBalance <- readTVar tvar
    case mBalance of
        Nothing -> return Nothing -- Account is closed
        Just balance -> do
            let newBalance = balance + amount
            writeTVar tvar (Just newBalance)
            return (Just newBalance) -- Return the new balance

-- Opens a new bank account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = do
    tvar <- atomically $ newTVar (Just 0) -- Initial balance is 0, account is open
    return (BankAccount tvar)
