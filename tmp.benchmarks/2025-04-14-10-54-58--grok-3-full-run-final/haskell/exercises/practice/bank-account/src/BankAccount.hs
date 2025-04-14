module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, readTVar, writeTVar)

-- BankAccount is a transactional variable holding a Maybe Integer
-- Nothing represents a closed account, Just Integer represents the balance
newtype BankAccount = BankAccount (TVar (Maybe Integer))

-- Opens a new bank account with an initial balance of 0
openAccount :: IO BankAccount
openAccount = do
    tvar <- newTVarIO (Just 0)
    return (BankAccount tvar)

-- Closes the bank account
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ writeTVar tvar Nothing

-- Gets the current balance of the bank account
-- Returns Nothing if the account is closed
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = readTVarIO tvar

-- Increments the balance by the given amount
-- Returns Nothing if the account is closed, otherwise Just newBalance
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    current <- readTVar tvar
    case current of
        Nothing -> return Nothing
        Just balance -> do
            let newBalance = balance + amount
            writeTVar tvar (Just newBalance)
            return (Just newBalance)
