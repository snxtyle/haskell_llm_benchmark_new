module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar, newTVarIO)

-- BankAccount holds a TVar for the balance and a TVar for its open/closed status.
-- The type itself (BankAccount) is exported, but not its constructor,
-- making it an abstract data type.
data BankAccount = Account (TVar Integer) (TVar Bool)

-- Opens a new bank account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = do
    balanceTVar <- newTVarIO 0
    statusTVar  <- newTVarIO True -- True means open, False means closed
    return (Account balanceTVar statusTVar)

-- Closes the bank account. Operations on a closed account should fail.
closeAccount :: BankAccount -> IO ()
closeAccount (Account _ statusTVar) =
    atomically $ writeTVar statusTVar False

-- Gets the current balance of the account.
-- Returns Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (Account balanceTVar statusTVar) =
    atomically $ do
        isOpen <- readTVar statusTVar
        if isOpen
        then do
            balance <- readTVar balanceTVar
            return (Just balance)
        else
            return Nothing

-- Increments the balance of the account by a given amount.
-- The amount can be negative for withdrawals.
-- Returns the new balance, or Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (Account balanceTVar statusTVar) amount =
    atomically $ do
        isOpen <- readTVar statusTVar
        if isOpen
        then do
            currentBalance <- readTVar balanceTVar
            let newBalance = currentBalance + amount
            writeTVar balanceTVar newBalance
            return (Just newBalance)
        else
            return Nothing
