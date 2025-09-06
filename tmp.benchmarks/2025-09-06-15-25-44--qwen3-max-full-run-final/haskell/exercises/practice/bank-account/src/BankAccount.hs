module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

-- BankAccount data type containing account state and balance
data BankAccount = BankAccount 
    { accountState :: TVar Bool  -- True if open, False if closed
    , accountBalance :: TVar Integer
    }

-- Open a new account with initial balance of 0
openAccount :: IO BankAccount
openAccount = do
    state <- newTVarIO True
    balance <- newTVarIO 0
    return $ BankAccount state balance

-- Close an account
closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ writeTVar (accountState account) False

-- Get the balance of the bank account
-- Returns Nothing if account is closed
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
    isOpen <- readTVar (accountState account)
    if isOpen
        then do
            balance <- readTVar (accountBalance account)
            return $ Just balance
        else return Nothing

-- Increment the balance of the bank account by the given amount
-- Returns Nothing if account is closed
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
    isOpen <- readTVar (accountState account)
    if isOpen
        then do
            currentBalance <- readTVar (accountBalance account)
            let newBalance = currentBalance + amount
            writeTVar (accountBalance account) newBalance
            return $ Just newBalance
        else return Nothing
