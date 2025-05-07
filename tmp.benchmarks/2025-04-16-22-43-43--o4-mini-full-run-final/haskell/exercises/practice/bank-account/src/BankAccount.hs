module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- A BankAccount holds a TVar containing Maybe Integer:
-- Just balance = open account, Nothing = closed account.
data BankAccount = BankAccount (TVar (Maybe Integer))

-- Create a new account with initial balance 0.
openAccount :: IO BankAccount
openAccount = do
    tv <- newTVarIO (Just 0)
    return (BankAccount tv)

-- Close the account by setting its state to Nothing.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tv) = atomically $ writeTVar tv Nothing

-- Read the current balance; Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tv) = atomically $ readTVar tv

-- Adjust the balance by the given amount (can be negative).
-- Returns the new balance, or Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tv) amount = atomically $ do
    m <- readTVar tv
    case m of
        Nothing    -> return Nothing
        Just cur -> do
            let new = cur + amount
            writeTVar tv (Just new)
            return (Just new)
