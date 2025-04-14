module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)

data AccountState = AccountState
    { balance :: Integer  -- The current balance
    , isOpen :: Bool     -- Whether the account is open
    }

newtype BankAccount = BankAccount (MVar AccountState)

-- | Opens a new bank account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = do
    mvar <- newMVar AccountState { balance = 0, isOpen = True }
    return (BankAccount mvar)

-- | Closes the bank account, preventing further operations.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount mvar) = do
    state <- takeMVar mvar  -- Atomically take the state
    putMVar mvar state { isOpen = False }  -- Update to closed

-- | Gets the balance of the bank account if it is open.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount mvar) = do
    state <- takeMVar mvar  -- Atomically take the state
    if isOpen state
        then do
            putMVar mvar state  -- Put back the state unchanged
            return (Just (balance state))
        else do
            putMVar mvar state  -- Put back the state
            return Nothing

-- | Increments the balance by the given amount if the account is open.
-- Returns Just the new balance if successful, Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount mvar) amount = do
    state <- takeMVar mvar  -- Atomically take the state
    if isOpen state
        then do
            let newBalance = balance state + amount  -- Update balance
            putMVar mvar state { balance = newBalance, isOpen = isOpen state }  -- Put back updated state
            return (Just newBalance)
        else do
            putMVar mvar state  -- Put back the state unchanged
            return Nothing
