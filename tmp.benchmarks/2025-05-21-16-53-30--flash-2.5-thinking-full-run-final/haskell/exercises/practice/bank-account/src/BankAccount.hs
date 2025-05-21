module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

-- | BankAccount data type holds the balance and a flag indicating if the account is open.
-- Both are TVars to allow atomic updates using STM.
data BankAccount = BankAccount
    { accountBalance :: TVar Integer
    , accountIsOpen  :: TVar Bool
    }

-- | Closes the given bank account.
-- After closing, operations like getBalance or incrementBalance should fail.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount _ isOpenTVar) = atomically $ writeTVar isOpenTVar False

-- | Gets the current balance of the bank account.
-- Returns Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount balanceTVar isOpenTVar) = atomically $ do
    isOpen <- readTVar isOpenTVar
    if isOpen
        then Just <$> readTVar balanceTVar
        else return Nothing

-- | Increments the balance of the bank account by the given amount.
-- The amount can be negative for withdrawals.
-- Returns the new balance if the operation was successful (account was open),
-- otherwise returns Nothing.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount balanceTVar isOpenTVar) amount = atomically $ do
    isOpen <- readTVar isOpenTVar
    if isOpen
        then do
            currentBalance <- readTVar balanceTVar
            let newBalance = currentBalance + amount
            writeTVar balanceTVar newBalance
            return $ Just newBalance
        else return Nothing

-- | Opens a new bank account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = atomically $ do
    balanceTVar <- newTVar 0
    isOpenTVar  <- newTVar True
    return $ BankAccount balanceTVar isOpenTVar
