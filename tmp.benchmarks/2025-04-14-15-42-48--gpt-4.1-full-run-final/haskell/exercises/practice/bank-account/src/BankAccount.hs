module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

data AccountState = AccountState
    { isOpen  :: Bool
    , balance :: Integer
    }

newtype BankAccount = BankAccount (TVar AccountState)

openAccount :: IO BankAccount
openAccount = do
    tvar <- newTVarIO (AccountState True 0)
    return (BankAccount tvar)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ do
    acc <- readTVar tvar
    writeTVar tvar acc { isOpen = False }

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = atomically $ do
    acc <- readTVar tvar
    if isOpen acc
        then return (Just (balance acc))
        else return Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    acc <- readTVar tvar
    if isOpen acc
        then do
            let newBalance = balance acc + amount
            writeTVar tvar acc { balance = newBalance }
            return (Just newBalance)
        else return Nothing
