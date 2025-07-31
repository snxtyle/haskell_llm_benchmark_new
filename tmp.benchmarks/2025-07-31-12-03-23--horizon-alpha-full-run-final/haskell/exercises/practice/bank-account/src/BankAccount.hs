module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- Represent the account state as a TVar of Maybe Integer.
-- Just balance => account is open with given balance.
-- Nothing      => account is closed.
newtype BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tv) = atomically $ writeTVar tv Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tv) = readTVarIO tv

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tv) amount = atomically $ do
    state <- readTVar tv
    case state of
        Nothing -> pure Nothing
        Just bal ->
            let newBal = bal + amount
            in writeTVar tv (Just newBal) >> pure (Just newBal)

openAccount :: IO BankAccount
openAccount = BankAccount <$> newTVarIO (Just 0)
