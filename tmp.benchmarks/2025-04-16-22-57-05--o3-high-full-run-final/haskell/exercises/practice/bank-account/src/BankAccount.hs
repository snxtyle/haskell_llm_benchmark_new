module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )

-- | A bank account is represented by a transactional variable that may contain
--   a balance (Just amount) or be closed (Nothing).
newtype BankAccount = BankAccount (TVar (Maybe Integer))

-- | Open a new account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = do
    tv <- newTVarIO (Just 0)
    pure (BankAccount tv)

-- | Close an existing account. After closing, all operations will return Nothing.
closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tv) = atomically $
    writeTVar tv Nothing

-- | Retrieve the current balance. Returns Nothing if the account is closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tv) = atomically $
    readTVar tv

-- | Increment (or decrement) the balance.
--   Returns the new balance, or Nothing if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tv) amount = atomically $ do
    mBalance <- readTVar tv
    case mBalance of
        Nothing       -> pure Nothing      -- account is closed
        Just balance  -> do
            let newBalance = balance + amount
            writeTVar tv (Just newBalance)
            pure (Just newBalance)
