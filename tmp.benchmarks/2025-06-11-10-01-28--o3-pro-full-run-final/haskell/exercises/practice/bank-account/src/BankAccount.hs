module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )

-- | A 'BankAccount' holds its balance in an 'MVar'.
--   The 'Maybe' wrapper represents whether the account is open:
--     • 'Just n'  – the account is open and the current balance is @n@
--     • 'Nothing' – the account has been closed
type BankAccount = MVar (Maybe Integer)

-- | Open a new account with an initial balance of 0.
openAccount :: IO BankAccount
openAccount = newMVar (Just 0)

-- | Close an account. After closing, all operations return 'Nothing'.
closeAccount :: BankAccount -> IO ()
closeAccount acc = modifyMVar_ acc (const (pure Nothing))

-- | Retrieve the current balance.
--   Returns 'Nothing' if the account has been closed.
getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar

-- | Increment (or decrement) the balance by the given amount.
--   Returns the updated balance, or 'Nothing' if the account is closed.
incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance acc amount =
    modifyMVar acc $ \state ->
        case state of
            Nothing      -> pure (Nothing, Nothing)          -- account closed
            Just balance ->
                let newBalance = balance + amount
                in  pure (Just newBalance, Just newBalance)
