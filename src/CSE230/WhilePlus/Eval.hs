{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}

module CSE230.WhilePlus.Eval where

import qualified Data.Map as Map
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Identity
import           CSE230.WhilePlus.Types

----------------------------------------------------------------------------------------------
-- | A Combined monad that is BOTH 
--    (i) a WState-Transformer monad 
--    (ii) an Exception monad with exceptions of type Value 
----------------------------------------------------------------------------------------------
type MonadWhile m = (MonadState WState m, MonadError Value m)

----------------------------------------------------------------------------------------------
-- | `readVar x` returns the value of the variable `x` in the "current store"
----------------------------------------------------------------------------------------------
readVar :: (MonadWhile m) => Variable -> m Value
readVar x = do 
  WS s _ <- get 
  case Map.lookup x s of
    Just v  -> return v
    Nothing -> throwError (IntVal 0)

----------------------------------------------------------------------------------------------
-- | `writeVar x v` updates the value of `x` in the store to `v`
----------------------------------------------------------------------------------------------
writeVar :: (MonadState WState m) => Variable -> Value -> m ()
writeVar x v = do 
  WS s log <- get 
  let s' = Map.insert x v s
  put (WS s' log)

----------------------------------------------------------------------------------------------
-- | `printString msg` adds the message `msg` to the output log
----------------------------------------------------------------------------------------------
printString :: (MonadState WState m) => String -> m ()
printString msg = do
  WS s log <- get
  put (WS s (msg:log))


-- NOTE: See how the types of `writeVar` and `printString` say they CANNOT throw an exception!

----------------------------------------------------------------------------------------------
-- | Requirements & Expected Behavior of New Constructs
----------------------------------------------------------------------------------------------

{-
  * `Print s e` should print out (eg to stdout) log the string corresponding
     to the string `s` followed by whatever `e` evaluates to, followed by a
     newline --- for example, `Print "Three: " (IntVal 3)' should "display" 
     i.e. add to the output log, the String  
     
     "Three: IntVal 3\n",

  * `Throw e` evaluates the expression `e` and throws it as an exception, and

  * `Try s x h` executes the statement `s` and if in the course of
     execution, an exception is thrown, then the exception comes shooting
     up and is assigned to the variable `x` after which the *handler*
     statement `h` is executed.

  In the case of exceptional termination, 

  * the output `wStore` should be the state *at the point where the last exception was thrown, and 

  * the output `wLog` should include all the messages *upto* that point
   
  * Reading an undefined variable should raise an exception carrying the value `IntVal 0`.

  * Division by zero should raise an exception carrying the value `IntVal 1`.

  * A run-time type error (addition of an integer to a boolean, comparison of
    two values of different types) should raise an exception carrying the value
    `IntVal 2`.
-}


evalS :: (MonadWhile m) => Statement -> m ()
evalS w@(While e s) = do
                        cond <- eval e 
                        case cond of 
                          BoolVal True -> evalS s >> evalS w
                          BoolVal False -> evalS Skip
                          otherwise -> do throwError $ IntVal 2-- Error While Cond is not Bool!
evalS Skip = return ()
evalS (Sequence s1 s2) = evalS s1 >> evalS s2
evalS (Assign x e) = do
                      res <- eval e
                      writeVar x res
evalS (If e s1 s2) = do
                      cond <- eval e
                      case cond of 
                        BoolVal True -> evalS s1
                        BoolVal False -> evalS s2
                        otherwise -> do throwError $ IntVal 2 -- Error If Cond: Not BoolVar
evalS (Print s e) = do
                      res <- eval e
                      printString $ s ++ show res
evalS (Throw e) = do
                    res <- eval e
                    throwError res
evalS (Try s x h) = do 
                    let handler err = do
                                        writeVar x err
                                        evalS h
                    catchError (evalS s) handler


eval :: (MonadWhile m) => Expression -> m Value
eval (Var x) = do readVar x
eval (Val x) = return x 
eval (Op o e1 e2) = do
                      v1 <- eval e1
                      v2 <- eval e2
                      semantics o v1 v2          

semantics :: (MonadWhile m) => Bop -> Value -> Value -> m Value
semantics Plus (IntVal i1) (IntVal i2) = return $ IntVal $ i1 + i2
semantics Minus (IntVal i1) (IntVal i2) = return $ IntVal $ i1 - i2
semantics Times (IntVal i1) (IntVal i2) = return $ IntVal $ i1 * i2
semantics Divide (IntVal i1) (IntVal i2)
  | i2 == 0 = do
                -- divided by 0
                throwError $ IntVal 1
  | otherwise = return $ IntVal $ i1 `div` i2
semantics Gt (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 > i2
semantics Ge (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 >= i2
semantics Lt (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 < i2
semantics Le (IntVal i1) (IntVal i2) = return $ BoolVal $ i1 <= i2
semantics _ _ _ = do
                    -- type mismatch
                    throwError $ IntVal 2
--------------------------------------------------------------------------
-- | Next, we will implement a *concrete instance* of a monad `m` that
--   satisfies the constraints of MonadWhile:
--------------------------------------------------------------------------

type Eval a = ExceptT Value (StateT WState (Identity)) a

--------------------------------------------------------------------------
-- | `runEval` implements a function to *run* the `Eval a` action from 
--   a starting `WState`. You can read the docs for `runState` and `runExceptT` 
--------------------------------------------------------------------------
runEval :: Eval a -> WState -> (Either Value a, WState)
runEval act s = runState (runExceptT act) s

{- | `execute sto stmt` returns a triple `(sto', exn, log)` where
      * `st'` is the output state,
      * `exn` is (Just v) if the program terminates with an "uncaught" exception with Value v 
         or Nothing if the program terminates without an exception.
      * `log` is the log of messages generated by the `Print` statements.

-}
execute :: Store -> Statement -> (Store, Maybe Value, String)
execute sto stmt     = (sto', leftMaybe v, unlines (reverse log))
  where
    (v, WS sto' log) = runEval (evalS stmt) (WS sto [])

leftMaybe :: Either a b -> Maybe a
leftMaybe (Left v)  = Just v
leftMaybe (Right _) = Nothing

------------------------------------------------------------------------------------
-- | When you are done you should see the following behavior 
------------------------------------------------------------------------------------

-- >>> execute initStore test1 == out1
-- True

-- >>> execute initStore test2 == out2
-- True

-- >>> execute initStore test3 == out3 
-- True
