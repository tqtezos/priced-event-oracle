{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.PingOracle where

import Data.Bool
import Data.Function
import Data.Maybe
import System.IO
import Text.Show
import Text.Read
import GHC.Enum

import Michelson.Text
import Tezos.Crypto.Orphans ()
import Lorentz
import Lorentz.Entrypoints ()

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.IO.Utf8 as Utf8

data Storage = Storage
  { waiting :: Bool
  , response :: MText
  , counter :: Natural
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

unStorage :: Storage & s :-> (Bool, (MText, Natural)) & s
unStorage = forcedCoerce_

toStorage :: (Bool, (MText, Natural)) & s :-> Storage & s
toStorage = forcedCoerce_

data Parameter
  = Ask
  | Respond (MText, Natural)
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

pingOracleContract :: Address -> Address -> Address -> MText -> Mutez -> ContractCode Parameter Storage
pingOracleContract owner oracle responder query price = do
  unpair
  caseT @Parameter
    ( #cAsk /-> do
        unStorage
        unpair
        if_
          (push (mkMTextUnsafe "waiting") >> failWith)
          (do
            sender
            push owner
            ifEq
              (do
                push oracle
                contract @(ContractRef (MText, Natural), MText)
                ifSome
                  (do
                    swap
                    dip $ do
                      push query
                      selfCalling @Parameter (Call @"Respond")
                      pair
                      push price
                      swap
                      transferTokens
                    push True
                    pair
                    toStorage
                    swap
                    nil
                    swap
                    cons
                  )
                  (push (mkMTextUnsafe "oracle not found") >> failWith)
              )
              (push (mkMTextUnsafe "not owner") >> failWith)
          )

    , #cRespond /-> do
        swap
        -- storage & param
        unStorage
        unpair
        -- waiting & stored_param & param
        if_
          (do
            sender
            push responder
            ifEq
              (do
                -- stored_param & param
                cdr
                -- stored_nat & param
                swap
                -- param & stored_nat
                dup
                -- param & param & stored_nat
                car
                -- new_str & param & stored_nat
                dip $ do
                  -- param & stored_nat
                  cdr
                  -- param_nat & stored_nat

                  dup
                  -- param_nat & param_nat & stored_nat
                  push @Natural 1
                  -- 1 & param_nat & param_nat & stored_nat
                  add
                  -- param_nat + 1 & param_nat & stored_nat
                  dip $ do
                    -- param_nat & stored_nat
                    ifEq
                      nop
                      (push (mkMTextUnsafe "unexpected counter") >> failWith)
                  -- param_nat + 1
                -- new_str & param_nat + 1
                pair
                push False
                pair
                toStorage
                nil
              )
              (push (mkMTextUnsafe "not responder") >> failWith)
          )
          (push (mkMTextUnsafe "not waiting") >> failWith)
    )
  pair

instance HasAnnotation Storage

printPingOracleContract :: Address -> Address -> Address -> MText -> Mutez -> Maybe FilePath -> Bool -> IO ()
printPingOracleContract owner oracle responder query price mOutput forceOneLine' =
  maybe TL.putStrLn Utf8.writeFile mOutput $
  printLorentzContract forceOneLine' $
    (defaultContract (pingOracleContract owner oracle responder query price))
      { cDisableInitialCast = True }

printExampleOracle :: IO ()
printExampleOracle =
  printPingOracleContract
  (read "tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8")
  (read "tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8")
  (read "tz1QS8VYYVDjv7iReBzXeheL6x63A1oATTj8")
  (mkMTextUnsafe "some_query")
  (toEnum 10)
  (Just "ping_oracle.tz")
  False





--instance HasAnnotation ExplicitAdmin
--instance HasAnnotation SetStorage

--instance ParameterHasEntrypoints (Parameter MText ExplicitAdmin) where
--  type ParameterEntrypointsDerivation (Parameter MText ExplicitAdmin) = EpdRecursive

--instance ParameterHasEntrypoints (Parameter MText LambdaAdmin) where
--  type ParameterEntrypointsDerivation (Parameter MText LambdaAdmin) = EpdPlain

--instance t ~ (ToT LambdaAdmin) => ParameterHasEntrypoints (Parameter MText (Value t)) where
--  type ParameterEntrypointsDerivation (Parameter MText (Value t)) = EpdPlain

--instance WellTyped t => IsoValue (Value t) where
--  type ToT (Value t) = t
--  toVal = id
--  fromVal = id

--instance SingI t => HasAnnotation (Value t) where
--  getAnnotation _ = starNotes


---- | Print `explicitAdminOracleContract`
----
---- @
----  printExplicitAdminOracleContract (Just "explicit_admin_oracle.tz") False
---- @
--printExplicitAdminOracleContract :: Maybe FilePath -> Bool -> IO ()
--printExplicitAdminOracleContract mOutput forceOneLine' =
--  maybe TL.putStrLn Utf8.writeFile mOutput $
--  printLorentzContract forceOneLine' $
--    (defaultContract (explicitAdminOracleContract @MText))
--      { cDisableInitialCast = True }

---- | Print `explicitAdminOracleContract`
----
---- @
----  printLambdaAdminOracleContract (Just "lambda_admin_oracle.tz") False
---- @
--printLambdaAdminOracleContract :: Maybe FilePath -> Bool -> IO ()
--printLambdaAdminOracleContract mOutput forceOneLine' =
--  maybe TL.putStrLn Utf8.writeFile mOutput $
--  printLorentzContract forceOneLine' $
--    (defaultContract (lambdaAdminOracleContract @MText))
--      { cDisableInitialCast = True }

---- | Print a `Lambda` to set the owner (admin) to the given `Address`
----
---- @
----  Tezos.Crypto.Orphans Text.Read> printSetOwnerLambda (read "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir")
----  { CDR; PUSH address "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir"; PAIR; NIL operation; PAIR }
---- @
--printSetOwnerLambda :: Address -> IO ()
--printSetOwnerLambda newOwner =
--  TL.putStrLn $
--  printLorentzValue @LambdaAdmin forceOneLine $
--  setOwnerLambdaAdmin newOwner
--  where
--    forceOneLine = True

---- | Print a `Lambda` to send the given amount of `Mutez` to the given `Address`
----
---- @
----  Tezos.Crypto.Orphans Text.Read> printSetPriceLambda (toMutez 2)
----  { CAR; PUSH mutez 2; SWAP; PAIR; NIL operation; PAIR }
---- @
--printSetPriceLambda :: Mutez -> IO ()
--printSetPriceLambda newPrice =
--  TL.putStrLn $
--  printLorentzValue @LambdaAdmin forceOneLine $
--  setPriceLambdaAdmin newPrice
--  where
--    forceOneLine = True

---- | Print a `Lambda` to send the given amount of `Mutez` to the given `Address`
--printSendMutezLambda :: Mutez -> Address -> IO ()
--printSendMutezLambda amountToSend target =
--  TL.putStrLn $
--  printLorentzValue @LambdaAdmin forceOneLine $
--  sendMutezLambdaAdmin amountToSend target
--  where
--    forceOneLine = True


