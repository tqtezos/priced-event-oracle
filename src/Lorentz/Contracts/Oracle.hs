{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Oracle where

import Data.Bool
import Data.Function
import Data.Maybe
import System.IO
import Text.Show

import Michelson.Text
import Michelson.Typed.Annotation (starNotes)
import Michelson.Typed.Haskell.Value (WellTyped)
import Lorentz
import Lorentz.Entrypoints ()

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.IO.Utf8 as Utf8

import Data.Singletons (SingI)

data Storage = Storage
  { owner :: Address
  , price :: Mutez
  }
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

unStorage :: Storage & s :-> (Address, Mutez) & s
unStorage = forcedCoerce_

toStorage :: (Address, Mutez) & s :-> Storage & s
toStorage = forcedCoerce_

data Parameter cp admin
  -- if @number of tez sent == price@, then do nothing, else fail
  = Ask (ContractRef (cp, Natural), MText)
  -- if @SENDER == owner@, then continue, else fail
  | Admin admin
  deriving stock Generic

deriving stock instance (Show cp, Show admin) => Show (Parameter cp admin)
deriving anyclass instance (IsoValue cp, IsoValue admin) => IsoValue (Parameter cp admin)


data SetStorage
  -- setPrice: then update price in storage
  = SetPrice Mutez
  -- update owner in storage
  | SetOwner Address
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

data ExplicitAdmin
  = Set SetStorage
  -- send `Mutez` to `SENDER`
  | Withdraw Mutez
  deriving stock Generic
  deriving stock Show
  deriving anyclass IsoValue

explicitAdminOracleContract :: forall cp. (IsoValue cp)
  => ContractCode (Parameter cp ExplicitAdmin) Storage
explicitAdminOracleContract = do
  unpair
  caseT @(Parameter cp ExplicitAdmin)
    ( #cAsk /-> do
        drop
        dup
        unStorage
        cdr
        amount
        ifEq
          nil
          (push (mkMTextUnsafe "wrong price") >> failWith)
    , #cAdmin /-> do
        swap
        dup
        unStorage
        car
        sender
        ifEq
          (do
            swap
            caseT @ExplicitAdmin
              ( #cSet /-> do
                  caseT @SetStorage
                    ( #cSetPrice /-> do
                        swap
                        unStorage
                        car
                    , #cSetOwner /-> do
                        swap
                        unStorage
                        cdr
                        swap
                    )
                  pair
                  toStorage
                  nil
              , #cWithdraw /-> do
                  sender
                  contract @()
                  ifNone
                    (push (mkMTextUnsafe "not receiver") >> failWith)
                    (do
                      swap
                      unit
                      transferTokens @()
                      nil
                      swap
                      cons
                    )
              )
          )
          (push (mkMTextUnsafe "not admin") >> failWith)
    )
  pair

type OriginalLambdaAdmin = Lambda () ([Operation], Storage)

originalLambdaAdminOracleContract :: forall cp. (IsoValue cp)
  => ContractCode (Parameter cp OriginalLambdaAdmin) Storage
originalLambdaAdminOracleContract = do
  unpair
  caseT @(Parameter cp OriginalLambdaAdmin)
    ( #cAsk /-> do
        drop
        dup
        unStorage
        cdr
        amount
        ifEq
          (nil >> pair)
          (push (mkMTextUnsafe "wrong price") >> failWith)
    , #cAdmin /-> do
        swap
        dup
        unStorage
        car
        sender
        ifEq
          (do
            drop @Storage
            unit
            exec @() @([Operation], Storage)
          )
          (push (mkMTextUnsafe "not admin") >> failWith)
    )

type LambdaAdmin = Lambda Storage ([Operation], Storage)

lambdaAdminOracleContract :: forall cp. (IsoValue cp)
  => ContractCode (Parameter cp (Value (ToT LambdaAdmin))) Storage
lambdaAdminOracleContract = do
  dup
  cdr
  swap
  car
  caseT @(Parameter cp (Value (ToT LambdaAdmin)))
    ( #cAsk /-> do
        drop
        dup
        unStorage
        cdr
        amount
        ifEq
          (nil >> pair)
          (push (mkMTextUnsafe "wrong price") >> failWith)
    , #cAdmin /-> do
        swap
        dup
        unStorage
        car
        sender
        ifEq
          (dip forcedCoerce_ >> exec)
          (push (mkMTextUnsafe "not admin") >> failWith)
    )

setOwnerLambdaAdmin :: Address -> LambdaAdmin
setOwnerLambdaAdmin newOwner = do
  unStorage
  cdr
  push newOwner
  pair
  toStorage
  nil
  pair

setPriceLambdaAdmin :: Mutez -> LambdaAdmin
setPriceLambdaAdmin newPrice = do
  unStorage
  car
  push newPrice
  swap
  pair
  toStorage
  nil
  pair

sendMutezLambdaAdmin :: Mutez -> Address -> LambdaAdmin
sendMutezLambdaAdmin amountToSend target = do
  nil
  push target
  contract @()
  ifNone
    (push (mkMTextUnsafe "not receiver") >> failWith)
    (do
      push amountToSend
      unit
      transferTokens
      cons
      pair
    )

instance HasAnnotation ExplicitAdmin
instance HasAnnotation SetStorage
instance HasAnnotation Storage

instance ParameterHasEntrypoints (Parameter MText ExplicitAdmin) where
  type ParameterEntrypointsDerivation (Parameter MText ExplicitAdmin) = EpdRecursive

instance ParameterHasEntrypoints (Parameter MText LambdaAdmin) where
  type ParameterEntrypointsDerivation (Parameter MText LambdaAdmin) = EpdPlain

instance t ~ (ToT LambdaAdmin) => ParameterHasEntrypoints (Parameter MText (Value t)) where
  type ParameterEntrypointsDerivation (Parameter MText (Value t)) = EpdPlain

instance WellTyped t => IsoValue (Value t) where
  type ToT (Value t) = t
  toVal = id
  fromVal = id

instance SingI t => HasAnnotation (Value t) where
  getAnnotation _ = starNotes


-- | Print `explicitAdminOracleContract`
--
-- @
--  printExplicitAdminOracleContract (Just "explicit_admin_oracle.tz") False
-- @
printExplicitAdminOracleContract :: Maybe FilePath -> Bool -> IO ()
printExplicitAdminOracleContract mOutput forceOneLine' =
  maybe TL.putStrLn Utf8.writeFile mOutput $
  printLorentzContract forceOneLine' $
    (defaultContract (explicitAdminOracleContract @MText))
      { cDisableInitialCast = True }

-- | Print `explicitAdminOracleContract`
--
-- @
--  printLambdaAdminOracleContract (Just "lambda_admin_oracle.tz") False
-- @
printLambdaAdminOracleContract :: Maybe FilePath -> Bool -> IO ()
printLambdaAdminOracleContract mOutput forceOneLine' =
  maybe TL.putStrLn Utf8.writeFile mOutput $
  printLorentzContract forceOneLine' $
    (defaultContract (lambdaAdminOracleContract @MText))
      { cDisableInitialCast = True }

-- | Print a `Lambda` to set the owner (admin) to the given `Address`
--
-- @
--  Tezos.Crypto.Orphans Text.Read> printSetOwnerLambda (read "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir")
--  { CDR; PUSH address "tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir"; PAIR; NIL operation; PAIR }
-- @
printSetOwnerLambda :: Address -> IO ()
printSetOwnerLambda newOwner =
  TL.putStrLn $
  printLorentzValue @LambdaAdmin forceOneLine $
  setOwnerLambdaAdmin newOwner
  where
    forceOneLine = True

-- | Print a `Lambda` to send the given amount of `Mutez` to the given `Address`
--
-- @
--  Tezos.Crypto.Orphans Text.Read> printSetPriceLambda (toMutez 2)
--  { CAR; PUSH mutez 2; SWAP; PAIR; NIL operation; PAIR }
-- @
printSetPriceLambda :: Mutez -> IO ()
printSetPriceLambda newPrice =
  TL.putStrLn $
  printLorentzValue @LambdaAdmin forceOneLine $
  setPriceLambdaAdmin newPrice
  where
    forceOneLine = True

-- | Print a `Lambda` to send the given amount of `Mutez` to the given `Address`
printSendMutezLambda :: Mutez -> Address -> IO ()
printSendMutezLambda amountToSend target =
  TL.putStrLn $
  printLorentzValue @LambdaAdmin forceOneLine $
  sendMutezLambdaAdmin amountToSend target
  where
    forceOneLine = True

