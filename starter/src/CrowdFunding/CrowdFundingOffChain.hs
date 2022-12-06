
{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-} -- To allow notation like Getparams {..}

module CrowdFunding.CrowdFundingOffChain where

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)

-- Plutus imports
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Ledger                                          (PaymentPubKeyHash, Value,POSIXTime)
import qualified Text.Printf              as TextPrintf (printf)
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger 


import qualified Ledger.Address               as Address

--Own modules
import qualified CrowdFunding.CrowdFundingOnChain as OnChain

data StartParams = StartParams
    { 
      spcrowdSourcer :: Ledger.PaymentPubKeyHash     -- part of parameter
    , sptargetAmount :: Ada.Ada                      -- part of parameter
    , spDeadline    :: !LedgerApiV2.POSIXTime        -- part of parameter
    , spcrowdRefunder :: Ledger.PaymentPubKeyHash
    , spDdata :: !Integer                              -- Datum for now not meaningful   
    , spAmount      :: !Integer            -- Ada at script - This will be the Contribution amount 
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data GrabParams = GrabParams 
    {
          gpcrowdSourcer :: Ledger.PaymentPubKeyHash      -- part of parameter
        , gpDeadline :: !LedgerApiV2.POSIXTime            -- part of parameter
        , gpTargetAmount :: Ada.Ada                       -- part of parameter
        , gpInt :: !Integer -- new
        , gpRunner :: Ledger.PaymentPubKeyHash
        , gpAction    :: !Integer                          -- Redeemer - 1 CrowdSource Beneficiary
                                                           --            2 Refund
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type CrowdSchema =
    PlutusContract.Endpoint "Start" StartParams
    PlutusContract..\/ PlutusContract.Endpoint "Grab" GrabParams 

start :: PlutusContract.AsContractError e => StartParams -> PlutusContract.Contract w s e ()
start sp = do
    let p = OnChain.CrowdParam
            {
                OnChain.crowdSourcer = spcrowdSourcer sp,
                OnChain.targetAmount = sptargetAmount sp,
                OnChain.deadline = spDeadline sp
            }
        d = OnChain.Dat { OnChain.ddata = spDdata sp, 
                        OnChain.crowdRefunder =  spcrowdRefunder sp }   -- dummy Datum for now
        v = Ada.lovelaceValueOf $ spAmount sp
        txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) (LedgerApiV2.Datum $ PlutusTx.toBuiltinData d) v
        -- txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) () v        
        lookups = Constraints.plutusV2OtherScript $ OnChain.validator p
        scriptAddress = OnChain.address p
        scriptHash = OnChain.validatorHash p

-- the final goal is to build and submit the transaction
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups txConstraints
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Start Endpoint - Submited Contribution amount - Value: %s ---------------------------" (P.show v)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Script with Address: %s and Hash: %s ---------------------------" (P.show scriptAddress) (P.show scriptHash)

grab :: forall w s. GrabParams -> PlutusContract.Contract w s DataText.Text ()
grab GrabParams{..} = do              -- the {..} lets us access the inside variables directly
    -- if i < 1
    --   then do
    --      --gpcrowdSourcer <- PlutusContract.ownFirstPaymentPubKeyHash
    --      PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline:- Now: "
    --     -- gpcrowdSourcer <- getPaymentPubKeyHash la
    --     -- 
    --   else 
    if (gpInt > 0)
      then do 
             gpcrowdSourcer <- getPaymentPubKeyHash gpRunner
             PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline: %s" (P.show gpDeadline)
      else do 
             gpcrowdSourcer <- PlutusContract.ownFirstPaymentPubKeyHash
             PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline: %s" (P.show gpDeadline)
             
    -- gpcrowdSourcer <- "97add5c3ca491534a1d81165f637d338e072d47ec6af8100463c4a1d"
    now <- PlutusContract.currentTime
    let param = OnChain.CrowdParam {
                        OnChain.crowdSourcer = gpcrowdSourcer
                      , OnChain.targetAmount = gpTargetAmount
                      , OnChain.deadline = gpDeadline
        }
        r = OnChain.Redeem { OnChain.redeemAction = gpAction }   -- not using yet just placeholder

    if (now < gpDeadline)  
        then 
            if ( gpAction == 2 ) 
                then 
                    -- do 
                    --   xx <- refundProcess param gpAction r now 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline: %s - Now: %s" (P.show gpDeadline) (P.show now)

                    -- if (now > gpDeadline)
                    --     then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline: %s - Now: %s" (P.show gpDeadline) (P.show now)
                    --     else PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline: %s - Now: %s" (P.show gpDeadline) (P.show now)
                else 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline already reached - No refund. Deadline: %s - Now: %s" (P.show gpDeadline) (P.show now)
        else do
        -- now deadline we need to Pass for CrowdSource Beneficiary to take funds. 
        -- Only after deadline we can do below for now.            
            -- let param = OnChain.CrowdParam {
            --             OnChain.crowdSourcer = gpcrowdSourcer
            --           , OnChain.targetAmount = gpTargetAmount
            --           , OnChain.deadline = gpDeadline
            --     }
            --     r = OnChain.Redeem { OnChain.redeemAction = gpAction }   -- not using yet just placeholder
            maybeutxo <- findUtxoInValidator param gpAction --finds the utxos associated to the CrowdSourcer that have passed deadline and can withdraw
            (allUtxos,amtUtxo) <- amountUtxoInValidator param gpAction
            -- (refundAllUtxos) <- refundUtxoInValidator param gpAction
            -- calculatedTarger <- valueToAdaList amtUtxo
            --PlutusContract.logInfo @P.String $ TextPrintf.printf "Amount in 1st utxo -: %s" (P.show $ valueToAdaList amtUtxo)
            --PlutusContract.logInfo @P.String $ TextPrintf.printf "Chuck- 1st utxo -: %s" (P.show (fst (head allUtxos)))
            --PlutusContract.logInfo @P.String $ TextPrintf.printf "Chuck- 2nd utxo -: %s" (P.show (sndHead allUtxos))
            -- && (gpTargetAmount > (valueToAdaList amtUtxo))
            case maybeutxo of
                Nothing -> do 
                             PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %s or not deadline reached %s and pubkey %s" (P.show r) (P.show $ now) (P.show $ OnChain.crowdSourcer param)
                             PlutusContract.logInfo @P.String $ TextPrintf.printf "Chuck -pubkeyParam = %s and pubKey own = %s" (P.show $ OnChain.crowdSourcer param) (P.show gpcrowdSourcer )
                Just (oref, o) -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Chuck -pubkeyParam = %s and pubKey own = %s" (P.show $ OnChain.crowdSourcer param) (P.show gpcrowdSourcer )
                    case ((valueToAdaList amtUtxo) > gpTargetAmount) of
                      False -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Target amount %s  not reached, actual = %s " (P.show gpTargetAmount) (P.show (valueToAdaList amtUtxo)) 
                      True -> do 
                        PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
                        let lookups = -- Constraints.unspentOutputs (Map.singleton oref o) P.<>
                                  -- Constraints.unspentOutputs (Map.singleton (fst (head allUtxos)) ( (snd $ head allUtxos))) P.<>
                                  Constraints.unspentOutputs (Map.fromList allUtxos) P.<>
                                  Constraints.plutusV2OtherScript (OnChain.validator param)
                            tx = joinTx allUtxos (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                                     Constraints.mustValidateIn (LedgerApiV2.from now) -- P.<>
                            -- tx = Constraints.mustSpendScriptOutput (fst (head allUtxos)) (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            --    Constraints.mustSpendScriptOutput (sndHead allUtxos) (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            --    Constraints.mustValidateIn (LedgerApiV2.from now) -- P.<>
                            
                        submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
                        Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                        PlutusContract.logInfo @P.String $ "collected gifts"

joinTx :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> ScriptsLedger.Redeemer -> Constraints.TxConstraints i o
-- joinTx [] r = mempty
joinTx [x] r =  Constraints.mustSpendScriptOutput (fst x) (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r)
joinTx (x:xs) r = Constraints.mustSpendScriptOutput (fst x) (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<> (joinTx xs r)

sndHead :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> LedgerApiV2.TxOutRef
sndHead xs =  fst $ head (tail xs) 

--getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getValue :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.Value
getValue (_, o) = LedgerTx._ciTxOutValue  o  

valueToAdaList :: [Ledger.Value] -> Ada.Ada
valueToAdaList [] = Ada.fromValue ada0
valueToAdaList [x] = Ada.fromValue x 
valueToAdaList (x : xs) = (Ada.fromValue x ) + valueToAdaList xs


getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    LedgerApiV2.Datum e <- snd datHashOrDatum
    
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Integer -> Bool
checkUTXO (oref,o) n = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Dat{..}
            | ddata == n -> True
            | otherwise  -> False

refundCheckUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Integer -> Bool
refundCheckUTXO (oref,o) n = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Dat{..}
            | ddata == n -> True
            | otherwise  -> False


ada0 :: Ledger.Value
ada0 = Ada.toValue 0

ada25 :: Ledger.Value
ada25 = Ada.toValue 25

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref,o)] n = do
    if checkUTXO (oref, o) n then 
        return (oref, o)
    else 
        Nothing
findUTXO ((oref,o):xs) n
    | checkUTXO (oref ,o)  n = return (oref, o)
    | otherwise = findUTXO xs n



refundFindUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
refundFindUTXO [] _ = Nothing
refundFindUTXO [(oref,o)] n = do
    if checkUTXO (oref, o) n then 
        return (oref, o)
    else 
        Nothing
refundFindUTXO ((oref,o):xs) n
    | checkUTXO (oref ,o)  n = return (oref, o)
    | otherwise = findUTXO xs n


getAmtUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> [(Ledger.Value)]
getAmtUTXO [] _ = [ada0]
getAmtUTXO [(oref,o)] n = [getValue (oref, o)]
getAmtUTXO ((oref,o):xs) n = getValue (oref, o) : getAmtUTXO xs n


refundUtxoInValidator :: OnChain.CrowdParam -> Integer -> PlutusContract.Contract w s DataText.Text ([(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)])
refundUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        outUtxos = xs    -- for now take all UTXOs
        -- Chuck 
        --v = P.printXs (xs, 0)
    --PlutusContract.logInfo @P.String $ TextPrintf.printf "Total utxos amount %s" (P.show v)
    return (outUtxos)
 

amountUtxoInValidator :: OnChain.CrowdParam -> Integer -> PlutusContract.Contract w s DataText.Text ([(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)],[Ledger.Value])
amountUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        outAmt = getAmtUTXO xs n
        outUtxos = xs    -- for now take all UTXOs
        -- Chuck 
        --v = P.printXs (xs, 0)
    --PlutusContract.logInfo @P.String $ TextPrintf.printf "Total utxos amount %s" (P.show v)
    return (outUtxos, outAmt)


findUtxoInValidator :: OnChain.CrowdParam -> Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs n
        -- Chuck 
        --v = P.printXs (xs, 0)
    --PlutusContract.logInfo @P.String $ TextPrintf.printf "Total utxos amount %s" (P.show v)
    return out


getPaymentPubKeyHash ::  Ledger.PaymentPubKeyHash -> PlutusContract.Contract w s DataText.Text (Ledger.PaymentPubKeyHash)
getPaymentPubKeyHash gpRunner = return gpRunner

getTotalValuePay :: LedgerTx.ChainIndexTxOut -> Ledger.Value
getTotalValuePay o = do
    Ada.toValue $ (Ada.fromValue $ LedgerTx._ciTxOutValue o) `Ada.divide` 10
    -- return tValue



-- refundProcess :: OnChain.CrowdParam -> Integer -> OnChain.Redeem -> Ledger.POSIXTime -> PlutusContract.Contract w s DataText.Text ()
-- refundProcess param gpAction r now = do 
-- -- refundProcess ::
--             maybeutxo <- findUtxoInValidator param gpAction --finds the utxos associated to the CrowdSourcer that have passed deadline and can withdraw
--             (allUtxos,amtUtxo) <- amountUtxoInValidator param gpAction
--             (refundAllUtxos) <- refundUtxoInValidator param gpAction
--             -- calculatedTarger <- valueToAdaList amtUtxo
--             --PlutusContract.logInfo @P.String $ TextPrintf.printf "Amount in 1st utxo -: %s" (P.show $ valueToAdaList amtUtxo)
--             --PlutusContract.logInfo @P.String $ TextPrintf.printf "Chuck- 1st utxo -: %s" (P.show (fst (head allUtxos)))
--             --PlutusContract.logInfo @P.String $ TextPrintf.printf "Chuck- 2nd utxo -: %s" (P.show (sndHead allUtxos))
--             -- && (gpTargetAmount > (valueToAdaList amtUtxo))
--             case maybeutxo of
--                 Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %s or not deadline reached %s and pubkey %s" (P.show r) (P.show $ now) (P.show $ OnChain.crowdSourcer param)
--                 Just (oref, o) -> do
--                     case ((valueToAdaList amtUtxo) > (gpTargetAmount param)) of
--                       False -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Target amount %s  not reached, actual = %s " (P.show (gpTargetAmount param)) (P.show (valueToAdaList amtUtxo)) 
--                       True -> do 
--                         PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
--                         let lookups = -- Constraints.unspentOutputs (Map.singleton oref o) P.<>
--                                   -- Constraints.unspentOutputs (Map.singleton (fst (head allUtxos)) ( (snd $ head allUtxos))) P.<>
--                                   Constraints.unspentOutputs (Map.fromList allUtxos) P.<>
--                                   Constraints.plutusV2OtherScript (OnChain.validator param)
--                             tx = joinTx allUtxos (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
--                                      Constraints.mustValidateIn (LedgerApiV2.from now) -- P.<>
--                             -- tx = Constraints.mustSpendScriptOutput (fst (head allUtxos)) (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
--                             --    Constraints.mustSpendScriptOutput (sndHead allUtxos) (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
--                             --    Constraints.mustValidateIn (LedgerApiV2.from now) -- P.<>
                            
--                         submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
--                         Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
--                         PlutusContract.logInfo @P.String $ "collected gifts"




-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () CrowdSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` grab') >> endpoints
    where 
        start' = PlutusContract.endpoint @"Start" start
        grab' = PlutusContract.endpoint @"Grab" $ grab 