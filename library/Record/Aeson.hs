-- |
-- This module exports instances of 'ToJSON' for all record types.
module Record.Aeson where

import BasePrelude
import Record.Types
import Record.Lens
import Data.Aeson.Types
import GHC.TypeLits
import Language.Haskell.TH
import Record.Aeson.TH


-- 
-- Produce something in the spirit of the following:
-- 
-- instance (KnownSymbol n1, KnownSymbol n2, ToJSON v1, ToJSON v2) => ToJSON (Record2 n1 v1 n2 v2) where
--   toJSON x =
--     object pairs
--     where
--       pairs =
--         [
--           (undefined :: FieldName n1) & \n -> (fromString (symbolVal n), toJSON (view (fieldLens n) x)),
--           (undefined :: FieldName n2) & \n -> (fromString (symbolVal n), toJSON (view (fieldLens n) x))
--         ]
return $ do
  arity <- [1 .. 24]
  return $
    let
      recordName =
        mkName $ showString "Record" $ show arity
      arityNumbers =
        enumFromTo 1 arity
      fieldNameVarNames =
        mkName . showString "n" . show <$> arityNumbers
      valueVarNames =
        mkName . showString "v" . show <$> arityNumbers
      instanceDec =
        InstanceD constraints headType decs
        where
          constraints =
            knownSymbolConstraints <> toJSONConstraints
            where
              knownSymbolConstraints =
                do
                  varName <- fieldNameVarNames
                  return $ mkClassP ''KnownSymbol [(VarT varName)]
              toJSONConstraints =
                do
                  varName <- valueVarNames
                  return $ mkClassP ''ToJSON [(VarT varName)]
          headType =
            AppT (ConT ''ToJSON) $
            foldl' AppT (ConT recordName) $
            map VarT $
            interlace fieldNameVarNames valueVarNames
            where
              interlace a b = join (zipWith (\a b -> [a, b]) a b)
          decs =
            [toJSONDec]
            where
              toJSONDec =
                FunD 'toJSON [clause]
                where
                  clause =
                    Clause [pat] body []
                    where
                      pat =
                        ConP recordName varPats
                        where
                          varPats =
                            map VarP valueVarNames
                      body =
                        NormalB exp
                        where
                          exp =
                            AppE (VarE 'object) $ ListE $ do
                              (valueVarName, fieldNameVarName) <- zip valueVarNames fieldNameVarNames
                              let
                                labelExp =
                                  AppE (VarE 'fromString) $
                                  AppE (VarE 'symbolVal) $
                                  SigE (VarE 'undefined) $
                                  AppT (ConT ''FieldName) $
                                  VarT fieldNameVarName
                                valueExp =
                                  AppE (VarE 'toJSON) $
                                  VarE valueVarName
                              return $ TupE $ [labelExp, valueExp]
      in instanceDec

