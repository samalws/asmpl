module Compiler.Typechecker.Unify where

import Compiler.Typechecker.Types
import Compiler.Typechecker.Monads

import Control.Monad (unless, join, liftM2)
import Data.Maybe (fromMaybe)

simplifyType :: (UnifyMonad m) => Type -> m Type
simplifyType t@(VarType v) = fromMaybe t <$> lookupVar v
simplifyType (RecordsType l) = RecordsType <$> mapM (mapM simplifyRecordEntry) l
simplifyType t = pure t

simplifyRecordEntry :: (UnifyMonad m) => RecordEntryType -> m RecordEntryType
simplifyRecordEntry (RecordEntryType t c) = (\t' -> RecordEntryType t' c) <$> simplifyType t

bindSimplify :: (UnifyMonad m) => (Type -> m ()) -> Type -> m ()
bindSimplify f a = f =<< simplifyType a

bindSimplify2 :: (UnifyMonad m) => (Type -> Type -> m ()) -> Type -> Type -> m ()
bindSimplify2 f a b = join $ liftM2 f (simplifyType a) (simplifyType b)

-- assumes both inputted types were fed through simplifyType
unifyTypes_ :: (UnifyMonad m) => Type -> Type -> m ()
unifyTypes_ a b | a == b = pure ()
unifyTypes_ (VarType va) (VarType vb) = do
  tmpa <- varIsTemplateType va
  tmpb <- varIsTemplateType vb
  if (not tmpa) then assignVar va (VarType vb)
  else if (not tmpb) then assignVar vb (VarType va)
  else fail "Tried to unify two template types"
unifyTypes_ (VarType v) t = assignVar v t -- will fail if v is a template type; that's good, so we don't need to check for it here
unifyTypes_ t s@(VarType _) = unifyTypes_ s t
unifyTypes_ (RecordsType a) (RecordsType b) | length a /= length b || sum (length <$> a) /= sum (length <$> a) = fail "Tried to unify mismatched record types"
unifyTypes_ (RecordsType a) (RecordsType b) = sequence_ $ zipWith unifyRecordEntries (concat a) (concat b)
unifyTypes_ _ _ = fail "Tried to unify mismatched types"

unifyRecordEntries :: (UnifyMonad m) => RecordEntryType -> RecordEntryType -> m ()
unifyRecordEntries (RecordEntryType ta ca) (RecordEntryType tb cb) | ca /= cb = fail "Mismatched record entry constraints"
unifyRecordEntries (RecordEntryType ta ca) (RecordEntryType tb cb) = unifyTypes ta tb

resolveMemberTypeConstraint_ :: (UnifyMonad m) => Type -> RecordMember -> Type -> m ()
resolveMemberTypeConstraint_ _ (RecordMember i) _ | i < 0 = fail "Negative record lookup"
resolveMemberTypeConstraint_ (RecordsType r) (RecordMember i) t = mapM_ resolveIndividual_ r where
  resolveIndividual_ l | i >= length l = fail "Invalid record lookup"
  resolveIndividual_ l = resolveEntry_ (l !! i)
  resolveEntry_ (RecordEntryType s _) = unifyTypes s t
resolveMemberTypeConstraint_ (VarType _) _ _ = pure () -- hopefully we can loop around
resolveMemberTypeConstraint_ _ _ _ = fail "Failed to unify member type constraint on non record type"

-- past here we can assume that all vars should be resolved...

resolveGeqConstraint_ :: (UnifyMonad m) => Type -> Type -> m ()
resolveGeqConstraint_ a b | a == b = pure ()
-- for comparing 2 record types:
-- s >= t: we can assign s <- t
-- x|y >= x
-- {x} >= {x,y} TODO not doing this one yet
-- for every entry eb in b there must exist an entry ea in a such that ea == eb (eventually, once we add the {x}>={x,y} rule, the condition will be: ...such that ea is a prefix of eb)
resolveGeqConstraint_ (RecordsType a) (RecordsType b) | all (`elem` a) b = pure ()
resolveGeqConstraint_ _ _ = fail "Failed to resolve >= constraint"

resolveNumericConstraint_ :: (UnifyMonad m) => Type -> m ()
resolveNumericConstraint_ (IntType{}) = pure ()
resolveNumericConstraint_ FloatType = pure ()
resolveNumericConstraint_ DoubleType = pure ()
resolveNumericConstraint_ (VarType v) = do
  assumed <- varAssumedNumeric v
  unless assumed $ fail "Typevar not assumed numeric"
resolveNumericConstraint_ _ = fail "Failed is-numeric constraint"

resolveIntConstraint_ :: (UnifyMonad m) => Type -> m ()
resolveIntConstraint_ (IntType{}) = pure ()
resolveIntConstraint_ (VarType v) = do
  assumed <- varAssumedInt v
  unless assumed $ fail "Typevar not assumed int"
resolveIntConstraint_ _ = fail "Failed is-int constraint"

-- AUTO-GEN:

unifyTypes :: (UnifyMonad m) => Type -> Type -> m ()
unifyTypes = bindSimplify2 unifyTypes_

resolveEqConstraint :: (UnifyMonad m) => Type -> Type -> m ()
resolveEqConstraint = unifyTypes

resolveMemberTypeConstraint :: (UnifyMonad m) => Type -> RecordMember -> Type -> m ()
resolveMemberTypeConstraint a b c = bindSimplify2 (\a' c' -> resolveMemberTypeConstraint_ a' b c') a c

resolveGeqConstraint :: (UnifyMonad m) => Type -> Type -> m ()
resolveGeqConstraint = bindSimplify2 resolveGeqConstraint_

resolveNumericConstraint :: (UnifyMonad m) => Type -> m ()
resolveNumericConstraint = bindSimplify resolveNumericConstraint_

resolveIntConstraint :: (UnifyMonad m) => Type -> m ()
resolveIntConstraint = bindSimplify resolveIntConstraint_
