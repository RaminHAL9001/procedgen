-- | A module providing handy infix operators for constructing query functions in the Reader monad,
-- especially using lenses.
module Procedgen.ReaderLogic
  ( -- * Predicates
    PredicateOn, QueryLens(..),
    (?==), (?==?), (?/=), (?/=?), (?->), (?->>), (.&&.), (.||.),
    -- * Sections
    Section,
    top, top1, partBy,
    module Control.Monad.Reader
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List (partition)

----------------------------------------------------------------------------------------------------

-- | A function that selects some range of elements in a list, returning the pair of the selected
-- elements and the not selected elements.
type Section r = [r] -> ([r], [r])

top :: Int -> Section r
top = splitAt

top1 :: Section r
top1 = splitAt 1

partBy :: PredicateOn r -> Section r
partBy p = partition (runReader p)

----------------------------------------------------------------------------------------------------

type PredicateOn r = Reader r Bool

class Functor m => QueryLens r m | m -> r where
  queryLens :: Getter r a -> m a

instance Monad m => QueryLens r (ReaderT r m) where { queryLens = view; }
instance Monad m => QueryLens st (StateT st m) where { queryLens = use; }

-- | For example: @'Procedgen.Plot.plotLabel' '?==' "sinusoid"@ will select all plots that are labeled
-- with the exact string "sinusoid".
(?==) :: (Eq a, Applicative m, QueryLens r m) => Getter r a -> a -> m Bool
(?==) a b = (==) <$> queryLens a <*> pure b
infix 4 ?==

-- | Just like @('?==')@ but takes a 'Control.Lens.Getter' on as both arguments
(?==?) :: (Eq a, Applicative m, QueryLens r m) => Getter r a -> Getter r a -> m Bool
(?==?) a b = (==) <$> queryLens a <*> queryLens b
infix 4 ?==?

-- | For example: @'Procedgen.Plot.lineStyle' . 'Procedgen.Plot.lineColor' ?/= black@ will select all
-- plots that are not drawn with a black color.
(?/=) :: (Eq a, Applicative m, QueryLens r m) => Getter r a -> a -> m Bool
(?/=) a b = (/=) <$> queryLens a <*> pure b
infix 4 ?/=

-- | Just like @('?/=')@ but takes a 'Control.Lens.Getter' on as both arguments
(?/=?) :: (Eq a, Applicative m, QueryLens r m) => Getter r a -> Getter r a -> m Bool
(?/=?) a b = (/=) <$> queryLens a <*> queryLens b
infix 4 ?/=?

-- | For example: @'Procedgen.Plot.plotLabel' '?->' 'Strict.isInfixOf' ".diff"@ will select all plots
-- that 
(?->) :: (Eq a, Functor m, QueryLens r m) => Getter r a -> (a -> Bool) -> m Bool
(?->) lens f = f <$> queryLens lens
infixl 1 ?->

-- | Like @('?->')@ but takes a monadic function as the right-hand parameter.
(?->>) :: (Eq a, Monad m, QueryLens r m) => Getter r a -> (a -> m Bool) -> m Bool
(?->>) lens f = queryLens lens >>= f
infixl 1 ?->>

-- | Short-circuiting logical AND operator lifted into the Reader monad.
(.&&.) :: Reader r Bool -> Reader r Bool -> Reader r Bool
(.&&.) a b = a >>= \ ok -> if ok then b else pure False

-- | Short-circuiting logical OR operator lifted into the Reader monad.
(.||.) :: Reader r Bool -> Reader r Bool -> Reader r Bool
(.||.) a b = a >>= \ ok -> if ok then pure True else b
