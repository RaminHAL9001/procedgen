-- | Defines a typeclass of lazy functional data structures that can be "collapsed", meaning
-- serialized or marshalled, to an in-memory 'UVec.Vector'. These lazy functional data structures
-- must also have a method for being reconstructed from a 'UVec.Vector'. This allows lazy functional
-- data structures to be explicitly stored as a contiguous block of 'UVec.Unbox'ed 'UVec.Vector'
-- elements in memory, which takes a heavy load off of the garbage collector.
module Procedgen.Collapsible where

import           Procedgen.VectorBuilder

import           Control.Monad.ST

import qualified Data.Vector.Unboxed               as UVec

----------------------------------------------------------------------------------------------------

-- | Declares a type @a@ to have an unboxed 'Data.Vector.Unboxed.Vector' of @elem@ elements so that
-- @a@ can be frozen into a contiguous block of memory and therefore requiring considerably fewer
-- thunk allocations and garbage collection cycles.
class Collapsible elem a | a -> elem where
  collapse   :: a -> VectorBuilder UVec.MVector elem (ST s) ()
  uncollapse :: UVec.Vector elem -> a

