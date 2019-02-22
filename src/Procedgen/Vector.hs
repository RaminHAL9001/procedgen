-- | This module defines an efficient 'Data.Vector.Vector' building monad that is evaluated in the
-- 'Control.Monad.ST.ST' monad. This is useful for constructing immutable referentially transparent
-- 'Data.Vector.Vector's with O(1) index updates.
--
-- As a reminder, functions evaluated in the 'Control.Monad.ST.ST' monad are strictly evaluated, not
-- lazy as is with all other Haskell data types. The 'Control.Monad.ST.ST' monad also uses clever
-- typing (with the @forall s@ type variable) to create a specially isolated evaluation environment
-- of 'Control.Monad.ST.ST' such that nothing outside of the 'Control.Monad.ST.ST' monad can be
-- changed, and as a result the data type 'Control.Monad.return'-ed can be considered a pure,
-- referentially transparent value.
--
-- You can think of the time taken to evaluate the 'Control.Monad.ST.ST' typed function as a sort of
-- "cool-down" time for 'Data.Vector.Vector' construction. Before evaluation of
-- 'Control.Monad.ST.runST' has completed, the 'Data.Vector.Vector' is still "warm" and easily
-- mutable with O(1) updates. After the 'Control.Monad.ST.ST' monad has returned it's final value,
-- the 'Data.Vector.Vector' freezes, the and 'Control.Monad.ST.ST' environment dissapears leaving
-- the outside world untouched with absolutely no side-effects. Therefore the final
-- 'Data.Vector.Vector' returned is actually a pure value and does not need to be wrapped in a
-- monadic 'Control.Monad.ST.ST' data type (unlike @IO@ types which can never be unwrapped). It is
-- as if the mutable 'Data.Vector.Vector' was created atomically
module Procedgen.Vector where
