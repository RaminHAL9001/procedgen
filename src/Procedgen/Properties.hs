-- | A module containing properties common to many different data types, expressed as type classes
-- that define lenses. For example, when multiple data types provide animation control, there is an
-- 'Animated' property which provides the lenses 'animationControl' which is a lens that extracts or
-- updates a 'AnimationControl' data type from any data structure which contains one.
module Procedgen.Properties where

import           Control.Lens

import           Data.Time.Clock

----------------------------------------------------------------------------------------------------

data AnimationControl
  = AnimationControl
    { theAnimationIsRunning    :: Bool
    , theAnimationCurrentFrame :: NominalDiffTime
    }

class Animated obj where { animationControl :: Lens' obj AnimationControl; }
instance Animated AnimationControl where { animationControl = lens id $ flip const; }

makeAnimationControl :: AnimationControl
makeAnimationControl = AnimationControl
  { theAnimationIsRunning    = False
  , theAnimationCurrentFrame = 0.0
  }

-- | Get or set whether the animation should respond to events.
animationIsRunning :: Animated obj => Lens' obj Bool
animationIsRunning = animationControl .
  lens theAnimationIsRunning (\ a b -> a{ theAnimationIsRunning = b })
  
animRun :: Animated obj => Lens' obj Bool
animRun = animationIsRunning

animationCurrentFrame :: Animated obj => Lens' obj NominalDiffTime
animationCurrentFrame = animationControl .
  lens theAnimationCurrentFrame (\ a b -> a{ theAnimationCurrentFrame = b })

animFrame :: Animated obj => Lens' obj NominalDiffTime
animFrame = animationCurrentFrame

