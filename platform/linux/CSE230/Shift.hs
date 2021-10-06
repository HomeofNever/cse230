module CSE230.Shift where 

import qualified Graphics.Gloss as G

shift :: Float -> (Float, Float) -> (Float, Float)
shift _ (x, y) = (x, y)

scale :: G.Picture -> G.Picture
scale = id
