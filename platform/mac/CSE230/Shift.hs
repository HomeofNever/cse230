module CSE230.Shift where 

import qualified Graphics.Gloss as G

shift :: Float -> (Float, Float) -> (Float, Float)
shift dim (x, y) = (x - (dim / 2), y + (dim / 2))

scale :: G.Picture -> G.Picture
scale p = G.scale 0.5 0.5 p
