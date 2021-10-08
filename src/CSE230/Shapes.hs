module CSE230.Shapes where

import Graphics.Htdp
import CSE230.List 
import CSE230.Graphics

-------------------------------------------------------------------------------
main :: IO ()
-------------------------------------------------------------------------------
main = do 
  mkRainbow
  mkChess1 
  mkChess2 
  mkTriangle1
  mkTriangle2
  mkCarpet

-------------------------------------------------------------------------------
-- | Rainbow with 'map' 
-------------------------------------------------------------------------------
mkRainbow :: IO ()
mkRainbow = save "img/rainbow.png" rainbow

rainbow :: Image
rainbow = foldr1 f xs
  where 
    xs  = map g [1..7] 
    f   = overlay
    g   = dwCircle 

-- >>> dwCircle 1 red

dwCircle :: Int -> Image
dwCircle i = circle (fromIntegral i * 20) solid $ rainbowColor i

-- >>> rainbowColor 6
-- RGBA 1.0 0.5 0.0 1.0

rainbowColor :: Int -> Color
rainbowColor i = [red, orange, yellow, green, aquamarine, blue, violet] !! (7 - i)

-------------------------------------------------------------------------------
-- | ChessBoard with 'clone'
-------------------------------------------------------------------------------
mkChess1 :: IO ()
mkChess1   = save "img/chess1.png"   chessBoard1

chessBoard1 :: Image
chessBoard1 = aboves (clone 4 row) 
  where 
    row     = besides (clone 4 gridSquare)

gridSquare :: Image
gridSquare = aboves [ besides [ whSq, blSq ]
                    , besides [ blSq, whSq ] ]
  where
    whSq   = square 50 solid bgCol
    blSq   = square 50 solid fgCol

-------------------------------------------------------------------------------
-- | ChessBoard with `iter`
-------------------------------------------------------------------------------
mkChess2 :: IO ()
mkChess2   = save "img/chess2.png"   chessBoard2

chessBoard2 :: Image 
chessBoard2 = iter 2 f base
  where
    f       = (\i -> besides [aboves[i, i], aboves[i, i]])
    base    = gridSquare 

-------------------------------------------------------------------------------
-- | Sierpinski Triangle with recursion
-------------------------------------------------------------------------------
mkTriangle1 :: IO ()
mkTriangle1 = save "img/triangle1.png" sierpinskiTriangle1

sierpinskiTriangle1 :: Image 
sierpinskiTriangle1 = triRec 8

triRec :: Int -> Image
triRec 0 = blueTriangle
triRec n = aboves [triRec (n - 1), (besides [triRec (n - 1), triRec (n - 1)])]

blueTriangle :: Image
blueTriangle = triangle 5 solid fgCol

-------------------------------------------------------------------------------
-- | Sierpinski Triangle with `iter`
-------------------------------------------------------------------------------
mkTriangle2 :: IO ()
mkTriangle2 = save "img/triangle2.png" sierpinskiTriangle2

sierpinskiTriangle2 :: Image
sierpinskiTriangle2 = iter 8 f base
 where
   f               = (\n -> aboves [n, (besides [n, n])])
   base            = blueTriangle 

-------------------------------------------------------------------------------
-- | Sierpinski Carpet with `iter`
-------------------------------------------------------------------------------
mkCarpet :: IO ()
mkCarpet   = save "img/carpet.png" sierpinskiCarpet

sierpinskiCarpet :: Image
sierpinskiCarpet = iter 4 f base 
  where 
    f            = singleSquare
    base         = blueSquare

blueSquare :: Image
blueSquare = square 4 solid fgCol  

gapSize :: Float
gapSize = 1

singleSquare :: Image -> Image
singleSquare img = 
  let 
    cp = carpetRow img False
    cpGap = carpetRow img True
    gap = carpetGap (width cp) gapSize
  in aboves [ 
              gap,
              cp,
              gap,
              cpGap,
              gap,
              cp,
              gap
            ]

carpetGap :: Float -> Float -> Image
carpetGap w h = rectangle w h solid white

carpetRow :: Image -> Bool -> Image
carpetRow img shouldMiddleGap = do
  let gap = carpetGap gapSize (height img) 
  if shouldMiddleGap then
    besides [ gap, img, gap, carpetGap (width img) (height img), gap, img, gap ]
  else 
    besides [ gap, img, gap, img, gap, img, gap ]

-- singleSquare img = aboves [ 
--                       besides [ img, img, img ],
--                       overlay (aboveAlign high img (besides [ img, img, img ])) (aboveAlign low img (besides [ img, img, img ]))
--                 ]