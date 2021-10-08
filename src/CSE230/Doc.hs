module CSE230.Doc 
  ( 
    -- * A document type 
    Doc 
    
    -- * Constructors
  , empty, doc 

    -- * Accessors
  , width, height

    -- * Example Doc
  , aDoc, bDoc, lineDoc, animals, triangles

    -- * Combinators
  , vcatL, vcatR, hcatB, hcatT

    -- * Properties
  , prop_hcatT_width  
  , prop_hcatT_height 
  , prop_hcatB_width  
  , prop_hcatB_height 
  , prop_vcatL_width  
  , prop_vcatR_width  
  , prop_vcatL_height 
  , prop_vcatR_height 
  
  ) where

import qualified Test.QuickCheck as QC
import           Prelude hiding (maximum)
import           CSE230.List

-------------------------------------------------------------------------------
-- | A 'Doc' is a 'String' list
-------------------------------------------------------------------------------
data Doc = D { docLines :: [String] }
  deriving (Eq, Ord)

empty :: Doc
empty = D []

doc :: String -> Doc
doc s = D (lines s)

aDoc :: Doc
aDoc = D [ "a"
         , "aaa"
         , "aaaaa"]

bDoc :: Doc
bDoc = D [ "b"
         , "bbb"] 

lineDoc :: Doc
lineDoc = doc "<----- HERE"

animals :: [Doc]
animals = [ doc "cat"
          , doc "horse"
          , doc "mongoose" 
          ]

-------------------------------------------------------------------------------
-- | Printing a Doc 
-------------------------------------------------------------------------------
-- >>> aDoc
-- a
-- aaa
-- aaaaa
-- <BLANKLINE>
--

instance Show Doc where
  show (D ls) = unlines ls 

-------------------------------------------------------------------------------
-- | Generating Random Docs
-------------------------------------------------------------------------------
instance QC.Arbitrary Doc where
  arbitrary = fmap D QC.arbitrary

-------------------------------------------------------------------------------
-- | Dimensions of a 'Doc'
-------------------------------------------------------------------------------
-- >>> width beers
-- 11

width :: Doc -> Int
width (D ls) = maximum 0 (map length ls) 

-- >>> height beers
-- 3

height :: Doc -> Int
height (D ls) = length ls

-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Left
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatL` (doc "horse") `vcatL` (doc "mongoose")
-- cat
-- horse
-- mongoose
-- <BLANKLINE>
--

vcatL :: Doc -> Doc -> Doc
vcatL d1 d2 = D ((docLines d1) ++ (docLines d2))
    

-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Right
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatR` (doc "horse") `vcatR` (doc "mongoose")
--      cat
--    horse
-- mongoose
-- <BLANKLINE>
--

vcatR :: Doc -> Doc -> Doc
vcatR d1 d2 = 
  let 
      len1 = width d1
      len2 = width d2
      diff = len1 - len2
      padDoc = \doc len -> D $ map (\x -> pad DirL (len + (length x)) ' ' x) (docLines doc)
  in
    if diff < 0 then vcatL (padDoc d1 (- diff)) d2
    else if diff > 0 then vcatL d1 (padDoc d2 diff)
    else vcatL d1 d2

-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Top
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------

-- >>> hcatT aDoc lineDoc
-- a    <----- HERE
-- aaa  
-- aaaaa
-- <BLANKLINE>
--
-- >>> hcatT aDoc bDoc
-- a    b
-- aaa  bbb
-- aaaaa
-- <BLANKLINE>
--
hcatT :: Doc -> Doc -> Doc
hcatT d1 d2 = 
  let 
    len = width d1
    strPad = \x y -> (pad DirR len ' ' x) ++ y
  in 
    D $ losslessZipWith "" "" strPad (docLines d1) (docLines d2)

-- >>> zipWith (+) [1,2,3] [1,2,3,4]
-- [2,4,6]

-- >>> losslessZipWith "l1" "l2" (++) ["1", "2", "3"] ["1", "2"]
-- ["11","22","3l2"]
--
losslessZipWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
losslessZipWith d1 d2 f (x:l1) (y:l2) = (f x y) : (losslessZipWith d1 d2 f l1 l2)
losslessZipWith _ d2 f l1 [] = zipWith f l1 (repeat d2) 
losslessZipWith d1 _ f [] l2 = zipWith f (repeat d1) l2

elongate :: Dir -> Int -> Doc -> Doc
elongate dir h (D ls) = D (pad dir h "" ls) 

-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Bottom
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------
-- >>> hcatB aDoc lineDoc
-- a    
-- aaa  
-- aaaaa<----- HERE
-- <BLANKLINE>
--
-- >>> hcatB aDoc bDoc
-- a    
-- aaa  b
-- aaaaabbb
-- <BLANKLINE>
--
hcatB :: Doc -> Doc -> Doc
hcatB d1 d2 = 
  let 
    len1 = height d1
    len2 = height d2
    elo = \x y -> elongate DirL x y
  in 
    hcatT (elo len2 d1) (elo len1 d2)

triangle :: Doc
triangle = D 
  [ "*"
  , "***"
  , "*****" ]

-- >>> foldr vcatL empty triangles
-- *
-- ***
-- *****
-- *    *
-- ***  ***
-- **********
-- *    *    *
-- ***  ***  ***
-- ***************
-- <BLANKLINE>
--

-- >>> foldr vcatR empty triangles
--           *
--           ***
--           *****
--      *    *
--      ***  ***
--      **********
-- *    *    *
-- ***  ***  ***
-- ***************
-- <BLANKLINE>
--
triangles :: [Doc]
triangles = [ triangle
            , triangle `hcatT` triangle
            , triangle `hcatT` triangle `hcatT` triangle ]

-------------------------------------------------------------------------------
-- | Properties of `Doc` combinators ------------------------------------------
-------------------------------------------------------------------------------

prop_hcatT_width  d1 d2 = width  (d1 `hcatT` d2) == width d1 + width d2
prop_hcatT_height d1 d2 = height (d1 `hcatT` d2) == max (height d1) (height d2)
prop_hcatB_width  d1 d2 = width  (d1 `hcatB` d2) == width d1 + width d2
prop_hcatB_height d1 d2 = height (d1 `hcatB` d2) == max (height d1) (height d2)
prop_vcatL_width  d1 d2 = width  (d1 `vcatL` d2) == max (width d1) (width d2)
prop_vcatR_width  d1 d2 = width  (d1 `vcatR` d2) == max (width d1) (width d2)
prop_vcatL_height d1 d2 = height (d1 `vcatL` d2) == height d1 + height d2
prop_vcatR_height d1 d2 = height (d1 `vcatR` d2) == height d1 + height d2
