module CSE230.Directory where

import CSE230.Doc 
import CSE230.List hiding (Dir)
import qualified Data.List as L
import System.FilePath      (takeDirectory, takeFileName, (</>))
import System.Directory     (doesFileExist, listDirectory)

-------------------------------------------------------------------------------
-- | Top-level "main" function
-------------------------------------------------------------------------------
main :: [String] -> IO ()
main ["-ls", p]        = showDirectory (normalize p)
main ["-find", p, sub] = showMatchingFiles (normalize p) sub 
main args              = putStrLn (errorMsg args)

normalize :: FilePath -> FilePath
normalize p = takeDirectory (p </> ".")

errorMsg :: [String] -> String
errorMsg args = unlines 
  [ "Sorry, don't understand: " ++ unwords args ++ ". Try one of"
  , " *  'htree -ls <path> ' to render the directory of all files under <path>" 
  , " *  'htree -find <path> sub' to find all files matching 'sub' under <path>"
  ]

-------------------------------------------------------------------------------
-- | 'showDirectory path' builds and displays the 'Dir' rooted at 'path' 
-------------------------------------------------------------------------------
-- >>> showDirectory "src"
-- src
-- ├── CSE230
-- │   ├── Directory.hs
-- │   ├── Doc.hs
-- │   ├── Graphics.hs
-- │   ├── List.hs
-- │   └── Shapes.hs
-- └── Main.hs
-- <BLANKLINE>
--

showDirectory :: FilePath -> IO ()
showDirectory path = do
  dir <- build path 
  print (dirDoc dir)

-------------------------------------------------------------------------------
-- | 'showMatchingFiles path sub' finds the files matching 'sub' (deep) in the 
--   the directory rooted at 'path' and prints them out.
-------------------------------------------------------------------------------
-- >>> showMatchingFiles "src" ".hs"
-- src/CSE230/Directory.hs
-- src/CSE230/Doc.hs
-- src/CSE230/Graphics.hs
-- src/CSE230/List.hs
-- src/CSE230/Shapes.hs
-- src/Main.hs

showMatchingFiles :: FilePath -> String -> IO ()
showMatchingFiles path sub = do 
  dir   <- build path
  mapM_ putStrLn (findFiles sub dir)


-------------------------------------------------------------------------------
-- | The 'Directory' data type
-------------------------------------------------------------------------------
data Dir a 
    = Fil a             -- ^ A single file named `a`
    | Sub a [Dir a]     -- ^ A sub-directory name `a` with contents `[Dir a]`
    deriving (Eq, Show)

example :: Dir FilePath
example = Sub "." 
            [ Fil "LICENSE"
            , Fil "README.md"
            , Fil "cse230-tree.cabal" 
            , Sub "out" [ Fil "carpet.png"
                        , Fil "chess1.png"
                        , Fil "chess2.png"
                        , Fil "rainbow.png"
                        , Fil "triangle1.png"
                        , Fil "triangle2.png"
                        ]
            , Sub "src" [ Sub "CSE230" [ Fil "Directory.hs"
                                       , Fil "Doc.hs"
                                       , Fil "Graphics.hs"
                                       , Fil "List.hs"
                                       , Fil "Shapes.hs"
                                       ]
                        , Fil "Main.hs"
                        ]
            , Fil "stack.yaml"
            ]

srcDir :: Dir FilePath
srcDir = Sub "src"
         [ Sub "CSE230" [ Fil "Directory.hs"
                        , Fil "Doc.hs"
                        , Fil "Graphics.hs"
                        , Fil "List.hs"
                        , Fil "Shapes.hs"
                        ]
         , Fil "Main.hs"
         ]


-------------------------------------------------------------------------------
-- | Printing Directories
-------------------------------------------------------------------------------

-- >>> dirDoc example
-- .
-- ├── LICENSE
-- ├── README.md
-- ├── cse230-tree.cabal
-- ├── out
-- │   ├── carpet.png
-- │   ├── chess1.png
-- │   ├── chess2.png
-- │   ├── rainbow.png
-- │   ├── triangle1.png
-- │   └── triangle2.png
-- ├── src
-- │   ├── CSE230
-- │   │   ├── Directory.hs
-- │   │   ├── Doc.hs
-- │   │   ├── Graphics.hs
-- │   │   ├── List.hs
-- │   │   └── Shapes.hs
-- │   └── Main.hs
-- └── stack.yaml
-- <BLANKLINE>
-- >>> dirDoc srcDir
-- src
-- ├── CSE230
-- │   ├── Directory.hs
-- │   ├── Doc.hs
-- │   ├── Graphics.hs
-- │   ├── List.hs
-- │   └── Shapes.hs
-- └── Main.hs
-- <BLANKLINE>
--
dirDoc :: Dir FilePath -> Doc 
dirDoc (Fil f)    = doc f
dirDoc (Sub f ds) = 
  let 
    currentDoc = dirDoc (Fil f)
    currentHeight = length ds
    innerDocPad = foldr vcatL empty $ map (\x -> dirDocHelper x currentHeight) $ zip [1..] ds 
    innerDoc = foldr vcatL empty $ map dirDoc ds
  in 
    vcatL currentDoc $ hcatB innerDocPad innerDoc
    
    
dirDocHelper :: (Int, Dir FilePath) -> Int -> Doc
dirDocHelper (idx, (Fil _)) len = folderEnd $ idx == len
dirDocHelper (idx, (Sub _ ds)) len = 
  let 
    innerDoc = map dirDoc ds
    innerHeight = foldr (+) 0 (map height innerDoc)
    paddedDoc = foldr vcatL empty (replicate innerHeight bar)
  in 
    vcatL (folderEnd (idx == len)) paddedDoc

folderEnd :: Bool -> Doc
folderEnd isLast 
  | isLast = hcatT angle dash
  | otherwise = hcatT stile dash

-------------------------------------------------------------------------------
-- | Some useful 'Doc's--------------------------------------------------------
-------------------------------------------------------------------------------
dash :: Doc
dash = doc "── "

stile :: Doc
stile = doc "├"

angle :: Doc
angle = doc "└"

bar :: Doc
bar = doc "│"

-------------------------------------------------------------------------------
-- | A 'fold' for directories 
-------------------------------------------------------------------------------
data DirElem a = SubDir a | File a 

foldDir :: ([a] -> r -> DirElem a -> r) -> r -> Dir a -> r
foldDir f = go []  
  where
      go stk r (Fil a)    = f stk r (File a)  
      go stk r (Sub a ds) = L.foldl' (go stk') r' ds                          
        where 
            r'   = f stk r (SubDir a)  
            stk' = a:stk

-------------------------------------------------------------------------------
-- | 'allFiles dir' returns a list of all the Files in 'dir'
-------------------------------------------------------------------------------
-- >>> allFiles example
-- ["LICENSE","README.md","cse230-tree.cabal","carpet.png","chess1.png","chess2.png","rainbow.png","triangle1.png","triangle2.png","Directory.hs","Doc.hs","Graphics.hs","List.hs","Shapes.hs","Main.hs","stack.yaml"]

allFiles :: Dir FilePath -> [FilePath]
allFiles dir = reverse (foldDir f [] dir)
  where 
      f = go
        where
          go _ r (File a) = a:r
          go _ r (SubDir _) = r
-------------------------------------------------------------------------------
-- | 'allDirs dir' returns a list of all the sub-directories in 'dir'
-------------------------------------------------------------------------------
--
-- >>> allDirs example
-- [".","out","src","CSE230"]

allDirs :: Dir FilePath -> [FilePath]
allDirs dir = reverse (foldDir f [] dir)
  where
      f = go
        where 
          go _ r (File _) = r
          go _ r (SubDir a) = a:r


-------------------------------------------------------------------------------
-- | 'findFiles sub dir' returns a list of all the Files-with-paths in 'dir'
--   such that 'sub' is a substring of the Files' names. 
-------------------------------------------------------------------------------
--
-- >>> findFiles ".hs" example
-- ["./src/CSE230/Directory.hs","./src/CSE230/Doc.hs","./src/CSE230/Graphics.hs","./src/CSE230/List.hs","./src/CSE230/Shapes.hs","./src/Main.hs"]
--
findFiles :: String -> Dir FilePath -> [FilePath]
findFiles sub dir = reverse (foldDir f [] dir)
   where
      f = go
          where 
            go stk r (File a) = 
              if (isSubSequence sub a) then
                (foldl (\x y -> y </> x) a stk) : r
              else r
            go _ r (SubDir _) = r
    

-------------------------------------------------------------------------------
-- | 'build path' constructing the Directory on the filesystem rooted at 'path'
-------------------------------------------------------------------------------
--
-- >>> build "src"
-- Sub "src" [Sub "CSE230" [Fil "Directory.hs", Fil "Doc.hs", Fil "Graphics.hs", Fil "List.hs", Fil "Shapes.hs"],Fil "Main.hs"]
--

-- >>> listDirectory "./src/CSE230"
-- ["Doc.hs","Graphics.hs","List.hs","Directory.hs","Shapes.hs"]
--

build :: FilePath -> IO (Dir FilePath)
build path = build' [] path


build' :: [FilePath] -> FilePath -> IO (Dir FilePath)
build' stk ps =
  let fullPath = (foldr (</>) ps stk) in do
    isFile <- doesFileExist fullPath
    if isFile then return $ Fil $ takeFileName ps
    else do
      ls <- listDirectory fullPath
      lls <- mapM (\x -> build' (stk ++ [ps]) x) (L.sort ls)
      return $ Sub ps $ lls


lshow :: Doc -> [String]
lshow = lines . show