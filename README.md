# Assignment 1: Recursion, Datatypes and Higher Order Functions (400 points)

## Overview

The overall objective of this assignment is to get some 
experience using the *core* features of functional programming,
namely, Recursion, Datatypes and Higher-Order functions.

The assignment is in the following files that you will modify

- [List.hs](/src/CSE230/List.hs)
- [Shapes.hs](/src/CSE230/Shapes.hs)
- [Doc.hs](/src/CSE230/Doc.hs)
- [Directory.hs](/src/CSE230/Directory.hs)

Finally, there are a`Test.hs` has some sample tests to be used  
to check your assignments before submitting.

- [test/Test.hs](/test/Test.hs)

You should **only modify** the parts of the files which say:

```haskell
error "fill this in"
```

with suitable Haskell implementations. 

**You are free to write and use any helper functions.**

## Instructions

### Assignment Testing and Evaluation

Your functions/programs **must** compile and run on the [VM][VM-URL].

Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error "fill me in"` with your 
partial solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

### Submission Instructions

To submit your code, just do:

```bash
$ make turnin
```

If you are working in a group, make sure to update the file `COLLABORATORS.md` 
with your group members, but **each person must submit on their own**.

### Installation Instructions for Ubuntu/Linux Users

You may have to do the equivalent of 

```
$ sudo apt-get install libxi-dev libxrandr-dev libxcursor-dev libxinerama-dev freeglut3-dev
```

to get the graphics stuff working. (Or just use the [VM image][VM-URL])

On Centos the relevant packages appear to be:

```
$ yum install libXi-devel libXrandr-devel libXcursor-devel libXinerama-devel freeglut
```

### Installation Instructions for WSL Users

**We strongly recommend using the VM for this assignment!**

If you're using the Windows Subsystem for Linux in Windows 10, 
there's additional steps required to get this code working for you:

  - The required OpenGL libraries are not supported in WSL 
    out of the box, and so you'll need to install `mesa-utils`. 

  - On some distributions that use the apt package manager, 
    such as `Pengwin`, you can type `sudo apt install mesa-utils`

  - You need to have an X window server installed and running 
    in Windows 10 as well, even though this assignment doesn't 
    use it. X410 is easy to use, but it is not free. 
    Xming is available for free. If WSL can't find the display, 
    try `export DISPLAY=:0`

  - Superuser priviliges are required within WSL, so instead of 
    `stack run`, type `sudo stack run --allow-different-user`

## Problem 1: Lists

### Cloning

Fill in the implementation of `clone` such that `clone n x` returns 
a list of `n` copies of `x`. When you are done, you should see the 
following behavior:

```haskell
>>> clone 5 'a'
"aaaaa"

>>> clone 3 "cat"
["cat","cat","cat"]
```

### Padding

Fill in the implementation of `pad` such that `pad dir n x ys` 
"pads" the list `ys` with as many copies of `x` as needed to make 
it exactly of size `n`. The `dir` parameter specifies whether the 
copies of `x` should be added at the beginning (`DirL`) or at the 
end (`DirR`).

When you are done, you should see the following behavior:

```haskell
>>> pad DirL 10 0 [1,2,3,4,5] 
[0,0,0,0,0,1,2,3,4,5]

>>> pad DirR 10 0 [1,2,3,4,5] 
[1,2,3,4,5,0,0,0,0,0]

>>> pad DirL 3 0 [1,2,3,4,5] 
[1,2,3,4,5]

>>> pad DirR 3 0 [1,2,3,4,5] 
[1,2,3,4,5]
```

### Sub-Sequence

Fill in the definition of `isSubsequence` such that 
`isSubSequence s1 s2` returns True if `s1` can be 
obtained by *deleting* some elements of `s2`.

When you are done, you should see the following behavior:

```haskell
>>> isSubSequence "cat" "dog"
False

>>> isSubSequence "cat" "craptasticdog"
True
```

### Maximum

Fill in the implementation of `maximum` so that `maximum d xs` 
returns the largest of `d:xs`.

When you are done, you should see the following behavior:

```haskell
>>> maximum 99 []
99

>>> maximum 99 [90, 100, 200, 52]
200
```

### Intersperse

Fill in the definition of `intersp` such that 
`intersp s [x1,x2,...,xn]` returns the list 
`[s, x1, s, x2, s, ..., xn, s]`. 

When you are done, you should see the following behavior:

```haskell
>>> intersp '|' "chewbacca"
"|c|h|e|w|b|a|c|c|a|"

>>> intersp "yo!" ["which", "way", "is", "the", "park"]
["yo!","which","yo!","way","yo!","is","yo!","the","yo!","park","yo!"]
```

### `iter` 

Fill in the definition of `iter` such that `iter n f x` 
returns the result of calling `f` on `x` exactly `n` times, e.g. 

- `iter 3 f x` returns `f (f (f x))`, and 
- `iter 5 f x` returns `f (f (f (f (f x))))`. 

When you are done you should get the following behavior:

```haskell
>>> iter 10 (\x -> 2 * x) 1
1024
```

## Problem 2: Shapes 

### Rainbow 

Fill in the implementation of `rainbow` so that when you 
are done, running 

```haskell
>>> mkRainbow
```

creates a file `img/rainbow.png` that looks identical to 

![Rainbow](/out/rainbow.png)

**HINT:** Read the documentation for `overlay` and `circle` from 
the [`Graphics.Htdp` library](http://hackage.haskell.org/package/htdp-image-1.1.0.0/docs/Graphics-Htdp.html).

### ChessBoard using `iter` 

Fill in the implementation of `chessBoard2` so that when you 
are done, running 

```haskell
>>> mkChess2
```

creates a file `img/chess2.png` that looks identical to 

![Chess Board](/out/chess2.png)

**HINT:** Make sure you understand the API in `chessBoard1`.

### Triangle using recursion

Fill in the implementation of `triRec` so that when you 
are done, running 

```haskell
>>> mkTriangle1
```

creates a file `img/triangle1.png` that looks identical to 

![Chess Board](/out/triangle1.png)

**HINT:** You may find the `Graphics.Htdp` functions `above` and `beside` useful.

### Triangle using `iter` 

Fill in the implementation of `sierpinskiTriangle2` so that when you 
are done, running 

```haskell
>>> mkTriangle2
```

creates a file `img/triangle2.png` that looks identical to 

![Chess Board](/out/triangle2.png)

**HINT:** Make sure you understand the relation between `iter` and recursion. 

### Carpet with `iter`

Fill in the implementation of `sierpinskiCarpet` so that when you 
are done, running 

```haskell
>>> mkCarpet
```

creates a file `img/carpet.png` that looks identical to 

![Chess Board](/out/carpet.png)

## Problem 3: Documents 

For this problem you will write a simple document layout engine, that allows the pretty printing of nested documents.

Thus, a document is a list of lines, each of which is a string. For example, the document

```
a
aa
aaa
aaaa
```

is represented as 

```haskell
>>> D ["a", "aa", "aaa", "aaaa"]
```

We have also provided implementations of methods for computing

* the `height` of a document (the number of lines)
* the `width` of a document (the maximum number of characters in a line)

### Vertical Concatenation aligned at Left

Fill in the implementation of `vcatL` such that `vcatL d1 d2`
*vertically concatenates* the documents `d1` and `d2` aligning
their *left* sides. 

When you are done, you should see the following behavior:

```haskell
>>> (doc "cat") `vcatL` (doc "horse") `vcatL` (doc "mongoose")
cat
horse
mongoose
```

### Vertical Concatenation aligned at Right

Fill in the implementation of `vcatR` such that `vcatR d1 d2`
*vertically concatenates* the documents `d1` and `d2` aligning
their *right* sides. 

When you are done, you should see the following behavior:

```haskell
>>> (doc "cat") `vcatR` (doc "horse") `vcatR` (doc "mongoose")
     cat
   horse
mongoose
```



### Horizontal Concatenation aligned at Top

Fill in the implementation of `hcatT` such that `hcatT d1 d2`
*horizontally concatenates* the documents `d1` and `d2` aligning 
their *top* sides.


Suppose you have the following `Doc` values:

```haskell
>>> aDoc
a
aaa
aaaaa
```

```haskell
>>> bDoc
b
bbb
```

```haskell
>>> lineDoc 
<----- HERE
```

When you are done with `hcatT`, you should see the following behavior:

```haskell
>>> hcatT aDoc lineDoc
a    <----- HERE
aaa  
aaaaa
```

and 

```haskell
>>> hcatT aDoc bDoc
a    b
aaa  bbb
aaaaa
```


### Horizontal Concatenation aligned at Bottom

Fill in the implementation of `hcatB` such that `hcatB d1 d2`
*horizontally concatenates* the documents `d1` and `d2` aligning 
their *bottom* sides.


When you are done you should see the following behavior:

```haskell
>>> hcatB aDoc lineDoc
a    
aaa  
aaaaa<----- HERE
```

and 

```haskell
>>> hcatB aDoc bDoc
a    
aaa  b
aaaaabbb
```

## Problem 4: `htree`

Finally, we will use `Doc` to build a command-line tool 
called `htree` which does two tasks.

*1. Showing the Sub-Directories*

At the shell, executing

```
$ htree -ls src
```

produces the following tree-representation of the `src` 
directory (of this repo).

```
src
├── CSE230
│   ├── Directory.hs
│   ├── Doc.hs
│   ├── Graphics.hs
│   ├── List.hs
│   └── Shapes.hs
└── Main.hs
```

*2. Finding Files Matching a Pattern*

At the shell, executing

```
$ htree -find src .hs
```

prints out the list of all the files (recursively) inside `src` 
that match the substring `.hs`:

```
src/CSE230/Directory.hs
src/CSE230/Doc.hs
src/CSE230/Graphics.hs
src/CSE230/List.hs
src/CSE230/Shapes.hs
src/Main.hs
```

### Directories

We represent a *directory* via a datatype 

```haskell
data Dir a 
    = Fil a             -- ^ A single file named `a`
    | Sub a [Dir a]     -- ^ A sub-directory name `a` with contents `[Dir a]`
```

For example, the files in the `src` directory can be represented as:

```haskell
srcDir :: Dir FilePath
srcDir = Sub "src" 
           [ Sub "CSE230" 
               [ Fil "Directory.hs"
               , Fil "Doc.hs"
               , Fil "Graphics.hs"
               , Fil "List.hs"
               , Fil "Shapes.hs"
               ]
            , Fil "Main.hs"
            ]
```

### `dirDoc`

Fill in the implementation of `dirDoc` that converts a `Dir FilePath`,
i.e. a directory where each name is a `FilePath` (i.e. `String`) into 
a `Doc` value (from the previous problem). Your code should only use
the *exported* functions of the `Doc` module, i.e. 

* the `doc` constructor, and
* the combinators `hcatT`, `hcatB`, `vcatL` and `vcatR`.

**HINT:** You may find `dash`, `stile` , `angle` and `bar` useful.

When you are done, you should see the following behavior:

```haskell
>>> dirDoc srcDir
src
├── CSE230
│   ├── Directory.hs
│   ├── Doc.hs
│   ├── Graphics.hs
│   ├── List.hs
│   └── Shapes.hs
└── Main.hs
```

```haskell
>>> dirDoc example
.
├── LICENSE
├── README.md
├── cse230-tree.cabal
├── out
│   ├── carpet.png
│   ├── chess1.png
│   ├── chess2.png
│   ├── rainbow.png
│   ├── triangle1.png
│   └── triangle2.png
├── src
│   ├── CSE230
│   │   ├── Directory.hs
│   │   ├── Doc.hs
│   │   ├── Graphics.hs
│   │   ├── List.hs
│   │   └── Shapes.hs
│   └── Main.hs
└── stack.yaml
```

### `allFiles`

Understand and use `foldDir` to fill in the implementation 
of `allFiles dir` which returns a list of *all the files* 
in the directory `dir`.

When you are done, you should see the following behavior:

```haskell
>>> allFiles example
["LICENSE","README.md","cse230-tree.cabal","carpet.png","chess1.png","chess2.png","rainbow.png","triangle1.png","triangle2.png","Main.hs","Directory.hs","Doc.hs","Graphics.hs","List.hs","Shapes.hs","stack.yaml"]
```


### `allDirs`

Understand and use `foldDir` to fill in the implementation of 
`allDirs dir` which returns a list of *all the sub-directories* 
in the directory `dir`.

When you are done, you should see the following behavior:

```haskell
>>> allDirs example
[".","out","src","CSE230"]
```


### `findFiles`

Understand and use `foldDir` to fill in the implementation of 
`findFiles sub dir` which returns a list of *all the files* 
in the directory `dir` such that `sub` is a *subsequence* 
of the files' names. 

When you are done, you should see the following behavior:

```haskell
>>> findFiles ".hs" example
["./src/CSE230/Directory.hs","./src/CSE230/Doc.hs","./src/CSE230/Graphics.hs","./src/CSE230/List.hs","./src/CSE230/Shapes.hs","./src/Main.hs"]

```

### `build`

Finally, complete the implementation of the function 
`build path` that recursively traverses the filesystem 
starting at `path` to build the `Dir FilePath` object 
describing the filesystem's contents at `path`. (You 
can ignore complexities like symbolic links etc.)

When you are done, you should see the following 
behavior (assuming you have not added extra files 
in your `src/` directory!)

```haskell
>>> build "src"
Sub "src" [Sub "CSE230" [Fil "Directory.hs", Fil "Doc.hs", Fil "Graphics.hs", Fil "List.hs", Fil "Shapes.hs"],Fil "Main.hs"]
```

Finally, at this point, `stack install` should build and
install a standalone executable `htree` that you can then 
run as described above:

```
$ htree -ls   src
```

and 

```
$ htree -find src .hs
```


[VM-URL]: https://drive.google.com/file/d/1BlYeSZPNVrxUu8jQWgUkquRBjE6wQww3/view?usp=sharing 
