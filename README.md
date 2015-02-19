diagrams-html5
===============

diagrams-html5 is a HTML5 Canvas backend for diagrams based on the static-canvas
https://github.com/jeffreyrosenbluth/static-canvas package. 
Diagrams is a powerful, flexible, declarative domain-specific language for 
creating vector graphics, using the Haskell programming language.
It supports most features defined in [diagrams-lib].

[diagrams-lib]: http://hackage.haskell.org/package/diagrams%2Dlib

# Installation

```
cabal update && cabal install diagrams-html5
```

# Usage

A simple example that uses _diagrams-html5_ to draw a square.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.html5.CmdLine

b1 = square 20 # lw 0.002

main = mainWith (pad 1.1 b1)
```

Save this to file named `Square.hs` and compile this program:

```
ghc --make -threaded Square.hs
```

This will generate an executable which, when run dispays the resulting
diagrams to http://localhost:3000/

```
$ ./Square -w 750
