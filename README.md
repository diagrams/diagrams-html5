diagrams-html5
===============

diagrams-html5 is an HTML5 Canvas backend for **diagrams** based on the static-canvas
https://github.com/jeffreyrosenbluth/static-canvas package.

**diagrams** is a powerful, flexible, declarative domain-specific language for 
creating vector graphics, using the Haskell programming language.

[diagrams-lib]: http://hackage.haskell.org/package/diagrams%2Dlib

# Installation

```
cabal update && cabal install diagrams-html5
```

# Usage

A simple example that uses _diagrams-html5_ to draw a the Swirpinski triangle.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.Html5.CmdLine

sierpinski 1 = eqTriangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
  where s = sierpinski (n-1)

example :: Diagram B
example = sierpinski 7 # center # lw none # fc black

main = mainWith $ example # frame 0.1
```

Save this to file named `Sierpinski.hs` and compile this program:

```
ghc Sierpinski.hs
```

This will generate an executable which, when run creates an html file
containing the HTML5 Canvas code to generate the diagram.

```
$ ./Sierpinski -o sierpinski.html -w 750