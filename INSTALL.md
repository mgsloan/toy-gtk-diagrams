Checkout Diagrams:
```sh
darcs get http://patch-tag.com/r/byorgey/dubl-tree
darcs get http://patch-tag.com/r/byorgey/diagrams-core
darcs get http://patch-tag.com/r/byorgey/diagrams-lib
darcs get http://patch-tag.com/r/byorgey/diagrams-cairo
darcs get http://patch-tag.com/r/byorgey/active
darcs get http://patch-tag.com/r/byorgey/force-layout
darcs get http://patch-tag.com/r/byorgey/diagrams-contrib
```

Checkout Toy:
```sh
git clone git@github.com:mgsloan/gtk-toy.git
git clone git@github.com:mgsloan/gtk-toy-diagrams.git
```

Build it all:
```sh
cd monoid-extras
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../dubl-tree
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../diagrams-core
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../diagrams-lib
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../diagrams-cairo
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../active
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../force-layout
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../diagrams-contrib
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../gtk-toy
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install

cd ../gtk-toy-diagrams
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
```
