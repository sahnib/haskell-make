# haskell-make
This is a simple GNU "Make" command implementation in haskell. I aim to get my hands dirty o haskell with this implementaion.

[![Build Status](https://travis-ci.org/sahnib/haskell-make.svg?branch=master)](https://travis-ci.org/sahnib/haskell-make)

## Build Steps:
    $ clone the repo; 
    $ cd haskell-make;
    
    $ cabal sandbox init
    $ cabal build

You need at least GHC 7.8 to build the project.

## Current status:
- parses the Makefile and creates a dependency tree. 
