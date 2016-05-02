Tze Machine
==========

Tze Maschine ist eine Implementierung der Z-Maschine in Haskell.

## Who what when where why how

I am a reader of Eric Lippert's blog:
[Fabulous adventures in coding](https://ericlippert.com). There he is
implementing a [Z-Machine](https://en.wikipedia.org/wiki/Z-machine)
interpreter in OCaml. I decided to do the same in Haskell.

Not entirely new to FP I decided it would be a good project for
several reasons:

 - Bit fiddling!
 - It's a video game!
 - It's, as far as I know, one of the first virtual machines!
 - It would be something borderline useful!
 - Because Haskell
      
There are a few branches all named "blog *n*" where *n* is a
number. Each branch builds on top of the previous one. So if you
really want to learn something and/or build your own, I suggest you
follow the branches.
      
The code is written using literate Haskell. This boils down to having
more comments than code. Since I hope that this might be useful to
somebody someday somewhere on a distant star date I decided to
document it extensively.


## Building

I built this Z-machine with Cabal. So the usual instructions suffice:

 * `git clone https://github.com/m1dnight/tze-machine.git`
 * `cd tze-machine`
 * `cabal sandbox init` (optional)
 * `cabal install`
 * `cabal run`
 
