exploration-game
================

A text exploration game I made to learn Haskell. Explore a map of places by typing directions. 

useage
------

### install
```sh
git clone https://github.com/emhoracek/exploration-game.git
cd exploration-game
cabal sandbox init && cabal install
```

### play
```sh
./dist/build/exploration-game/exploration-game
```

playing a game
--------------

Type directions or instructions like "south", "east", "north" or "west" to explore the map. There are also secret commands (try "xyzzy"!). 

making your own map
-------------------

You can make your own map by making a file named "places.exp". The map file should be formatted exactly like this:
```
{1. Place Name. "Description." [Direction: 1, Another Direction: 2]}
```

In other words,
1) Start with a curly brace for each place on the map.
2) Add a unique number to represent the place.
3) Type the place name ending with a period. This doesn't have to be unique.
4) Add a description of the place between quotation marks.
5) List the exits from the place between brackets. An exit is a direction to go in, a colon and space, and the number of place to which it leads.
6) Finish with another curly brace.
7) Start a new line for each place on the map.

If you have any questions, email me at libby@daydrea.me or add an issue here!


