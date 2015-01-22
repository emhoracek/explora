exploration-game
================

A text exploration game engine I made to learn Haskell. Explore a map of places by typing directions. Note it's text exploration, not text adventure, because so far you can't interact with objects in the environment in any way. :)

installing the engine
---------------------

Right now, you need the Haskell platform installed on your computer to use this engine. Eventually, I would like to make it useable online. Once you have Haskell installed, install the game with:

```(sh)
clone https://www.github.com/emhoracek/exploration-game 
cd exploration-game
cabal sandbox init
cabal install
```

playing a game
--------------

After installing, `exploration-game example.exp` inside the game folder will open the example game.

Type directions or instructions like "south", "east", "north" or "west" to explore the map. There are also secret commands (try "xyzzy"!). 

making your own map
-------------------

You can make your own map by making a file named "places.exp". The map file should be formatted exactly like this:
```
1. Place Name 
   Description
   -> Direction (d, dir): 1, Another (a, another): 2
2. Another place
   Description
   -> Direction: 1
3. A place with no exits
   Description
```

In other words,
<ol><li>Start with a unique number for the place then a period.
<li>Type the place name.
<li>On a new line, add a description of the place.
<li>On the next like, type "->" and a list of the exits from the place, if any. An exit is a direction to go in, optional synonyms for that direction within parentheses, a colon and space, and the number of the place to which it leads. Note that right now, directions and synonyms have to the single words. I would love to fix this!
<li>Start a new line for each place on the map.</ol>

If you have any questions, email me at libby@daydrea.me or add an issue here!


