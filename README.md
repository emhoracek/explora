explora
=======

A text exploration game engine I made to learn Haskell. Explore a map of places by typing directions. Note it's text exploration, not text adventure, because so far you can't interact with objects in the environment in any way. :)

installing the engine
---------------------

Right now, you need the Haskell platform installed on your computer to use this engine. (Eventually, I would like to make it useable online.) Once you have Haskell installed, install the game with:

```(sh)
clone https://www.github.com/emhoracek/explora 
cd explora
cabal install
```

playing a game
--------------

After installing, starting `explora` inside the explora folder then pressing enter will open the example game. 

Type directions or instructions like "south", "east", "north" or "west" to explore the map. There are also secret commands (try "xyzzy"!). 

making your own map
-------------------

You can make your own map to explore and share with friends! The map file should be formatted exactly like this:
```
1. Place Name 
   Description
   -> Direction (d, dir): 1, Another (a, another): 2
2. Another place
   Description
   -> Direction: 1
3. A place with no exits
   Description
4. A dangerous room
   A room that kills the player.
   * kill player
```

In other words,
<ol><li>Start with a unique number for the place then a period.</li>
<li>Type the place name.</li>
<li>On a new line, add a description of the place.</li>
<li>Next you can optionally have an action that will happen on entering the place. Right now you can only "kill player" or "go (direction)". Just start the line with an asterisk ("*").
<li>On the next like, type "->" and a list of the exits from the place, if any. An exit is a direction to go in, optional synonyms for that direction within parentheses, a colon and space, and the number of the place to which it leads. Note that right now, directions and synonyms have to the single words. I would love to fix this!</li>
<li>Start a new line for each place on the map.</li></ol>

If you have any questions, email me at libby@daydrea.me or add an issue here!

contributing
------------

I would very much appreciate contributions of any sort! 

As a beginner eager to learn more about Haskell, I would also appreciate constructive criticism about how to make my program better.
