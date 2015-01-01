exploration-game
================

A text exploration game I made to learn Haskell. Explore a map of places by typing directions. 

playing a game
--------------

Type directions or instructions like "south", "east", "north" or "west" to explore the map. There are also secret commands (try "xyzzy"!). 

making your own map
-------------------

You can make your own map by making a file named "places.exp". The map file should be formatted exactly like this:
```
1. Place Name 
   Description
   -> Direction: 1, Another Direction: 2
2. Another place
   Description
   -> Direction: 1
3. A place with no exits
   Description
```

In other words,
<ol><li>Start with a unique number for the place then a period.
<li>Type the place name ending with a period.
<li>On a new line, add a description of the place.
<li>On the next like, type "->" and a list of the exits from the place, if any. An exit is a direction to go in, a colon and space, and the number of the place to which it leads.
<li>Start a new line for each place on the map.</ol>

If you have any questions, email me at libby@daydrea.me or add an issue here!


