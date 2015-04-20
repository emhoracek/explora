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

```(sh)
explora
```

will start Explora.

The engine will ask what game you want to play. You can type any path relative to the directory you're currently in. So, if you start Explora in the same directory as your game, you can simply type the name of your game file.

Want to try an example game? If you start Explora inside the root "explora" folder, leave the space blank, and just hit Enter, the main example game will start.

To play, type directions or instructions like "south", "east", "north" or "west" to explore the map. There are also secret commands (try "xyzzy"!). 

To end the game, hit CTRL + C.

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
<li>Next you can optionally have an action that will happen on entering the place. Right now you can only "kill player" or "go (direction)". Just start the line with an asterisk ("*").</li>
<li>On the next like, type "->" and a list of the exits from the place, if any. An exit is a direction to go in, optional synonyms for that direction within parentheses, a colon and space, and the number of the place to which it leads. Note that right now, directions and synonyms have to the single words. I would love to fix this!</li>
<li>Start a new line for each place on the map.</li></ol>

If you have any questions, email me at libby@daydrea.me or add an issue here!

contributing
------------

I would very much appreciate contributions of any sort! 

As a beginner eager to learn more about Haskell, I would also appreciate constructive criticism about how to make my program better.

I've tried really hard to document the code and make it clear and easy to understand. If something doesn't make sense, that's probably because it wasn't a good idea and I don't know how to fix it. :) 

The code is organized like this:

* executable -- the main executable.
* library -- all the modules 
  * Actions -- Anything related to actions the player can take in the game
  * Dictionary -- The words the games can understand
  * DIYGraph -- This is a little inductive functional graph library, based on fgl (here) and this blog post (here)
  * Game -- The game data type
  * Graph -- The game's graph data structure
  * Init -- The initialization of the game.
  * Input -- How the player's input is understand as a verb and subject
  * Items -- The item data structure
  * Loop -- The main game loop
  * Parse -- Parses the game file
  * Places -- The place data type
  * Player -- The player data type
  * Properties -- Functions the change the properties of items
  * Response -- The response data type
* test-suite -- most of the libraries have tests with the name of the library plus "Spec". So the tests for Items are in ItemsSpec. There's also Spec.hs, which is what automatically finds all those tests, and two modules full of small samples (tiny "game files" and "games" that are easier to test).

To install with tests,
```(sh)
cabal install --enable-tests
```

To run the tests alone, 
```(sh)
runhaskell -itest-suite -ilibrary test-suite/Spec.hs
```

How does explora work? First, the game file is parsed and turned into lists of 
places and a dictionary. Then, the list of places is turned into a directed 
graph. A default player, the graph, and the dictionary together make up the 
game state. The game starts with the first node in the graph, and as the player 
"moves" through the places in the game, the interpreter shows one part of 
the graph at a time. When the player picks up or drops an object, that changes 
the player's inventory and the place's inventory. 

If this seems a little convoluted, it's because I wrote this game while 
learning Haskell and without understanding any of the more complicated things 
like monads or applicatives or anything like that. So, I could have used the 
State monad or Threads or something, and lenses would probably make a lot of 
the data field access much easier. However, trying to make a game like this 
with as little state as possible was a very educational exercise!

Luckily, the simplicity of the interpreter means it should be easy for even 
absolute beginners to hack on, so please fork and try it out!!

If you have any questions, please don't hesitate to email me at libby@daydrea.me

Happy hacking!! <3 Libby
