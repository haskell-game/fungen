# **FunGEn** (**Fun**ctional **G**ame **En**gine)

<a href="https://github.com/haskell-game/fungen/blob/master/examples/hello.hs#L1"><img border=0 src="site/logo.gif" title="Click to see the hello world example" style="margin-top:2em;"></a>

FunGEn is a BSD-licensed, cross-platform,
OpenGL/GLUT-based, non-FRP game engine/framework written in
Haskell. Created by Andre Furtado in 2002, it's the oldest Haskell
game engine, and with very few dependencies and two example games,
it's one of the easiest ways to get started with Haskell game
development. It provides:

* Initialization, updating, removing, rendering and grouping
  routines for game objects
* Definition of a game background (or map), including texture-based
  maps and tile maps
* Reading and intepretation of the player's keyboard and mouse input
* Collision detection
* Time-based functions and pre-defined game actions
* Loading and displaying of 24-bit bitmap files
* Some debugging and game performance evaluation facilities
<!-- * Sound support (windows only, not in current release) -->

[Simon Michael](http://joyful.com) provides infrequent basic maintenance for
this package. If you'd like to take it over, contact me (`sm` on the #haskell-game IRC channel).

<a href="https://github.com/haskell-game/fungen/blob/master/examples/pong/pong.hs#L1"><img border=0 src="site/pong.png" title="Click to see the pong example" style="margin-top:2em;"></a>
<a href="https://github.com/haskell-game/fungen/blob/master/examples/worms/worms.hs#L1"><img border=0 src="site/worms.png" title="Click to see the worms (snake) example" style="margin-top:1em;"></a>

## Install
### Latest released:
([changes](http://hackage.haskell.org/package/FunGEn/changelog))
```
$ stack install FunGEn  # or cabal
```
Make sure `~/.local/bin` (stack) or `~/.cabal/bin` (cabal) or the windows equivalent are in your shell PATH, then run the example games:
```
$ fungen-hello
$ fungen-pong
$ fungen-worms
```
### Latest unreleased:
([changes](https://github.com/haskell-game/fungen/blob/master/CHANGES.md#L1))
```
$ git clone http://github.com/haskell-game/fungen
$ cd fungen
$ stack build  # or cabal
$ stack exec fungen-hello
$ stack exec fungen-pong
$ stack exec fungen-worms
``````


## Docs

- https://github.com/haskell-game/fungen#readme
- Andre's tutorial, [original](https://htmlpreview.github.io/?https://github.com/haskell-game/fungen/blob/master/site/example.html) or [reformatted](TUTORIAL.md#fungen-pong-tutorial) version
- [Haddock API docs](https://hackage.haskell.org/package/FunGEn/docs/Graphics-UI-Fungen.html)
- [Changelog](CHANGES.md)
- [FunGEn package](http://hackage.haskell.org/package/FunGEn) on Hackage
- [FunGEn questions](http://stackoverflow.com/search?tab=newest&q=fungen) on Stack Overflow
- [Andre's old site](http://www.cin.ufpe.br/~haskell/fungen)
- [#haskell-game](http://webchat.freenode.net/?channels=haskell-game) IRC channel
  <!-- ([log](http://ircbrowse.tomsmeding.com/day/haskell-game/today/recent)) -->
- [/r/haskellgamedev](http://www.reddit.com/r/haskellgamedev) reddit
- [haskell-cafe](http://www.haskell.org/haskellwiki/Mailing_lists) mail list
- Haskell wiki: [Game_Development](http://www.haskell.org/haskellwiki/Game_Development)
- Haskell wiki: [OpenGL tutorial](http://www.haskell.org/haskellwiki/OpenGLTutorial1)



## History

Excerpts from Andre's website in 2002:

**What is a game engine?**

A game engine can be considered as a library that provides game facilities
to a game programmer. When using a game engine, the programmer must
specify when the game events happen, rather than how they are
implemented. A same functionality may have its implementation varying from
platform to platform, in the case the engine is platform-independent. The
main advantage of a game engine is that it can be reused to the
development of many different kind of games, in an automated way, saving a
lot of programming time.

**Why Haskell?**

We believe that Haskell is a great language to develop games, because of
its high level of abstraction and the generation of a more concise,
elegant and shorter code. This is great for code maintenance and
understanding. Combining the power of Haskell with the facilities provided
by game engines seems a promising project. You can find more info on
Haskell in its official site.

**What is HOpenGL?**

HOpenGL stands for Haskell Open Graphics Library. Actually, it is a
binding to one of the most famous graphics libraries around the world
(OpenGL) and its auxiliary toolkit (GLUT). In other words, it makes
possible to call OpenGL/GLUT routines (which were written in the C
language) when programming in Haskell. You can find more info on HOpenGL
in my HOpenGL Tutorial site, or in its official site.

**<s>Current</s> 2002 Status**

Some feedback indicated that the first version of FunGEn was not as "functional" as it was desired: some game issues were still being dealt through an imperative fashion. This way, the authors of this project decided to change the game engine philosophy: programmers should describe a game as a set of "specifications" rather than defining its behavior imperatively. One plausible alternative for accomplishing this task is porting the Clean Game Library (CGL) to Haskell, adding some FunGEn specific features. Hence, this is the actual status of the FunGEn project: it is being rebuilt in order to provide game programming mechanisms following the CGL concepts. This really demands some time, but the authors expect a new version to be released soon.

... PLEASE NOTE: this is the very first version of FunGEn, and it was released just to get some feedback from game programmers. You are strongly invited to <A HREF="mailto:awbf@cin.ufpe.br">tell</A> your game programming experiences with FunGEn, helping us to release a definitive, stable version). Ok, after this disclaimer, please fell yourself free to take a quick tour in the site; it contains a lot of useful information for those who are really interested in trying a new game programming experience. Nice coding...

**Todo**

Here you have a list of some upcoming FunGEn features, and some other
desired features (but with no implementation prevision yet).

- Support map scrolling (coming soon);
- Support mouse input management (coming soon);
- Make a polygon map definition avaiable (coming soon);
- Make sound avaible to non-Win32 platforms;
- Create, if possible, some operators to avoid the excessive (x <- ...) syntax;
- Support auto-animated objects;
- Create a GLUT independent font support (or perhaps extend it);
- Improve the installation process;
- Upgrade FunGEn to be both a 2D (bidimensional) and 2D 1/2 (bi and a half dimensional) engine;
- Create a map editor/generator (possibly in other language, or using the brand new Haskell GUI...);
- Take courage to start thinking about the 3D world...

Would you like to suggest a feature? Feel free to do it. Would you like to
implement a feature? Please do it! Keep in touch.

**Credits**

FunGEn was created by Andre Furtado, Computation Science graduation
student at the Informatics Center (CIn) of the Federal University of
Pernambuco (UFPE), as part of a Scientific Iniciation (PIBIC/CNPq)
research project (Creating a Game Platform Using Haskell), oriented by
lecturer Andre Santos (PhD, 1995, University of Glasgow), who was
responsible for figuring out a lot of FunGEn implementation details.

I would like to thank also the following people who contributed for the development of FunGEn:

- Sven Panne
- Jay Cox
- Geber Ramalho
- Carlos Andre Pessoa
- Charles Madeira
- Monique Monteiro
- The people at the Haskell mailing lists

FunGEn can be distributed freely, in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE. I would thank you if you cite my name
and this site if you are going to use FunGEn for other things besides home
programming.
