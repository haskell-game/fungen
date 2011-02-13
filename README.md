# FunGEN - Functional Game Engine

FunGEn (Functional Game Engine) is a BSD-licensed 2D platform-independent
game engine implemented in and for Haskell, using HOpenGL. It is intended
to help game programmers in the game development process, in a faster and
automated way. Actually, FunGEn supports:

- Initialization, updating, removing, rendering and grouping routines for game objects;
- Definition of a game background (or map), including texture-based maps and tile maps;
- Reading and intepretation of the player's keyboard input;
- Collision detection;
- Time-based functions and pre-defined game actions;
- Loading and displaying of 24-bit bitmap files;
- Debugging and game performance evaluation facilities;
- Sound support (actually for windows platforms only... :-[ );
- Hope to expand this list soon :-]

Original home: http://www.cin.ufpe.br/~haskell/fungen

New repo:      http://darcsden.com/simon/fungen

Hackage:       http://hackage.haskell.org/package/FunGEn

Copyright:     ./LICENSE

This README includes most but not all of the docs from the original site, lightly updated.
In particular, see also the tutorial at http://www.cin.ufpe.br/~haskell/fungen/example.html .

## History

* ghc 6.12-updated version published on darcsden by Simon Michael (2011/02)

* Cabalised, ghc 6.8/6.10-updated 0.1 released on hackage by Miloslav Raus (2008/09)

    *Tested under Win32 & Linux/Intel. Known glitches: Flickering under
    linux (at least on my shitty laptop). Weird pong paddle behavior under
    Win32.*

* Repo-ised, ghc 6.8-updated version by Simon Michael (2008/02)

* 0.1 released by Andre Furtado (2002)

    *Current Status:Some feedback indicated that the first version of
    FunGEn was not as "functional" as it was desired: some game issues
    were still being dealt through an imperative fashion. This way, the
    authors of this project decided to change the game engine philosophy:
    programmers should describe a game as a set of "specifications" rather
    than defining its behavior imperatively. One plausible alternative for
    accomplishing this task is porting the Clean Game Library (CGL) to
    Haskell, adding some FunGEn specific features. Hence, this is the
    actual status of the FunGEn project: it is being rebuilt in order to
    provide game programming mechanisms following the CGL concepts. This
    really demands some time, but the authors expect a new version to be
    released soon.*

    *FunGEn v1.0 can be downloaded here. (PLEASE NOTE: this is the very
    first version of FunGEn, and it was released just to get some feedback
    from game programmers. You are strongly invited to tell your game
    programming experiences with FunGEn, helping us to release a
    definitive, stable version). Ok, after this disclaimer, please fell
    yourself free to take a quick tour in the site; it contains a lot of
    useful information for those who are really interested in trying a new
    game programming experience. Nice coding...*

## Credits

FunGEn was created by a Computation Science graduation student, at the
Informatics Center (CIn) of the Federal University of Pernambuco (UFPE),
as part of a Scientific Iniciation (PIBIC/CNPq) research project (Creating
a Game Platform Using Haskell), oriented by lecturer Andre Santos (PhD,
1995, University of Glasgow). He was responsible for figuring out a lot of
FunGEn implementation details.

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

## To do

Andre's 2002 list:

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

## FAQ

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
