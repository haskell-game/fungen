HC      = ghc
TOP     = ../..
HC_OPT  = -i$(TOP)/lib -I$(TOP)/lib  -L$(TOP)/lib -fglasgow-exts -recomp -Wall -O -package lang -package win32
HC_LIBS = -lHOpenGL -lglut32 -lglu32 -lopengl32

AR0 = game

AR1 = UserInput
AR2 = Fun_Types
AR3 = Fun_Aux
AR4 = Fun_Loader
AR5 = Fun_Text
AR6 = Fun_Objects
AR7 = Fun_Map
AR8 = Fun_Game
AR9 = Fun_Display
AR10 = Fun_Input
AR11 = Fun_Timer
AR12 = Fun_Init
AR13 = FunGEn

ONE = game.hs

OBJS = $(AR1).o $(AR2).o $(AR3).o $(AR4).o $(AR5).o $(AR6).o $(AR7).o $(AR8).o $(AR9).o $(AR10).o $(AR11).o $(AR12).o $(AR13).o

all:
	make depend
	make link

compile:
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" ${OBJS:.o=.hs}

depend:
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR1).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR2).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR3).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR4).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR5).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR6).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR7).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR8).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR9).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR10).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR11).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR12).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR13).hs
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR0).hs

link:
	$(HC) -o $(AR0) $(HC_OPT)  "-#include <GL/glut.h>" $(AR0).o $(OBJS) $(HC_LIBS)

single:
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(ONE)
	make link	

clean:
	rm -f $(OBJS)
	rm -f ${OBJS:.o=.hi}
	rm -f ${OBJS:.o=.BAK}
	rm -f ${OBJS:.o=.bak}
	rm -f $(AR0).o $(AR0).hi $(AR0).bak $(AR0).BAK