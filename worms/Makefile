HC      = ghc
TOP     = ../../..
FG      = ..
HC_OPT  = -i$(TOP)/lib -I$(TOP)/lib  -L$(TOP)/lib -i$(FG)/ -fglasgow-exts -recomp -Wall -O -package lang -package win32
HC_LIBS = -lHOpenGL -lglut32 -lglu32 -lopengl32

AR0 = worms

AR1 = Fun_Sound
AR2 = UserInput
AR3 = Fun_Types
AR4 = Fun_Aux
AR5 = Fun_Loader
AR6 = Fun_Text
AR7 = Fun_Objects
AR8 = Fun_Map
AR9 = Fun_Game
AR10 = Fun_Display
AR11 = Fun_Input
AR12 = Fun_Timer
AR13 = Fun_Init
AR14 = FunGEn

ONE = worms.hs

OBJS = $(FG)/$(AR1).o $(FG)/$(AR2).o $(FG)/$(AR3).o $(FG)/$(AR4).o $(FG)/$(AR5).o $(FG)/$(AR6).o $(FG)/$(AR7).o $(FG)/$(AR8).o $(FG)/$(AR9).o $(FG)/$(AR10).o $(FG)/$(AR11).o $(FG)/$(AR12).o $(FG)/$(AR13).o $(FG)/$(AR14).o

all:
	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(ONE)
	make link	

depend:

	$(HC) -c $(HC_OPT) $(HC_LIBS) "-#include <GL/glut.h>" $(AR0).hs

link:
	$(HC) -o $(AR0) $(HC_OPT)  "-#include <GL/glut.h>" $(AR0).o $(OBJS) $(HC_LIBS)

clean:
	rm -f $(AR0).o $(AR0).hi $(AR0).bak $(AR0).BAK