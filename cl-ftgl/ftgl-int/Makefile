# =============================================================================
# MAKEFILE for libFTGLinterface
# =============================================================================

# $Id: Makefile,v 1.3 2006/08/26 16:08:27 fgoenninger Exp $

# by Frank Goenninger, Bempflingen, Germany
# August 2006

# =============================================================================
# MAKEFILE for libFTGLinterface
# =============================================================================

SRCS = FTGLFromC.cpp
OBJS = FTGLFromC.o

INCLUDEDIRS = -I/usr/X11R6/include/freetype2 -I/usr/X11R6/include/freetype2/freetype -I/opt/ftgl/src/FTGL/include

TARGET = libFTGLint.dylib
DEPLIBS = -framework Carbon -framework OpenGL /usr/X11R6/lib/libfreetype.a /opt/ftgl/src/FTGL/mac/build/Development/libftgl.a

# DEPLIBS = -framework Carbon -framework OpenGL -framework GLUT /usr/X11R6/lib/libfreetype.a /opt/ftgl/src/FTGL/mac/build/Development/libftgl.a


CC = gcc -O
LD = g++ -O

CCFLAGS = -dynamic
# LDFLAGS = -dynamiclib -flat_namespace
LDFLAGS = -dynamiclib

FTGLFromC.o:  $(SRCS)
	$(CC) $(CCFLAGS) $(INCLUDEDIRS) -o FTGLFromC.o -c FTGLFromC.cpp

all:	$(OBJS)
	$(LD) $(LDFLAGS) -o $(TARGET) $(DEPLIBS) FTGLFromC.o

clean:
	rm -f $(OBJS) $(TARGET)
