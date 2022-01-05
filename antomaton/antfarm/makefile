
# ANTFARM is based on the escape source code.
# I have preserved only the LINUX and WIN32 
# sections of this makefile, but I also know
# how to build for OS X.

ifndef LINUX
# makefile for windows

default : antfarm.exe

ZLIBDIR = ..\..\..\..\SDL_image_1.2.4\lib
ZINCDIR = ..\..\..\..\SDL_image_1.2.4\include
PLIBDIR = ZLIBDIR
PINCDIR = ZINCDIR

# not sure what fd does
DEBUGFLAGS = /Od /GZ /MDd /FD 
FASTFLAGS = /MD /Gf /G6

COMPILE=cl /nologo /GX ${FASTFLAGS} /W3 /I "..\..\..\..\SDL-1.2.8\include" /I "..\..\..\..\SDL_image-1.2.4\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /c /I ${ZINCDIR} /I ${PINCDIR}

LINK=link oldnames.lib "..\..\..\..\SDL-1.2.8\lib\SDL.lib" "..\..\..\..\SDL_image-1.2.4\lib\SDL_image.lib" libcpmt.lib msvcrt.lib msvcprt.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib uuid.lib /nologo /subsystem:windows /pdb:none /debug /machine:I386 /nodefaultlib /libpath:"..\..\..\..\SDL-1.2.8\lib" /libpath:"..\..\..\..\SDL_image-1.2.4\lib" /libpath:${ZLIBDIR} /libpath:${PLIBDIR}

%.obj : %.cpp extent.h
	${COMPILE} $<

antfarm.res : antfarm.rc antfarm.ico
	rc /r /fo antfarm.res antfarm.rc

antfarm.exe : main.obj sdlutil.obj font.obj util.obj md5.obj prompt.obj draw.obj message.obj menu.obj antfarm.obj winmain.obj
	${LINK} /out:"antfarm.exe" $^

clean :
	rm -f antfarm.exe *.obj antfarm.res

endif

ifdef LINUX
# makefile for linux

OFILES = main.o sdlutil.o font.o util.o md5.o prompt.o draw.o message.o menu.o antfarm.o

default : antfarm.exe

PROF =
# PROF = -pg

LINKFLAGS =
#  LINKFLAGS = -lefence

CPPFLAGS = `sdl-config --cflags` -g -Wall -O2 -fomit-frame-pointer ${PROF} -Wno-non-virtual-dtor -DNOSOUND

antfarm.exe : ${OFILES}
	g++ ${PROF} -o $@ $^ -lSDL_image `sdl-config --libs` -ltiff -lpng -ljpeg -lz ${LINKFLAGS}


wc :
	wc -l *.cpp *.h

clean :
	rm -f *.o *.exe core core.[0-9]* gmon.out vgcore.*

endif
