#
# Makefile for secdrscm
#
# Coded by Atsushi Moriwaki.
# Revised by Shoichi Hayashi.
#
# RCS info: $Header: /home/argama/s-haya/Scheme/SECDR/RCS/makefile,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
# =====================================================================

#IMPLPATH = A:\\\LANG\\\SCHEME\\\SRC
IMPLPATH = `pwd`

# BSD 4.x
##CC = cc -O -DBSD
#CC = gcc -O -DBSD
#CFLAGS = -DDefaultPath=\"$(IMPLPATH)\"
#LDFLAGS =
#OBJS = error.o init.o  memory.o misc.o mk.o prims.o print.o \
#       read.o secdrcyc.o secdrscm.o utils.o

# SYSV 
CC = /usr/5bin/cc -O -DSYSV
CFLAGS = -DDefaultPath=\"$(IMPLPATH)\"
LDFLAGS =
OBJS = error.o init.o  memory.o misc.o mk.o prims.o print.o \
       read.o secdrcyc.o secdrscm.o utils.o

# TURBOC version 2.0
#CC = tcc
#CFLAGS = -ml -G -DTURBO2 -DDefaultPath=\"$(IMPLPATH)\" -c 
#LDFLAGS = -ml -esecdrscm
#OBJS = error.obj init.obj  memory.obj misc.obj mk.obj prims.obj print.obj \
#       read.obj secdrcyc.obj secdrscm.obj utils.obj

# MSC version 4.00
#CC = cl
#CFLAGS = /AL /Ot /DMSC4 /DDefaultPath=\"$(IMPLPATH)\" /c 
#LDFLAGS = /AL /Fesecdrscm
#OBJS = error.obj init.obj  memory.obj misc.obj mk.obj prims.obj print.obj \
#       read.obj secdrcyc.obj secdrscm.obj utils.obj

# UNIX (BSD 4.x or SYSV)
secdrscm : $(OBJS) secdrscm.h operator.h
	$(CC) -o secdrscm $(OBJS) -lm

# MSDOS (TURBOC or MSC)
#.c.obj :
#	$(CC) $(CFLAGS) $*.c
#secdrscm.exe : $(OBJS) secdrscm.h operator.h
#	$(CC) $(LDFLAGS) *.obj

$(OBJS) : secdrscm.h
error.o secdrcyc.o utils.o : operator.h

clean :
	rm -f *.o secdrscm core

