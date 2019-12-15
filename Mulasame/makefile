#
# Makefile for secdrscm & mulasame
#
# Coded by Shoichi Hayashi.
#
# RCS info: $Header: /usr/PDS/lang/Scheme/Mulasame/RCS/makefile,v 1.4 1994/09/14 02:55:46 s-haya Exp s-haya $
# =====================================================================

#IMPLPATH = A:\\\LANG\\\SCHEME\\\SRC
IMPLPATH = `pwd`

# BSD 4.x
CC = gcc -O2 -DBSD -DMulasame -DTurtle	# mulasame
#CC = gcc -O2 -DBSD -DTurtle		# secdrscm
# Macintosh A/UX 3.x
#CC = gcc -O2 -ZB -DBSD -DMulasame -DTurtle -DAUX	# mulasame
#CC = gcc -O2 -ZB -DBSD -DTurtle			# secdrscm
# MacMiNT
#CC = gcc -O2 -DBSD -DMulasame 		# mulasame
#CC = gcc -O2 -DBSD			# secdrscm
CFLAGS = -DDefaultPath=\"$(IMPLPATH)\"
#LDFLAGS = -lm
LDFLAGS = -lm -lX11
#LDFLAGS = -lpml		# for MacMiNT
OBJS = error.o init.o  memory.o misc.o mk.o prims.o print.o \
       read.o secdrcyc.o mulasame.o utils.o
MULASAMEOBJS = error.o init.o  memory.o misc.o mk.o prims.o print.o \
       read.o secdrcyc.o schedule.o mulasame.o utils.o

# SYSV 
#CC = /usr/5bin/cc -O2 -DSYSV -DMulasame -DTurtle	# mulasame
#CC = /usr/5bin/cc -O2 -DSYSV -DTurtle			# secdrscm
#CFLAGS = -DDefaultPath=\"$(IMPLPATH)\"
#LDFLAGS =
#OBJS = error.o init.o  memory.o misc.o mk.o prims.o print.o \
#       read.o secdrcyc.o mulasame.o utils.o
#MULASAMEOBJS = error.o init.o  memory.o misc.o mk.o prims.o print.o \
#       read.o secdrcyc.o schedule.o mulasame.o utils.o

# TURBOC version 2.0
#CC = tcc
#CFLAGS = -ml -G -DTURBO2 -DDefaultPath=\"$(IMPLPATH)\" -c 
#LDFLAGS = -ml -esecdrscm
#OBJS = error.obj init.obj  memory.obj misc.obj mk.obj prims.obj print.obj \
#       read.obj secdrcyc.obj mulasame.obj utils.obj

# MSC version 4.00
#CC = cl
#CFLAGS = /AL /Ot /DMSC4 /DDefaultPath=\"$(IMPLPATH)\" /c 
#LDFLAGS = /AL /Fesecdrscm
#OBJS = error.obj init.obj  memory.obj misc.obj mk.obj prims.obj print.obj \
#       read.obj secdrcyc.obj mulasame.obj utils.obj

# UNIX (BSD 4.x or SYSV) for mulasame
mulasame : $(MULASAMEOBJS) mulasame.h operator.h
	$(CC) -o mulasame $(MULASAMEOBJS) $(LDFLAGS)

# UNIX (BSD 4.x or SYSV) for secdrscm
secdrscm : $(OBJS) mulasame.h operator.h
	$(CC) -o secdrscm $(OBJS) $(LDFLAGS)

# MSDOS (TURBOC or MSC)
#.c.obj :
#	$(CC) $(CFLAGS) $*.c
#secdrscm.exe : $(OBJS) secdrscm.h operator.h
#	$(CC) $(LDFLAGS) *.obj

$(OBJS) : mulasame.h
$(MULASAMEOBJS) : mulasame.h
error.o secdrcyc.o utils.o : operator.h

prims.o : turtlegr.c
clean :
	rm -f *.o secdrscm mulasame core
