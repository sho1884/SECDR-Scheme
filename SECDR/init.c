/*
 * init.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/init.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    The author can be reached at s-haya@rst.fujixerox.co.jp or
    Shoichi Hayashi,
    SYSTEMS & COMMUNICATIONS LAB.  FUJI XEROX Co.,LTD.
    134 GODOCHO, HODOGAYA-KU, YOKOHAMA-SHI, KANAGAWA, 240, JAPAN
*/

#include "secdrscm.h"

#ifdef ASL
struct cell _NIL;
pointer NIL;               /* special cell representing empty cell */
struct cell _T;
pointer T ;                /* special cell representing #t */
struct cell _F;
pointer F;                 /* special cell representing #f */
struct cell _VOID;
pointer VOID;              /* special cell representing #v (void) */
struct cell _DUMMY;
pointer DUMMY;             /* dummy cell */
pointer TEMP;              /* temporary pointer marked by gc */
pointer * oblist;  /* symbol table */
pointer free_cell;         /* pointer to top of free cells */
long fcells;               /* # of free cells */
#else
struct cell _NIL;
pointer NIL = &_NIL;       /* special cell representing empty cell */
struct cell _T;
pointer T = &_T;           /* special cell representing #t */
struct cell _F;
pointer F = &_F;           /* special cell representing #f */
struct cell _VOID;
pointer VOID = &_VOID;     /* special cell representing #v (void) */
struct cell _DUMMY;
pointer DUMMY = &_DUMMY;   /* dummy cell */
pointer TEMP;              /* temporary pointer marked by gc */
pointer oblist[HASHTABLESIZE];  /* symbol table */
pointer free_cell = &_NIL; /* pointer to top of free cells */
long fcells = 0;           /* # of free cells */
#endif

/* global pointers to special symbols */
pointer COMPILE;     /* pointer to symbol -*-compile-*- */
pointer ERRORHOOK;   /* pointer to symbol -*-error-hook-*- */
pointer EVALHOOK;    /* pointer to symbol -*-eval-hook-*- */
pointer EXECHOOK;    /* pointer to symbol -*-exec-hook-*- */
pointer SYMBOLVALUE; /* pointer to symbol %symbol-value% */
pointer SYMBOLNAME;  /* pointer to symbol %symbol-name% */
pointer QUOTE;       /* pointer to symbol quote */
pointer QQUOTE;      /* pointer to symbol quasiquote */
pointer UNQUOTE;     /* pointer to symbol unquote */
pointer UNQUOTESP;   /* pointer to symbol unquote-splicing */

#ifdef ASL
struct cell _stdin_port;
pointer stdin_port;                  /* pointer to standart input port */
struct cell _stdout_port;
pointer stdout_port;                 /* pointer to standard output port */
struct cell _stderr_port;
pointer stderr_port;                 /* pointer to standard error port */
struct cell _eof_obj;
pointer eof_obj;                     /* pointer to end of file object */
pointer cur_inport;                  /* pointer to current input port */
pointer cur_outport;                 /* pointer to current output port */
#else
struct cell _stdin_port;
pointer stdin_port = &_stdin_port;   /* pointer to standart input port */
struct cell _stdout_port;
pointer stdout_port = &_stdout_port; /* pointer to standard output port */
struct cell _stderr_port;
pointer stderr_port = &_stderr_port;  /* pointer to standard error port */
struct cell _eof_obj;
pointer eof_obj = &_eof_obj;          /* pointer to end of file object */
pointer cur_inport = &_stdin_port;    /* pointer to current input port */
pointer cur_outport = &_stdout_port;  /* pointer to current output port */
#endif

#ifdef ASL
Char gc_verbose;        /* if gc_verbose is not zero, print gc status */
#else
Char gc_verbose = 0;    /* if gc_verbose is not zero, print gc status */
#endif

struct op_cell operator_table[OPMAX];

struct char_cell char_table[CHARMAX];

/* initialization of SECD-Scheme */
void init_scheme()
{
    int i;
    pointer x;
    struct char_cell *ch;
    struct op_cell *op;
    extern void init_primitives();

    /* initialize oblist table */
    for (i = 0 ; i < HASHTABLESIZE ; i++) oblist[i] = NIL;
    /* init NIL */
    type(NIL) = 0;
    flag(NIL) = (F_ATOM | F_MARK);
    car(NIL) = cdr(NIL) = NIL;
    /* init T */
    type(T) = 0;
    flag(T) = (F_ATOM | F_MARK);
    car(T) = cdr(T) = T;
    /* init F */
    type(F) = 0;
    flag(F) = (F_ATOM | F_MARK);
    car(F) = cdr(F) = F;
    /* init VOID */
    type(VOID) = 0;
    flag(VOID) = (F_ATOM | F_MARK);
    car(VOID) = cdr(VOID) = VOID;
    /* init DUMMY */
    type(DUMMY) = 0;
    flag(DUMMY) = (F_ATOM | F_MARK);
    car(DUMMY) = cdr(DUMMY) = DUMMY;

    /* initialize operator table */    
    for (i = 0 ; i < OPMAX ; i++) {
        op = &operator_table[i];
        type(op) = T_OPERATOR;
        flag(op) = (F_ATOM | F_MARK);
        opnum(op) = i;
    }

    /* initialize character table */
    for (i = 0 ; i < CHARMAX ; i++) {
        ch = &char_table[i];
        type(ch) = T_CHAR;
        flag(ch) = (F_ATOM | F_MARK);
        cvalue(ch) = (short)i;
    }

    /* allocate memory */
    if (alloc_cellseg(FIRST_CELLSEGS) != FIRST_CELLSEGS) 
        FatalError("Unable to allocate initial cell segments", "", "", "");

    /* intialization of global pointers to special symbols */
    COMPILE = mk_symbol("-*-compile-*-");
    ERRORHOOK = mk_symbol("-*-error-hook-*-");
    EVALHOOK = mk_symbol("-*-eval-hook-*-");
    EXECHOOK = mk_symbol("-*-exec-hook-*-");
    SYMBOLVALUE = mk_symbol("%symbol-value%");
    SYMBOLNAME = mk_symbol("%symbol-name%");
    QUOTE = mk_symbol("quote");
    QQUOTE = mk_symbol("quasiquote");
    UNQUOTE = mk_symbol("unquote");
    UNQUOTESP = mk_symbol("unquote-splicing");

    /* initialize standard port */
    type(stdin_port) = T_PORT;
    flag(stdin_port) = (F_ATOM | F_MARK);
    portfile(stdin_port) = stdin;
    portstatus(stdin_port) = (P_OPEN | P_INPUT);
    symvalue(mk_symbol("stdin-port")) = stdin_port;
    
    type(stdout_port) = T_PORT;
    flag(stdout_port) = (F_ATOM | F_MARK);
    portfile(stdout_port) = stdout;
    portstatus(stdout_port) = P_OPEN;
    symvalue(mk_symbol("stdout-port")) = stdout_port;
   
    type(stderr_port) = T_PORT;
    flag(stderr_port) = (F_ATOM | F_MARK);
    portfile(stderr_port) = stderr;
    portstatus(stderr_port) = P_OPEN;
    symvalue(mk_symbol("stderr-port")) = stderr_port;

    type(eof_obj) = 0;    
    flag(eof_obj) = (F_ATOM | F_MARK);
    car(eof_obj) = cdr(eof_obj) = eof_obj;

    /* initialize primitive-procedure */
    init_primitives();

}

