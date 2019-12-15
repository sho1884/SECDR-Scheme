/*
 * init.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/init.c,v 0.7 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"

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
#ifdef Mulasame
pointer PROCQUEUE;   /* pointer to symbol -*-process-*- */
pointer TERMCPU;     /* pointer to symbol -*-terminate-*- */
pointer WAITQUEUE;   /* pointer to symbol -*-wait-process-*- */
pointer SEMWAITQUEUE;/* pointer to symbol -*-sem-wait-process-*- */
pointer RESWAITQUEUE;/* pointer to symbol -*-res-wait-process-*- */
pointer ERRORCPU;    /* pointer to symbol -*-error-cpu-*- */
pointer BACKUPCPU;   /* pointer to symbol -*-backup-cpu-*- */
#endif
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
#ifdef THINK_C
#ifdef Mulasame
struct cell _substdin_port;
pointer substdin_port = &_substdin_port;   /* pointer to substandart input port */
struct cell _substdout_port;
pointer substdout_port = &_substdout_port; /* pointer to substandard output port */
#endif
#endif
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
    scmtype(NIL) = 0;
    flag(NIL) = (F_ATOM | F_MARK);
    car(NIL) = cdr(NIL) = NIL;
    /* init T */
    scmtype(T) = 0;
    flag(T) = (F_ATOM | F_MARK);
    car(T) = cdr(T) = T;
    /* init F */
    scmtype(F) = 0;
    flag(F) = (F_ATOM | F_MARK);
    car(F) = cdr(F) = F;
    /* init VOID */
    scmtype(VOID) = 0;
    flag(VOID) = (F_ATOM | F_MARK);
    car(VOID) = cdr(VOID) = VOID;
    /* init DUMMY */
    scmtype(DUMMY) = 0;
    flag(DUMMY) = (F_ATOM | F_MARK);
    car(DUMMY) = cdr(DUMMY) = DUMMY;

    /* initialize operator table */    
    for (i = 0 ; i < OPMAX ; i++) {
        op = &operator_table[i];
        scmtype(op) = T_OPERATOR;
        flag(op) = (F_ATOM | F_MARK);
        opnum(op) = i;
    }

    /* initialize character table */
    for (i = 0 ; i < CHARMAX ; i++) {
        ch = &char_table[i];
        scmtype(ch) = T_CHAR;
        flag(ch) = (F_ATOM | F_MARK);
        cvalue(ch) = (short)i;
    }

    /* allocate memory */
    if (alloc_cellseg(FIRST_CELLSEGS) != FIRST_CELLSEGS) 
        FatalError("Unable to allocate initial cell segments", "", "", "");

    /* intialization of global pointers to special symbols */
#ifdef Mulasame
    PROCQUEUE = mk_symbol("-*-process-*-");
    symvalue(PROCQUEUE) = NIL;
    TERMCPU = mk_symbol("-*-terminate-*-");
    symvalue(TERMCPU) = NIL;
    WAITQUEUE = mk_symbol("-*-wait-process-*-");
    symvalue(WAITQUEUE) = NIL;
    SEMWAITQUEUE = mk_symbol("-*-sem-wait-process-*-");
    symvalue(SEMWAITQUEUE) = NIL;
    RESWAITQUEUE = mk_symbol("-*-res-wait-process-*-");
    symvalue(RESWAITQUEUE) = NIL;
    ERRORCPU = mk_symbol("-*-error-cpu-*-");
    symvalue(ERRORCPU) = NIL;
    BACKUPCPU = mk_symbol("-*-backup-cpu-*-");
    symvalue(BACKUPCPU) = NIL;
#endif
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
    scmtype(stdin_port) = T_PORT;
    flag(stdin_port) = (F_ATOM | F_MARK);
    portfile(stdin_port) = stdin;
    portstatus(stdin_port) = (P_OPEN | P_INPUT);
    symvalue(mk_symbol("stdin-port")) = stdin_port;
    
    scmtype(stdout_port) = T_PORT;
    flag(stdout_port) = (F_ATOM | F_MARK);
    portfile(stdout_port) = stdout;
    portstatus(stdout_port) = P_OPEN;
    symvalue(mk_symbol("stdout-port")) = stdout_port;

#ifdef THINK_C
#ifdef Mulasame   
    scmtype(substdin_port) = T_PORT;
    flag(substdin_port) = (F_ATOM | F_MARK);
    portfile(substdin_port) = substdin;
    portstatus(substdin_port) = (P_OPEN | P_INPUT);
    symvalue(mk_symbol("substdin-port")) = substdin_port;
    
    scmtype(substdout_port) = T_PORT;
    flag(substdout_port) = (F_ATOM | F_MARK);
    portfile(substdout_port) = substdout;
    portstatus(substdout_port) = P_OPEN;
    symvalue(mk_symbol("substdout-port")) = substdout_port;
#endif
#endif
   
    scmtype(stderr_port) = T_PORT;
    flag(stderr_port) = (F_ATOM | F_MARK);
    portfile(stderr_port) = stderr;
    portstatus(stderr_port) = P_OPEN;
    symvalue(mk_symbol("stderr-port")) = stderr_port;

    scmtype(eof_obj) = 0;    
    flag(eof_obj) = (F_ATOM | F_MARK);
    car(eof_obj) = cdr(eof_obj) = eof_obj;

    /* initialize primitive-procedure */
    init_primitives();

}

