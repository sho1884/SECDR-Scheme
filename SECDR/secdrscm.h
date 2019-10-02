/*
 * =====================================================================
 *
 *                  ----- SECDR-Scheme Version 1.0 ----- 
 *
 *     --------------------------------------------------------------
 *     Version 1.0 modified and coded by Shoichi Hayashi (6/7/1993)
 *
 *            E-MAIL :  s-haya@rst.fujixerox.co.jp
 *            MIX    :  s.hayasi
 *     --------------------------------------------------------------
 *     SECDR-Scheme Version 0.65 coded by Atsushi Moriwaki (3/6/1990)
 *
 *            E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 *            MIX    :  riemann
 *            NIFTY  :  PBB01074
 *
 */
#ifndef THINK_C
static char *rcshid =
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/secdrscm.h,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
#endif
/* =====================================================================
 *
 * secdrscm.h
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

/* Here is System declaration */
/* #define MPW     MPW C version 3.2 for Macintosh */
/* #define THINK_C THINK C version 5.0(4.0) for Macintosh */
/* #define ASL     H.Y ASLEdit+ version (due to Syoichi Hayashi) */
/* #define BSD     4.x BSD */
/* #define SYSV    SYSV */
/* #define MSC     Microsoft C Compiler v.4.00 */
/* #define TURBO   Turbo C compiler v.2.0 */

/* default boot path */
#ifndef DefaultPath
#define DefaultPath ""
#endif

#define BootFile "boot.bin"
#define InitFile "Init.scm"

#define Banner "SECDR-Scheme version 1.0,\n\
Copyright (C)  1990, 1991, 1992, 1993  Atsushi Moriwaki, Shoichi Hayashi.\n\
SECDR-Scheme comes with ABSOLUTELY NO WARRANTY; for details type `(terms)'.\n\
This is free software, and you are welcome to redistribute it\n\
under certain conditions; type `(terms)' for details.\n"

#define GoodBye ";EXIT\n"

#define CELL_SEGSIZE    5000  /* # of cells in one segment */
#define CELL_NSEGMENT   300   /* # of segments for cells */

#define USE_SYSTEM
#define USE_BRACKET
#define RUNTIME
/* #define DEBUG_MODE */

#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <math.h>

/* System dependency */
#ifdef MPW
#include <string.h>
#include <stdlib.h>
#define prompt "> input next line\n"
#define Suffix ':'
#define SOFTTYPE "macos"
#define FIRST_CELLSEGS 8 
#define R_BINMODE "rb"
#define W_BINMODE "wb"
#endif

#ifdef THINK_C
#include <string.h>
#include <stdlib.h>
#define malloc NewPtr
#define free DisposPtr
#define prompt "> "
#define Suffix ':'
#define SOFTTYPE "macos"
#define FIRST_CELLSEGS 8 
#define R_BINMODE "rb"
#define W_BINMODE "wb"
#define Macintosh
#endif

#ifdef BSD
#include <string.h>
#include <stdlib.h>
#define prompt "> "
#define Suffix '/'
#define SOFTTYPE "unix"
#define FIRST_CELLSEGS 15 
#define R_BINMODE "r"
#define W_BINMODE "w"
#endif

#ifdef SYSV
#include <string.h>
#define prompt "> "
#define Suffix '/'
#define SOFTTYPE "unix"
#define FIRST_CELLSEGS 15 
#define R_BINMODE "r"
#define W_BINMODE "w"
#endif

#ifdef MSC
#include <string.h>
#include <stdlib.h>
#include <malloc.h>
#define prompt "> "
#define Suffix '\\'
#define SOFTTYPE "msdos"
#define FIRST_CELLSEGS 5
#define R_BINMODE "rb"
#define W_BINMODE "wb"
#endif

#ifdef TURBO
#include <string.h>
#include <stdlib.h>
#define prompt "> "
#define Suffix '\\'
#define SOFTTYPE "msdos"
#define FIRST_CELLSEGS 5
#define R_BINMODE "rb"
#define W_BINMODE "wb"
#endif

typedef unsigned char Char;

/* cell structure */
struct cell {
    unsigned char _type;
    unsigned char _flag;
    union {
        long _ivalue;
        double _rvalue;
        struct { long _num; long _den; } _qvalue;
        struct { long _len; Char *_svalue; } _string;
        struct { struct cell *_car; struct cell *_cdr; } _cons;
        struct { long _len; struct cell **_vec; } _vector;
        struct { FILE *_file; unsigned char _status; } _port;
    } _object;
};

/* chracter cell structure */
struct char_cell {
    unsigned char _type;
    unsigned char _flag;
    short _cvalue;
};

/* primitive procedure cell structure */
struct prim_cell {
    unsigned char _type;
    unsigned char _flag;
    struct cell *(*_fun)();
    Char *_name;
};

/* operator cell structure */
struct op_cell {
    unsigned char _type;
    unsigned char _flag;
    int _opnum;
};

typedef struct cell *pointer;

/* CPU structure */
struct machine {
	/* We use 5 registers. */
    pointer S_Reg; /* stack register */
    pointer E_Reg; /* environment register */
    pointer C_Reg; /* control list register */
    pointer D_Reg; /* dump register */
    pointer R_Reg; /* return value register */
};

typedef struct machine CPU;

#define T_STRING          1
#define T_INTEGER         2
#define T_RATIONAL        3
#define T_REAL            4
#define T_CHAR            5
#define T_SYMBOL          6
#define T_PRIMITIVE       7
#define T_PORT            8
#define T_PAIR            9
#define T_CLOSURE        10
#define T_CONTINUATION   11
#define T_VECTOR         12
#define T_OPERATOR       13
#define T_BOX            14

#define F_ATOM            1  /* 00000001 */
#define CLRATOM         254  /* 11111110 */
#define F_MARK            2  /* 00000010 */
#define UNMARK          253  /* 11111101 */
#define F_PROMISE         4  /* 00000100 */

/* macros for cell operations */
#define type(p)         ((p)->_type)
#define flag(p)         ((p)->_flag)

#define isoperator(p)   (type(p) == T_OPERATOR)
#define opnum(p)        (((struct op_cell *)(p))->_opnum)

#define isprimitive(p)  (type(p) == T_PRIMITIVE)
#define primfun(p)      (((struct prim_cell *)(p))->_fun)
#define primname(p)     (((struct prim_cell *)(p))->_name)

#define isstring(p)     (type(p) == T_STRING)
#define stringlen(p)    ((p)->_object._string._len)
#define stringvalue(p)  ((p)->_object._string._svalue)

#define ischar(p)       (type(p) == T_CHAR)
#define cvalue(p)       (((struct char_cell *)(p))->_cvalue)

#define isinteger(p)    (type(p) == T_INTEGER)
#define ivalue(p)       ((p)->_object._ivalue)
#define isrational(p)   (type(p) == T_RATIONAL)
#define qnvalue(p)      ((p)->_object._qvalue._num)
#define qdvalue(p)      ((p)->_object._qvalue._den)
#define isreal(p)       (type(p) == T_REAL)
#define rvalue(p)       ((p)->_object._rvalue)
#define isnumber(p)     (isinteger(p)||isrational(p)||isreal(p))

#define isport(p)       (type(p) == T_PORT)
#define portfile(p)     ((p)->_object._port._file)
#define portstatus(p)   ((p)->_object._port._status)

#define ispair(p)       (type(p) == T_PAIR)
#define car(p)          ((p)->_object._cons._car)
#define cdr(p)          ((p)->_object._cons._cdr)

#define isvector(p)     (type(p) == T_VECTOR)
#define vectorlen(p)    ((p)->_object._vector._len)
#define vector(p)       ((p)->_object._vector._vec)

#define issymbol(p)     (type(p) == T_SYMBOL)
#define symvalue(p)     car(p)
#define symstring(p)    car(cdr(p))
#define hasprop(p)      (type(p) == T_SYMBOL)
#define symprop(p)      cdr(cdr(p))

#define isclosure(p)    (type(p) == T_CLOSURE)
#define closure_code(p) car(p)
#define closure_env(p)  cdr(p)

#define iscontinuation(p) (type(p) == T_CONTINUATION)
#define cont_dump(p)    cdr(p)

#define isbox(p)        (type(p) == T_BOX)
#define boxcontext(p)   car(p)

#define caar(p)         car(car(p))
#define cadr(p)         car(cdr(p))
#define cdar(p)         cdr(car(p))
#define cddr(p)         cdr(cdr(p))
#define caaar(p)        car(car(car(p)))
#define cadar(p)        car(cdr(car(p)))
#define cdaar(p)        cdr(car(car(p)))
#define cddar(p)        cdr(cdr(car(p)))
#define caadr(p)        car(car(cdr(p)))
#define cdadr(p)        cdr(car(cdr(p)))
#define caddr(p)        car(cdr(cdr(p)))
#define cdddr(p)        cdr(cdr(cdr(p)))
#define caaaar(p)       car(car(car(car(p))))
#define caadar(p)       car(car(cdr(car(p))))
#define cadaar(p)       car(cdr(car(car(p))))
#define caddar(p)       car(cdr(cdr(car(p))))
#define cdaaar(p)       cdr(car(car(car(p))))
#define cdadar(p)       cdr(car(cdr(car(p))))
#define cddaar(p)       cdr(cdr(car(car(p))))
#define cdddar(p)       cdr(cdr(cdr(car(p))))
#define cadddr(p)       car(cdr(cdr(cdr(p))))
#define cddddr(p)       cdr(cdr(cdr(cdr(p))))
#define caddddr(p)      car(cdr(cdr(cdr(cdr(p)))))
#define cdddddr(p)      cdr(cdr(cdr(cdr(cdr(p)))))

/* true or false value macro */
/* #define istrue(p)       ((p) != NIL && (p) != F) */
#define istrue(p)       ((p) != F)
#define isfalse(p)      ((p) == NIL || (p) == F)

/* We use SECDR-machines. */
extern CPU Master;
extern CPU Slave;
extern CPU* Current;

extern pointer NIL;         /* special cell representing empty cell */
extern pointer T;           /* special cell representing #t */
extern pointer F;           /* special cell representing #f */
extern pointer VOID;        /* special cell representing #v (void) */
extern pointer DUMMY;       /* dummy cell */
extern pointer Args;        /* pointer to argument of primitive procedure */
extern pointer TEMP;        /* temporary pointer marked by gc */
#define HASHTABLESIZE 256

#ifdef ASL
extern pointer * oblist;  /* symbol table */
#else
extern pointer oblist[HASHTABLESIZE];  /* symbol table */
#endif

extern jmp_buf error_jmp;
extern int repflg;

/* global pointers to special symbols */
extern pointer COMPILE;     /* pointer to symbol -*-compile-*- */
extern pointer ERRORHOOK;   /* pointer to symbol -*-error-hook-*- */
extern pointer EVALHOOK;    /* pointer to symbol -*-eval-hook-*- */
extern pointer EXECHOOK;    /* pointer to symbol -*-exec-hook-*- */
extern pointer SYMBOLVALUE; /* pointer to symbol %symbol-value% */
extern pointer SYMBOLNAME;  /* pointer to symbol %symbol-name% */
extern pointer QUOTE;       /* pointer to symbol quote */
extern pointer QQUOTE;      /* pointer to symbol quasiquote */
extern pointer UNQUOTE;     /* pointer to symbol unquote */
extern pointer UNQUOTESP;   /* pointer to symbol unquote-splicing */

extern pointer stdin_port;  /* pointer to standart input port */
extern pointer stdout_port; /* pointer to standard output port */
extern pointer stderr_port; /* pointer to standard error port */
extern pointer eof_obj;     /* pointer to end of file object */
extern pointer cur_inport;  /* pointer to current input port */
extern pointer cur_outport; /* pointer to current output port */

#define P_OPEN      1 /* 00000001 */
#define P_INPUT     2 /* 00000010 */
#define CLROPEN   254 /* 11111110 */
#define isopenport(p)  (portstatus(p)&P_OPEN)
#define isinputport(p) (portstatus(p)&P_INPUT)

#define OPMAX 19  /* check # of OP_XXXXX (see operator.h). */
extern struct op_cell operator_table[OPMAX];
#define mk_operator(a) ((pointer)&operator_table[(a)])

/* global variable for character table */
#define CHARMAX  256
#define CHARMASK 255
#define OCTALFORMAT "#\\%03o"
extern struct char_cell char_table[CHARMAX];

#define STRBUFFSIZE 1024
extern Char strbuff[STRBUFFSIZE];

extern Char gc_verbose;  /* if gc_verbose is not zero, print gc status */

/***  prototype  ***/
extern int cmpstr();
extern void flushinput();
extern void flushoutput();
extern void message();
extern void quit();

extern void init_primitives();
extern void FatalError();
extern void Error();
extern void breakHandler();

extern void init_scheme();

extern int alloc_cellseg();
extern pointer get_cell();
extern void *gc_alloc();
extern void gc();

extern pointer cons();
extern pointer mk_integer();
extern pointer mk_rational();
extern pointer mk_real();
extern pointer alloc_string();
extern pointer mk_substring();
extern pointer mk_string();
extern int hash();
extern pointer _mk_symbol();
extern pointer mk_symbol();
extern pointer mk_atom();
extern pointer trans_base();
extern int numtype();
extern pointer mk_const();
extern pointer mk_vector();
extern pointer mk_inputport();
extern pointer mk_outputport();
extern pointer closeport();
extern pointer mk_closure();
extern pointer mk_continuation();
extern pointer mk_box();

extern pointer numerical_operator();
extern double round();
extern int Is_Digit();
extern int string_eqv();
extern int ToUpper();
extern int ToLower();
extern int string_compare();
extern int string_compare_ci();

extern void printlist();
extern void format_print();
extern long list_width();

extern pointer readsexpr();

extern int SECDR_Cycle();

extern pointer gensym();
extern pointer list2vec();
extern pointer vec2list();
extern int eqv();
extern pointer string_append();
extern pointer run();
extern pointer compile_run();
extern pointer bin_load();
extern pointer load();
extern pointer ex_load();

#ifdef RUNTIME
extern unsigned long internal_runtime();
extern double myruntime();
#endif

#define NO_NUM_TYPE    0
#define NUM_TYPE_INT   1
#define NUM_TYPE_REAL  2
#define NUM_TYPE_RAT   3

#define EXACT 1
#define INEXACT 2

#define INTERRUPT 1
#define ERRORCOD 2

extern int maxint, maxintstrlen;
extern Char maxintstr[30];
extern double gc_time;

#ifdef ASL
#define MAXNAMETABLE 200
#endif
