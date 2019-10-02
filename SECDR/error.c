/*
 * error.c 
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/error.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
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
#include "operator.h"
#ifndef ASL
#include <signal.h>
#endif

void FatalError(fmt,a,b,c)
Char *fmt;
Char *a;
Char *b;
Char *c;
{
    fprintf(stderr, "Fatal Error: ");
    fprintf(stderr, fmt, a, b, c);
    fprintf(stderr, "\n");
    exit(1);
}

void Error(fmt,args)
Char *fmt;
pointer args;
{
#ifdef BSD
    stdin->_cnt = 0;
    stdin->_ptr = stdin->_base;
#else
    fflush(stdin);
#endif
    fflush(stdout);
#ifndef ASL
    signal(SIGINT,breakHandler);
#endif
    fprintf(stderr, "Error: ");
    format_print(stderr, fmt, args);
    fprintf(stderr, "\n");
    if(repflg) {
        /* Error Hook */
        Slave.S_Reg = cons(NIL, NIL);
        Slave.E_Reg = NIL;
        Slave.C_Reg = cons(mk_operator(OP_AP), cons(mk_operator(OP_STOP), NIL));
        Slave.D_Reg = NIL;
        Slave.R_Reg = symvalue(ERRORHOOK);
        SECDR_Cycle(&Slave);
    }
    flushinput();
    flushoutput();
    longjmp(error_jmp, ERRORCOD);
}
