/*
 * error.c 
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/error.c,v 0.6 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/
 
#include "mulasame.h"
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
#ifdef THINK_C
#ifdef Mulasame
	myBackgroundProcess = NULL;
#endif
#ifdef BSDALRM
    signal(SIGALRM, SIG_IGN);
#endif
#endif
    para_fflush(stdin);
#ifdef THINK_C
#ifdef Mulasame
    fflush(substdin);
#endif
#endif
    fflush(stdout);
#ifdef THINK_C
#ifdef Mulasame
    fflush(substdout);
#endif
#endif

#ifdef Mulasame
    if (Current->cpu_id > 1) {
      if(PreviousCell != NULL)
	cdr(PreviousCell) = cdr(CurrentCell);
      else
	symvalue(PROCQUEUE) = cdr(CurrentCell);
      cdr(CurrentCell) = symvalue(ERRORCPU);
      symvalue(ERRORCPU) = CurrentCell;
      cpustatus(car(CurrentCell)) = ERRORSUSPEND ;
    }
#endif

#ifndef ASL
    signal(SIGINT,breakHandler);
#endif
#ifdef THINK_C
#ifdef Mulasame
	cur_inport = stdin_port;
	cur_outport = stdout_port;
#endif
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
