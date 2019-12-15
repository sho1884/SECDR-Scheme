/*
 * misc.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/misc.c,v 0.2 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"

/* compare string */
int cmpstr(s1,l1,s2,l2)
register Char *s1;
register long l1;
register Char *s2;
register long l2;
{
    if (l1 != l2) return (0);
    while (l1-- > 0) if (*s1++ != *s2++) return (0);
    return (1);
}

/* back to standard input */
void flushinput()
{
    if (cur_inport != stdin_port) {
        closeport(cur_inport); cur_inport = stdin_port;
    }
}

/* back to standard output */
void flushoutput()
{
    if (cur_outport != stdout_port) {
        closeport(cur_outport); cur_outport = stdout_port;
    }
}

void message(fp,fmt,a,b,c)
FILE *fp;
Char *fmt;
Char *a;
Char *b;
Char *c;
{
    fprintf(fp, fmt, a, b, c);
    para_fflush(fp);
}

void quit(n)
int n;
{
    message(stdout, GoodBye, "", "", "");
    exit(n);
}

