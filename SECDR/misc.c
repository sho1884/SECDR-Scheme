/*
 * misc.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/misc.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
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
    fflush(fp);
}

void quit(n)
int n;
{
    message(stdout, GoodBye, "", "", "");
    exit(n);
}

