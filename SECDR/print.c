/*
 * print.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/print.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
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

#define isprquote(name, x) \
    (car((x)) == (name) && ispair(cdr(x)) && cddr((x)) == NIL)

/* print string */
static void printstring(fp,s,f1,f2)
FILE *fp;
pointer s;
Char f1;
Char f2;
{
    register long len = stringlen(s);
    register Char c, *p = stringvalue(s);

    if (f1) {
        if (f2) fputc('"', fp);
        while (len-- > 0) {
          if((*p == '\\' || *p == '"') && f2) fputc('\\', fp);
          fputc(*p++, fp);
        }
        if (f2) fputc('"', fp);
    } else {
        while (len-- > 0) {
            fputc(*p++, fp);
        }
    }
}
 
/* print atoms */
static void printatom(fp,l,f)
FILE *fp;
pointer l;
Char f;
{
    Char c;
    pointer *v;
    long len;
    Char sp[30];

    if (l == NIL) fprintf(fp, "()");
    else if (l == T) fprintf(fp, "#t");
    else if (l == F) fprintf(fp, "#f");
    else if (l == VOID && f >= 2) fprintf(fp, "#v");
    else if (l == DUMMY) fprintf(fp, "#dummy");
    else if (l == eof_obj) fprintf(fp, "#eof");
    else if (isinteger(l)) fprintf(fp, "%ld", ivalue(l));
    else if (isrational(l)) fprintf(fp, "%ld/%ld", qnvalue(l), qdvalue(l));
    else if (isreal(l)) {
      sprintf(sp, "%G", rvalue(l));
      fprintf(fp, sp);
      if(strchr(sp, '.') == NULL) fprintf(fp, ".0");
    } else if (isstring(l)) printstring(fp, l, f, 1);
    else if (issymbol(l)) printstring(fp, symstring(l), 1, 0);
    else if (ischar(l)) {
        if (isprint(c = (Char)cvalue(l))) {
            if(f) {
              if(c == ' ')
                fprintf(fp, "#\\space");
              else
                fprintf(fp, "#\\%c", c);
            } else
                fprintf(fp, "%c", c);
        } else if(c == '\n') {
            if(f)
              fprintf(fp, "#\\newline");
            else
              fprintf(fp, "%c", c);
        } else if(c == '\t') {
            if(f)
              fprintf(fp, "#\\ht");
            else
              fprintf(fp, "%c", c);
        } else {
            if(f)
              fprintf(fp, OCTALFORMAT, (int)c);
            else
              fprintf(fp, "%c", c);
        }
    } else if (isvector(l)) {
        fprintf(fp, "#(");
        if ((len = vectorlen(l)) != 0) {
            v = vector(l);
            while (--len > 0) {
                printlist(fp, *v++, f);
                fprintf(fp, " ");
            }
            printlist(fp, *v, f);
        }
        fprintf(fp, ")");
    } else if (isoperator(l)) {
        fprintf(fp, "#%d", opnum(l));
    } else if (isprimitive(l)) {
        fprintf(fp, "#<PRIMITIVE-PROCEDURE %s>", primname(l));
    } else if (isclosure(l)) {
        fprintf(fp, "#<CLOSURE>");
    } else if (iscontinuation(l))
        fprintf(fp, "#<CONTINUATION>");
    else if (isport(l)) {
        if (isinputport(l)) fprintf(fp, "#<INPUT PORT>");
        else fprintf(fp, "#<OUTPUT PORT>");
    } else if (isbox(l)) {
        fprintf(fp, "#&");
        printlist(fp, boxcontext(l), f);
    }
}

/* print list cell */
void printlist(fp,l,f)
FILE *fp;
pointer l;
Char f;
{
#ifdef Macintosh
    static int count;

    if (count++ > 100) {
        MacintoshEvent();
        count = 0;
    }
#endif

    if (!ispair(l)) printatom(fp, l, f);
    else {
        if (isprquote(QUOTE, l)) {
            fprintf(fp, "'");
            printlist(fp, cadr(l), f);
        } else if (isprquote(QQUOTE, l)) {
            fprintf(fp, "`");
            printlist(fp, cadr(l), f);
        } else if (isprquote(UNQUOTE, l)) {
            fprintf(fp, ",");
            printlist(fp, cadr(l), f);
        } else if (isprquote(UNQUOTESP, l)) {
            fprintf(fp, ",@");
            printlist(fp, cadr(l), f);
        } else {
            fprintf(fp, "(");
            printlist(fp, car(l), f);
            for (l = cdr(l) ; ispair(l) ; l = cdr(l)) {
                fprintf(fp, " ");
                printlist(fp, car(l), f);
            }
            if (l != NIL) {
                fprintf(fp, " . ");
                printatom(fp, l, f);
            }
            fprintf(fp, ")");
        }
    }
}

/* format print */
void format_print(fp,fmt,args)
FILE *fp;
Char *fmt;
pointer args;
{
    Char ch;

    while ((ch = *fmt++) != '\0') {
        if (ch == '~') {
            switch (*fmt++) {
            case 'a':
            case 'A': printlist(fp, car(args), 0);
                      args = cdr(args);
                      break;
            case 's':
            case 'S': printlist(fp, car(args), 2);
                      args = cdr(args);
                      break;
            case 'c':
            case 'C': fputc((Char)cvalue(car(args)), fp);
                      args = cdr(args);
                      break;
            case 'd':
            case 'D': fprintf(fp, "%ld", ivalue(car(args)));
                      args = cdr(args);
                      break;
            case 'o':
            case 'O': fprintf(fp, "%lo", ivalue(car(args)));
                      args = cdr(args);
                      break;
            case 'x':
            case 'X': fprintf(fp, "%lx", ivalue(car(args)));
                      args = cdr(args);
                      break;
            case 'e':
            case 'E': fprintf(fp, "%E", rvalue(car(args)));
                      args = cdr(args);
                      break;
            case 'f':
            case 'F': fprintf(fp, "%f", rvalue(car(args)));
                      args = cdr(args);
                      break;
            case 'g':
            case 'G': fprintf(fp, "%G", rvalue(car(args)));
                      args = cdr(args);
                      break;
            case '%': fputc('\n', fp);
                      break;
            case 't':
            case 'T': fputc('\t', fp);
                      break;
            case '~': fputc('~', fp);
                      break;
            default:  Error("format -- Illegal format directive", NIL);
            }
        } else fputc(ch, fp);
    }
}

/* caluculate atom width */
static long atom_width(l)
pointer l;
{
    Char c;
    pointer *v;
    long len, total;

    if (l == NIL || l == T || l == F || l == VOID) return ((long)2);
    else if (l == DUMMY) return ((long)strlen("#dummy"));
    else if (l == eof_obj) return((long)strlen("#eof"));
    else if (isinteger(l)) {
        sprintf(strbuff, "%ld", ivalue(l));
        return ((long)strlen(strbuff));
    } else if (isrational(l)) {
        sprintf(strbuff, "%ld/%ld", qnvalue(l), qdvalue(l));
        return ((long)strlen(strbuff));
    } else if (isreal(l)) {
        sprintf(strbuff, "%G", rvalue(l));
        return ((long)strlen(strbuff));
    } else if (isstring(l)) return (stringlen(l) + 2);
    else if (issymbol(l)) return (stringlen(symstring(l)));
    else if (ischar(l)) {
        if (isprint(c = (Char)cvalue(l))) sprintf(strbuff, "#\\%c", c);
        else sprintf(strbuff, OCTALFORMAT, (int)c);
        return ((long)strlen(strbuff));
    } else if (isvector(l)) {
        total = 2;
        if ((len = vectorlen(l)) != 0) {
            v = vector(l);
            while (--len > 0) {
                total += list_width(*v++) + 1;
            }
            total += list_width(*v);
        }
        return (total + 1);
    } else if (isoperator(l)) {
        sprintf(strbuff, "#%d", opnum(l));
        return ((long)strlen(strbuff));
    } else if (isprimitive(l)) {
        sprintf(strbuff, "#<PRIMITIVE-PROCEDURE %s>", primname(l));
        return ((long)strlen(strbuff));
    } else if (isclosure(l)) return ((long)strlen("#<CLOSURE>")); 
    else if (iscontinuation(l)) return ((long)strlen("#<CONTINUATION>"));
    else if (isport(l)) {
        if (isinputport(l)) return ((long)strlen("#<INPUT PORT>"));
        else return ((long)strlen("#<OUTPUT PORT>"));
    } else if (isbox(l)) {
        return ((long)strlen("#&") + list_width(boxcontext(l)));
    } else return ((long)0);
}

/* caluculate list width */
long list_width(l)
pointer l;
{
    long total;

    if (!ispair(l)) return (atom_width(l));
    else {
        if (isprquote(QUOTE, l))
            return (list_width(cadr(l)) + 1);
        else if (isprquote(QQUOTE, l))
            return (list_width(cadr(l)) + 1);
        else if (isprquote(UNQUOTE, l))
            return (list_width(cadr(l)) + 1);
        else if (isprquote(UNQUOTESP, l))
            return (list_width(cadr(l)) + 2);
        else {
            total = 1 + list_width(car(l));
            for (l = cdr(l) ; ispair(l) ; l = cdr(l))
                total += 1 + list_width(car(l));
            if (l != NIL)
                total += 3 + list_width(l);
            return (total + 1);
        }
    }
}
