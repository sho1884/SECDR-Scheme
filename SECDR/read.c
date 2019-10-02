/*
 * read.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/read.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
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

Char strbuff[STRBUFFSIZE];
static int token();

#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP  10
#define TOK_VECTOR 11
#define TOK_BOX    12
#define TOK_EOF    13

/* check c is delimiter */
static int isdelim(s,c)
register Char *s;
register int c;
{
    while (*s) if (*s++ == (Char)c) return (1);
    return (0);
}

/* read chacters to delimiter */
static Char *readstr(buff,delim,fp)
Char *buff;
Char *delim;
FILE *fp;
{
    Char *p = buff;
    int ch;

    for (;;) {
        if ((ch = getc(fp)) == EOF) {
            *p = '\0';
            return (buff);
        } else if (isdelim(delim, ch))  {
            *p = '\0';
            ungetc(ch, fp);
            return (buff);
        }
        else *p++ = (Char)ch;
    }
}

/* read string expression "xxx...xxx" */
static Char *readstrexp(buff,fp)
Char *buff;
FILE *fp;
{
    Char *p = buff;
    int ch;

    for(;;) {
        if ((ch = getc(fp)) == EOF)
            Error("Illegal end of file", NIL);
        else if (ch == (int)'"') {
            *p = '\0';
            return (buff);
        } else *p++ = (Char)ch;
        if (ch == (int)'\\') {
            p--;
            if ((ch = getc(fp)) == EOF)
                Error("Illegal end of file", NIL);
            else *p++ = (Char)ch;
        }
    }
}

/* read constant */
static Char *readconst(buff,delim,fp)
Char *buff;
Char *delim;
FILE *fp;
{
    Char *p = &buff[1];
    Char *q;
    int ch, count;

    buff[0] = '#';
    for (;;) {
        if ((ch = getc(fp)) == EOF) {
            *p = '\0';
            return (buff);
        } else if (isdelim(delim, ch))  {
            *p = '\0';
            ungetc(ch, fp);
            return (buff);
#ifdef USE_BRACKET
        } if (ch == (int)';' || ch == (int)'(' || ch == (int)')' ||
              ch == (int)'\"' || ch == (int)'[' || ch == (int)']' ) {
#else
        } if (ch == (int)';' || ch == (int)'(' || ch == (int)')' ||
              ch == (int)'\"') { 
#endif
            for(q = p-1, count = 0; q!=&buff[0] && *q == '\\'; q--, count++);

            if (count & 1) *p++ = (Char)ch;
            else {
                *p = '\0';
                ungetc(ch, fp);
                return (buff);
            }
        } else *p++ = (Char)ch;
    }
}

/* get token */
static int token(fp)
FILE *fp;
{
    int ch;
    
    /* skip white characters */
    for (;;) {
        if ((ch = getc(fp)) == EOF) return(TOK_EOF);
        if (!isspace((Char)ch)) break;
    }
    ungetc(ch, fp);
    
    switch (ch = getc(fp)) {
#ifdef USE_BRACKET
        case '['  :
#endif
        case '('  : return (TOK_LPAREN);
#ifdef USE_BRACKET
        case ']'  :
#endif
        case ')'  : return (TOK_RPAREN);
        case '.'  : return (TOK_DOT);
        case '\'' : return (TOK_QUOTE);
        case '`'  : return (TOK_BQUOTE);
        case ','  : if ((ch = getc(fp)) == (int)'@') return (TOK_ATMARK);
                    else { 
                        ungetc(ch, fp); return (TOK_COMMA); }
        case ';'  : return (TOK_COMMENT);
        case '"'  : return (TOK_DQUOTE);
        case '#'  : if ((ch = getc(fp)) == (int)'(') {
                        ungetc(ch, fp); return (TOK_VECTOR); }
                    else if (ch == (int)'&') return (TOK_BOX); 
                    else { 
                        ungetc(ch, fp); return (TOK_SHARP); }
        case EOF  : return (TOK_EOF);
        default   : ungetc(ch, fp); return (TOK_ATOM);
    }
}

/* reverse list --- no make new cells */
static pointer non_alloc_rev(term,list)
pointer term;
pointer list;
{
    register pointer p = list, result = term, q;
    
    while (p != NIL) {
        q = cdr(p);
        cdr(p) = result;
        result = p;
        p = q;
    }
    return (result);
}

/* read S-expression */
pointer readsexpr(cpu, fp)
CPU* cpu;
FILE* fp;
{

/* Operator for Reader */
#define OP_READBEGIN     0
#define OP_READEND       1
#define OP_READSEXPR     2
#define OP_READLIST      3
#define OP_READDOT       4
#define OP_READVECTOR    5
#define OP_READBOX       6
#define OP_READQUOTE     7
#define OP_READQQUOTE    8
#define OP_READUNQUOTE   9
#define OP_READUNQUOTESP 10

    pointer value = NIL, backup;
    int op = OP_READBEGIN, tok, ch;

/* control macros for reader */
#define r_goto(a) { op = (a); goto LOOP; }

#define r_save(a, b) { cpu->D_Reg = cons((b), cpu->D_Reg); \
                       cpu->D_Reg = cons(mk_operator((a)), cpu->D_Reg); }

#define r_return(a) {       \
    value = (a);            \
    op = opnum(car(cpu->D_Reg));     \
    cpu->S_Reg = cadr(cpu->D_Reg);            \
    cpu->D_Reg = cddr(cpu->D_Reg); goto LOOP; }

#define GET_EOF {           \
    cpu->S_Reg = cadr(backup);       \
    cpu->D_Reg = cddr(backup);       \
    return (eof_obj);  }
    
LOOP:
    switch (op) {
case OP_READBEGIN:
    tok = token(fp);
    r_save(OP_READEND, cpu->S_Reg)
    backup = cpu->D_Reg;
    r_goto(OP_READSEXPR)

case OP_READEND:
    return(value);

case OP_READSEXPR:
    switch (tok) {
    case TOK_COMMENT:
        for (;;) {
            if ((ch = getc(fp)) == EOF) GET_EOF
            else if (ch == (int)'\n') break;
        }
        tok = token(fp); r_goto(OP_READSEXPR)
    case TOK_LPAREN:
        tok = token(fp);
        if (tok == TOK_RPAREN) r_return(NIL)
        else if (tok == TOK_EOF)
            Error("Illegal end of file", NIL);
        else {
            r_save(OP_READLIST, NIL)
            r_goto(OP_READSEXPR)
        }
    case TOK_VECTOR:
        r_save(OP_READVECTOR, NIL)
        tok = token(fp);
        r_goto(OP_READSEXPR)
    case TOK_BOX:
        r_save(OP_READBOX, NIL)
        tok = token(fp);
        r_goto(OP_READSEXPR)
    case TOK_QUOTE: 
        r_save(OP_READQUOTE, NIL)
        tok = token(fp);
        r_goto(OP_READSEXPR)
    case TOK_BQUOTE: 
        r_save(OP_READQQUOTE, NIL)
        tok = token(fp);
        r_goto(OP_READSEXPR)
    case TOK_COMMA: 
        r_save(OP_READUNQUOTE, NIL)
        tok = token(fp);
        r_goto(OP_READSEXPR)
    case TOK_ATMARK:
        r_save(OP_READUNQUOTESP, NIL)
        tok = token(fp);
        r_goto(OP_READSEXPR)
    case TOK_ATOM:
#ifdef USE_BRACKET
        r_return (mk_atom(readstr(&strbuff[0], "()[]\";\t\n ", fp)))
#else
        r_return (mk_atom(readstr(&strbuff[0], "()\";\t\n ", fp)))
#endif
    case TOK_DQUOTE:
        r_return (mk_string(readstrexp(&strbuff[0], fp)))
    case TOK_SHARP:
        r_return (mk_const(readconst(&strbuff[0], "\t\n ", fp)))
    case TOK_DOT:
        if (isspace((Char)(ch = getc(fp))))
            Error("syntax error -- Illegal dot expression", NIL);
        else { /* case of float */
            ungetc(ch, fp);
#ifdef USE_BRACKET
            readstr(&strbuff[1], "()[]\";\t\n ", fp);
#else
            readstr(&strbuff[1], "()\";\t\n ", fp);
#endif
            strbuff[0] = '.';
            r_return(mk_atom(&strbuff[0]))
        }
    case TOK_EOF: GET_EOF
    default:
        Error("syntax error -- Illegal token", NIL);
    }
    break;

case OP_READLIST:
    cpu->S_Reg = cons(value, cpu->S_Reg);
    for (;;) {
        if ((tok = token(fp)) == TOK_EOF)
            Error("Illegal end of file", NIL);
        else if (tok == TOK_COMMENT) {
            for (;;) {
                if ((ch = getc(fp)) == EOF)
                    Error("Illegal end of file", NIL);
                else if (ch == (int)'\n') break;
            }
        } else break;
    }
    if (tok == TOK_RPAREN)
        r_return(non_alloc_rev(NIL, cpu->S_Reg))
    else if (tok == TOK_DOT) {
        if (isspace((Char)(ch = getc(fp)))) {
            r_save(OP_READDOT, cpu->S_Reg)
            tok = token(fp);
            r_goto(OP_READSEXPR)
        } else { /* case of float */
            ungetc(ch, fp);
#ifdef USE_BRACKET
            readstr(&strbuff[1], "()[]\";\t\n ", fp);
#else
            readstr(&strbuff[1], "()\";\t\n ", fp);
#endif
            strbuff[0] = '.';
            value = mk_atom(&strbuff[0]);
            r_goto(OP_READLIST)
        }
    } else if (tok == TOK_EOF) Error("Illegal end of file", NIL);
    else {
        r_save(OP_READLIST, cpu->S_Reg);
        r_goto(OP_READSEXPR)
    }

case OP_READDOT:
    if (token(fp) != TOK_RPAREN)
        Error("syntax error -- Illegal dot expression", NIL);
    else r_return(non_alloc_rev(value, cpu->S_Reg))

case OP_READVECTOR:
    r_return(list2vec(value))

case OP_READBOX:
    r_return(mk_box(value))

case OP_READQUOTE:
    r_return(cons(QUOTE, cons(value, NIL)))

case OP_READQQUOTE:
    r_return(cons(QQUOTE, cons(value, NIL)))

case OP_READUNQUOTE:
    r_return(cons(UNQUOTE, cons(value, NIL)))

case OP_READUNQUOTESP:
    r_return(cons(UNQUOTESP, cons(value, NIL)))
    }
}

