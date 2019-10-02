/*
 * utils.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/utils.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
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

/* numerical operators */
pointer numerical_operator(op)
register int op;
{    
#define I_TYPE 0 /* integer type */
#define Q_TYPE 1 /* rational type */
#define R_TYPE 2 /* real type */

    register pointer a = Args;
    register long ix, iy;
    register long qnx, qdx, qny, qdy;
    double rx, ry;
    register Char ntype = I_TYPE;
    int compare=0;

    if (a == NIL) { /* no argument */
        switch (op) {
        case NUMOP_ADD:
            return(mk_integer((long)0));
        case NUMOP_MUL:
            return(mk_integer((long)1));
        default:
            Error("No argument in numerical operator", NIL);
        }
    }

    /* get one argument */
    if (isreal(car(a))) {
        ntype = R_TYPE;
        rx = rvalue(car(a));
    } else if (isrational(car(a))) {
        ntype = Q_TYPE;
        qnx = qnvalue(car(a));
        qdx = qdvalue(car(a));
    } else if (isinteger(car(a)))
        ix = ivalue(car(a));
    else
        Error("Illegal type in numerical operator", NIL);
        
    if (cdr(a) == NIL) { /* one argument */
        switch (ntype) {
        case R_TYPE:
            switch (op) {
            case NUMOP_ADD:
            case NUMOP_MUL:
            case NUMOP_MIN:
            case NUMOP_MAX:
                return(mk_real(rx));
            case NUMOP_SUB:
                return(mk_real(-rx));
            case NUMOP_DIV:
                if (rx == 0.0) Error("Divided by zero", NIL);
                return(mk_real(1/rx));
            case NUMOP_ABS:
                if (rx < 0) return(mk_real(-rx));
                else return(mk_real(rx));
            case NUMOP_EXP:
                return(mk_real(exp(rx)));
            case NUMOP_LOG:
                return(mk_real(log(rx)));
            case NUMOP_SIN:
                return(mk_real(sin(rx)));
            case NUMOP_COS:
                return(mk_real(cos(rx)));
            case NUMOP_TAN:
                return(mk_real(tan(rx)));
            case NUMOP_ASIN:
                return(mk_real(asin(rx)));
            case NUMOP_ACOS:
                return(mk_real(acos(rx)));
            case NUMOP_ATAN:
                return(mk_real(atan(rx)));
            case NUMOP_SQRT:
                return(mk_real(sqrt(rx)));
            case NUMOP_ZEROP:
                if (rx == 0.0) return(T); else return(F);
            case NUMOP_POSP:
                if (rx > 0.0) return(T); else return(F);
            case NUMOP_NEGP:
                if (rx < 0.0) return(T); else return(F);
            case NUMOP_NEQ:
            case NUMOP_LESS:
            case NUMOP_GRE:
            case NUMOP_LEQ:
            case NUMOP_GEQ:
                return(T);
            case NUMOP_EVEN:
            case NUMOP_ODD:
                Error("Require integer as argument", NIL);
            case NUMOP_FLOOR:
                return(mk_real(floor(rx)));
            case NUMOP_CEIL:
                return(mk_real(ceil(rx)));
            case NUMOP_TRUN:
                if (rx >= 0) return(mk_real(floor(rx)));
                else return(mk_real(ceil(rx)));
            case NUMOP_ROUND:
                return(mk_real(round(rx)));
            case NUMOP_ONEPLUS:
                return(mk_real(rx + 1.0));
            case NUMOP_ONEMINUS:
                return(mk_real(rx - 1.0));
            default:
                Error("Few arguments in numerical operator", NIL);
            }
        case Q_TYPE:
            switch (op) {
            case NUMOP_ADD:
            case NUMOP_MUL:
            case NUMOP_MIN:
            case NUMOP_MAX:
                return(mk_rational(qnx, qdx));
            case NUMOP_SUB:
                return(mk_rational(-qnx, qdx));
            case NUMOP_DIV:
                if (qnx == 0) Error("Divided by zero", NIL);
                return(mk_rational((qnx < 0) ? -qdx : qdx,
                                   (qnx < 0) ? -qnx : qnx));
            case NUMOP_ABS:
                if (qnx < 0) return(mk_rational(-qnx, qdx));
                else return(mk_rational(qnx, qdx));
            case NUMOP_EXP:
                return(mk_real(exp((double)qnx/(double)qdx)));
            case NUMOP_LOG:
                return(mk_real(log((double)qnx/(double)qdx)));
            case NUMOP_SIN:
                return(mk_real(sin((double)qnx/(double)qdx)));
            case NUMOP_COS:
                return(mk_real(cos((double)qnx/(double)qdx)));
            case NUMOP_TAN:
                return(mk_real(tan((double)qnx/(double)qdx)));
            case NUMOP_ASIN:
                return(mk_real(asin((double)qnx/(double)qdx)));
            case NUMOP_ACOS:
                return(mk_real(acos((double)qnx/(double)qdx)));
            case NUMOP_ATAN:
                return(mk_real(atan((double)qnx/(double)qdx)));
            case NUMOP_SQRT:
                return(mk_real(sqrt((double)qnx/(double)qdx)));
            case NUMOP_ZEROP:
                if (qnx == 0) return(T); else return(F);
            case NUMOP_POSP:
                if (qnx > 0) return(T); else return(F);
            case NUMOP_NEGP:
                if (qnx < 0) return(T); else return(F);
            case NUMOP_NEQ:
            case NUMOP_LESS:
            case NUMOP_GRE:
            case NUMOP_LEQ:
            case NUMOP_GEQ:
                return(T);
            case NUMOP_EVEN:
            case NUMOP_ODD:
                Error("Require integer as argument", NIL);
            case NUMOP_FLOOR:
                return(mk_integer((long)floor((double)qnx/(double)qdx)));
            case NUMOP_CEIL:
                return(mk_integer((long)ceil((double)qnx/(double)qdx)));
            case NUMOP_TRUN:
                if (qnx >= 0) return(mk_integer((long)floor((double)qnx/(double)qdx)));
                else return(mk_integer((long)ceil((double)qnx/(double)qdx)));
            case NUMOP_ROUND:
                return(mk_integer((long)round((double)qnx/(double)qdx)));
            case NUMOP_ONEPLUS:
                return(mk_rational(qnx + qdx, qdx));
            case NUMOP_ONEMINUS:
                return(mk_rational(qnx - qdx, qdx));
            default:
                Error("Few arguments in numerical operator", NIL);
            }
        case I_TYPE:
            switch (op) {
            case NUMOP_ADD:
            case NUMOP_MUL:
            case NUMOP_MIN:
            case NUMOP_MAX:
            case NUMOP_FLOOR:
            case NUMOP_CEIL:
            case NUMOP_TRUN:
            case NUMOP_ROUND:
                return(mk_integer(ix));
            case NUMOP_SUB:
                return(mk_integer(-ix));
            case NUMOP_DIV:
                if (ix == 0) Error("Divided by zero", NIL);
                return (mk_rational((ix < 0) ? (long)-1 : (long)1,
                                    (ix < 0) ? -ix : ix));
            case NUMOP_ABS:
                if (ix < 0) return(mk_integer(-ix));
                else return(mk_integer(ix));
            case NUMOP_ODD:
                if (ix % 2) return(T);
                else return(F);
            case NUMOP_EVEN:
                if (ix % 2) return(F);
                else return(T);
            case NUMOP_EXP:
                return(mk_real(exp((double)ix)));
            case NUMOP_LOG:
                return(mk_real(log((double)ix)));
            case NUMOP_SIN:
                return(mk_real(sin((double)ix)));
            case NUMOP_COS:
                return(mk_real(cos((double)ix)));
            case NUMOP_TAN:
                return(mk_real(tan((double)ix)));
            case NUMOP_ASIN:
                return(mk_real(asin((double)ix)));
            case NUMOP_ACOS:
                return(mk_real(acos((double)ix)));
            case NUMOP_ATAN:
                return(mk_real(atan((double)ix)));
            case NUMOP_SQRT:
                return(mk_real(sqrt((double)ix)));
            case NUMOP_ZEROP:
                if (ix) return(F); else return(T);
            case NUMOP_POSP:
                if (ix > 0) return(T); else return(F);
            case NUMOP_NEGP:
                if (ix < 0) return(T); else return(F);
            case NUMOP_NEQ:
            case NUMOP_LESS:
            case NUMOP_GRE:
            case NUMOP_LEQ:
            case NUMOP_GEQ:
                return(T);
            case NUMOP_ONEPLUS:
                return(mk_integer(ix + 1L));
            case NUMOP_ONEMINUS:
                return(mk_integer(ix - 1L));
            default:
                Error("Few arguments in numerical operator", NIL);
            }
        }
    }

    if(op==NUMOP_NEQ || op==NUMOP_LESS || op==NUMOP_GRE ||
       op==NUMOP_LEQ||op==NUMOP_GEQ)
      compare=1;
    while ((a = cdr(a)) != NIL) { /* more arguments */
        switch (ntype) {
        case R_TYPE:
            if (isreal(car(a)))
                ry = rvalue(car(a));
            else if (isrational(car(a)))
                ry = (double)qnvalue(car(a))/(double)qdvalue(car(a));
            else if (isinteger(car(a)))
                ry = (double)ivalue(car(a));
            else
                Error("Illegal type in numerical operator", NIL);
            break;
        case Q_TYPE:
            if (isreal(car(a))) {
                ntype = R_TYPE;
                rx = (double)qnx/(double)qdx;
                ry = rvalue(car(a));
            } else if (isrational(car(a))) {
                qny = qnvalue(car(a));
                qdy = qdvalue(car(a));
            } else if (isinteger(car(a))) {
                qny = ivalue(car(a));
                qdy = 1;
            } else
                Error("Illegal type in numerical operator", NIL);
            break;
        case I_TYPE:
            if (isreal(car(a))) {
                ntype = R_TYPE;
                rx = (double)ix;
                ry = rvalue(car(a));
            } else if (isrational(car(a))) {
                ntype = Q_TYPE;
                qnx = ix; qdx = 1;
                qny = qnvalue(car(a));
                qdy = qdvalue(car(a));
            } else if (isinteger(car(a)))
                iy = ivalue(car(a));
            else
                Error("Illegal type in numerical operator", NIL);
            break;
        }
        
        switch (ntype) {
        case R_TYPE:
            switch (op) {
            case NUMOP_ADD: rx += ry; break;
            case NUMOP_SUB: rx -= ry; break;
            case NUMOP_MUL: rx *= ry; break;
            case NUMOP_DIV:
                if (ry == 0.0) Error("Divided by zero", NIL);
                rx /= ry; break;
            case NUMOP_QUO:
            case NUMOP_REM:
                Error("Every argument must be integer", NIL);
            case NUMOP_MIN:
                if (rx > ry) rx = ry;
                break;
            case NUMOP_MAX:
                if (rx < ry) rx = ry;
                break;
            case NUMOP_NEQ: 
                if (rx != ry) return(F);
                break;
            case NUMOP_LESS:
                if (rx < ry) rx = ry; else return(F);
                break;
            case NUMOP_GRE:
                if (rx > ry) rx = ry; else return(F);
                break;
            case NUMOP_LEQ:
                if (rx <= ry) rx = ry; else return(F);
                break;
            case NUMOP_GEQ:
                if (rx >= ry) rx = ry; else return(F);
                break;
            case NUMOP_EXPT:
                return(mk_real(pow(rx, ry)));
            default:
                Error("Maybe many arguments in real numerical operator", NIL);
            }
            break;
        case Q_TYPE:
            switch (op) {
            case NUMOP_ADD: qnx = qnx*qdy + qny*qdx;
                         qdx = qdx*qdy;
                         break;
            case NUMOP_SUB: qnx = qnx*qdy - qny*qdx;
                         qdx = qdx*qdy;
                         break;
            case NUMOP_MUL: qnx = qnx*qny;
                         qdx = qdx*qdy;
                         break;
            case NUMOP_DIV:
                if (qny == 0) Error("Divided by zero", NIL);
                qnx = qnx * ((qny < 0) ? -qdy : qdy);
                qdx = qdx * ((qny < 0) ? -qny : qny);
                break;
            case NUMOP_QUO:
            case NUMOP_REM:
                Error("Every argument must be integer", NIL);
            case NUMOP_MIN:
                if (qnx * qdy > qny * qdx) {
                  qnx = qny; qdx = qdy;
                }
                break;
            case NUMOP_MAX:
                if (qnx * qdy < qny * qdx) {
                  qnx = qny; qdx = qdy;
                }
                break;
            case NUMOP_NEQ:
                if (qnx * qdy != qny * qdx) return(F);
                break;
            case NUMOP_LESS:
                if (qnx * qdy < qny * qdx) {
                  qnx = qny; qdx = qdy;
                } else return(F);
                break;
            case NUMOP_GRE:
                if (qnx * qdy > qny * qdx) {
                  qnx = qny; qdx = qdy;
                } else return(F);
                break;
            case NUMOP_LEQ:
                if (qnx * qdy <= qny * qdx) {
                  qnx = qny; qdx = qdy;
                } else return(F);
                break;
            case NUMOP_GEQ:
                if (qnx * qdy >= qny * qdx) {
                  qnx = qny; qdx = qdy;
                } else return(F);
                break;
            case NUMOP_EXPT:
                return(mk_real(pow((double)qnx/(double)qdx,
                                   (double)qny/(double)qdy)));
            default:
                Error("Maybe many arguments in rational numerical operator", NIL);
            }
            break;
        case I_TYPE:
            switch (op) {
            case NUMOP_ADD: ix += iy; break;
            case NUMOP_SUB: ix -= iy; break;
            case NUMOP_MUL: ix *= iy; break;
            case NUMOP_DIV:
                if (iy == 0) Error("Divided by zero", NIL);
                ntype = Q_TYPE;
                qnx = (iy < 0) ? -ix : ix;
                qdx = (iy < 0) ? -iy : iy;
                break;
            case NUMOP_QUO:
                if (iy == 0) Error("Divided by zero", NIL);
                return(mk_integer(ix / iy));
            case NUMOP_REM:
                if (iy == 0) Error("Divided by zero", NIL);
                return(mk_integer(ix % iy));
            case NUMOP_MOD:
                if (iy == 0) Error("Divided by zero", NIL);
                if (ix < 0 && iy < 0 || ix >= 0 && iy > 0) {
                  return(mk_integer(ix % iy));
                } else {
                  ix = ix % iy;
                  return(mk_integer(ix+iy));
                }
            case NUMOP_MIN:
                if (ix > iy) ix = iy;
                break;
            case NUMOP_MAX:
                if (ix < iy) ix = iy;
                break;
            case NUMOP_NEQ: 
                if (ix != iy) return(F);
                break;
            case NUMOP_LESS:
                if (ix < iy) ix = iy; else return(F);
                break;
            case NUMOP_GRE:
                if (ix > iy) ix = iy; else return(F);
                break;
            case NUMOP_LEQ:
                if (ix <= iy) ix = iy; else return(F);
                break;
            case NUMOP_GEQ:
                if (ix >= iy) ix = iy; else return(F);
                break;
            case NUMOP_EXPT:
                return(mk_integer((long)pow((double)ix, (double)iy)));
            default:
                Error("Maybe many arguments in integer numerical operator", NIL);
            }
            break;
        }
    }
    if (compare) return(T);
    switch (ntype) {
    case R_TYPE: return(mk_real(rx));
    case Q_TYPE: return(mk_rational(qnx, qdx));
    case I_TYPE: return(mk_integer(ix));
    }
}

/* generate new symbol */
pointer gensym(head)
Char *head;
{
    static unsigned long symcnt = 0;
    Char *gensymname;
    register pointer x;
    long len;

    gensymname = 
      (Char *)gc_alloc(((head != NULL) ? (long)strlen(head) : 1) + 11L);
    do {
        /* generate a symbol, test whether it is new or not */
        if (head != NULL)
            sprintf(gensymname, "%s%lu", head, symcnt++);
        else
            sprintf(gensymname, "G%lu", symcnt++);
        /* check in oblist */
        len = (long)strlen(gensymname);
        x = oblist[hash(gensymname, len)];
        for ( ; x != NIL; x = cdr(x))
            if (cmpstr(gensymname, len,
                       stringvalue(symstring(car(x))),
                       stringlen(symstring(car(x)))))
                break;
    } while (x != NIL);
    x = mk_symbol(gensymname);
    free(gensymname);
    return (x);
}

/* list ==> vector */
pointer list2vec(l)
register pointer l;
{
    register long len;
    register pointer vec;
    register pointer *v;
    
    TEMP = l;
    for (len = 0 ; ispair(l) ; l = cdr(l)) len++;
    vec = mk_vector(len, NIL);
    v = vector(vec);
    l = TEMP; TEMP = NIL;
    while (len-- > 0) {
        *v++ = car(l);
        l = cdr(l);
    }
    return (vec);
}

/* vector ==> list */
pointer vec2list(x)
pointer x;
/* x must be marked */
{
    register pointer *v;
    register long len;
    
    len = vectorlen(x);
    v = vector(x) + len - 1;
    
    TEMP = NIL;
    while (len-- > 0) TEMP = cons(*v--, TEMP);
    x = TEMP;
    TEMP = NIL;
    return (x);
    
}

/* equivalence of atoms */
int eqv(a,b)
register pointer a;
register pointer b;
{
    if (isstring(a)) {
        if (isstring(b)) return (cmpstr(stringvalue(a), stringlen(a),
                                        stringvalue(b), stringlen(b)));
        else return (0);
    } else if (isinteger(a)) {
        if (isinteger(b)) return (ivalue(a) == ivalue(b));
        else return (0);
    } else if (isrational(a)) {
        if (isrational(b)) return (qnvalue(a) == qnvalue(b) &&
                                   qdvalue(a) == qdvalue(b));
        else return (0);
    } else if (isreal(a)) {
        if (isreal(b)) return (rvalue(a) == rvalue(b));
        else return (0);
    } else return (a == b);
}

pointer string_append(a,b)
pointer a;
pointer b;
{
    register long len1, len2;
    register Char *p1, *p2, *r;
    pointer x;

    len1 = stringlen(a); p1 = stringvalue(a);
    len2 = stringlen(b); p2 = stringvalue(b);
    x = alloc_string(len1 + len2, '\0'); 
    r = stringvalue(x);
    while(len1-- > 0) *r++ = *p1++;
    while(len2-- > 0) *r++ = *p2++;
    return (x);
}

pointer run(x)
pointer x;
{
  if(repflg) {
    Current->R_Reg = x;
    Current->D_Reg = cons(Current->S_Reg,
                        cons(Current->E_Reg,
                             cons(Current->C_Reg, Current->D_Reg)));
    Current->S_Reg = cons(cons(Current->R_Reg, NIL), NIL);
    Current->C_Reg = cons(mk_operator(OP_AP), cons(mk_operator(OP_STOP), NIL));
    Current->E_Reg = NIL;
    Current->R_Reg = symvalue(EXECHOOK);
    SECDR_Cycle(Current);
    /* restore registers */
    Current->S_Reg = car(Current->D_Reg);
    Current->E_Reg = cadr(Current->D_Reg);
    Current->C_Reg = caddr(Current->D_Reg);
    Current->D_Reg = cdddr(Current->D_Reg);
  } else {
    Current->R_Reg = x;
    Current->C_Reg = cons(mk_operator(OP_EXEC), cons(mk_operator(OP_STOP), Current->C_Reg));
    SECDR_Cycle(Current);
  }
  return (Current->R_Reg);
}

pointer compile_run(x)
pointer x;
{
    Current->R_Reg = x;
    Current->D_Reg = cons(Current->S_Reg,
                        cons(Current->E_Reg,
                             cons(Current->C_Reg, Current->D_Reg)));
    Current->S_Reg = cons(cons(Current->R_Reg, NIL), NIL);
    Current->C_Reg = cons(mk_operator(OP_AP), cons(mk_operator(OP_STOP), NIL));
    Current->E_Reg = NIL;
    Current->R_Reg = symvalue(EVALHOOK);
    SECDR_Cycle(Current);
    /* restore registers */
    Current->S_Reg = car(Current->D_Reg);
    Current->E_Reg = cadr(Current->D_Reg);
    Current->C_Reg = caddr(Current->D_Reg);
    Current->D_Reg = cdddr(Current->D_Reg);
    return (Current->R_Reg);
}

pointer bin_load(name, mode, errmode)
pointer name;
pointer mode;
pointer errmode;
{
    FILE *fp;
    pointer x;

    if (!isstring(name)) Error("bin-load -- argument is not string", NIL);    
    if ((fp = fopen(stringvalue(name), "r")) == NULL)
        if (errmode == F || errmode == NIL)
            Error("Unable to open bin-file ~S", cons(name, NIL));
        else
            return(F);
    message(stdout, "; loading bin-file %s\n", stringvalue(name), "", "");
    while ((x = readsexpr(Current, fp)) != eof_obj) {
      if(mode == F || mode == NIL) {
        run(x);
      } else {
        printlist(portfile(cur_outport), run(x), 1);
        message(portfile(cur_outport), "\n", "", "", "");
      }
    }
    fclose(fp);
    if (errmode == F || errmode == NIL)
        return (VOID);
    else
        return(T);
}

pointer load(name, mode, errmode)
pointer name;
pointer mode;
pointer errmode;
{
    FILE *fp;
    pointer x;
    
    if (!isstring(name)) Error("load -- argument is not string", NIL);    
    if ((fp = fopen(stringvalue(name), "r")) == NULL)
        if (errmode == F || errmode == NIL)
            Error("Unable to open scm-file ~S", cons(name, NIL));
        else
            return(F);
    message(stdout, "; loading scm-file %s\n", stringvalue(name), "", "");
    while ((x = readsexpr(Current, fp)) != eof_obj) {
      if(mode == F || mode == NIL) {
        compile_run(x);
      } else {
        printlist(portfile(cur_outport), compile_run(x), 1);
        message(portfile(cur_outport), "\n", "", "", "");
      }
    }
    fclose(fp);
    if (errmode == F || errmode == NIL)
        return (VOID);
    else
        return(T);
}

pointer ex_load(name, mode, errmode)
pointer name;
pointer mode;
pointer errmode;
{
    FILE *fp;
    pointer x;
    int binflg = 1;
    Char wk_name[256];
    Char wk_s_name[2];
    Char* s_name;
    int namelen;

#ifdef THINK_C
    if (!isstring(name) && name != NIL) 
#else
    if (!isstring(name))
#endif
      Error("ex-load -- argument is not string", NIL);

        if( name == NIL )
        {
          s_name=wk_s_name;
          s_name[0]='\0';
        }
        else
        {
       s_name = stringvalue(name);
       namelen = strlen(s_name);

       if (namelen >= 4 && strcmp(s_name+(namelen-4), ".scm") == 0) {
         return(load(name, mode, errmode));
       }
       if (namelen >= 4 && strcmp(s_name+(namelen-4), ".bin") == 0) {
         return(bin_load(name, mode, errmode));
       }
       if (load(name, mode, T) == T) {
         return((errmode == NIL || errmode == F) ? VOID : T);
       }
    }
    strncpy(wk_name, s_name, 250);
    strncat(wk_name, ".bin", 5);
#ifdef THINK_C
    if (s_name[0] == '\0' || (fp = fopen(wk_name, "r")) == NULL) {
#else
    if ((fp = fopen(wk_name, "r")) == NULL) {
#endif
      binflg = 0;
      strncpy(wk_name, s_name, 250);
      strncat(wk_name, ".scm", 5);
#ifdef THINK_C
      if (s_name[0] == '\0' || (fp = fopen(wk_name, "r")) == NULL) {
#else
      if ((fp = fopen(wk_name, "r")) == NULL) {
#endif
#ifdef THINK_C
        if(GetFileName(wk_name)) {
          namelen = strlen(wk_name);
          if (namelen >= 4 && strcmp(wk_name+(namelen-4), ".bin") == 0)
            binflg = 1;
          if((fp = fopen(wk_name, "r")) == NULL)
          if (errmode == F || errmode == NIL)
              Error("Unable to open file.", NIL);
          else
             return(F);
        } else
        if (errmode == F || errmode == NIL)
            Error("Cancel open file.", NIL);
        else
           return(F);
#else
      if (errmode == F || errmode == NIL)
        Error("Unable to open file ~S", cons(name, NIL));
      else
         return(F);
#endif
      }
    }
    if(binflg)
      message(stdout, "; loading bin-file %s\n", stringvalue(name), "", "");
    else
      message(stdout, "; loading scm-file %s\n", stringvalue(name), "", "");

    while ((x = readsexpr(Current, fp)) != eof_obj) {
      if(mode == F || mode == NIL) {
        if(binflg)
          run(x);
        else
          compile_run(x);
      } else {
        if(binflg)
          printlist(portfile(cur_outport), run(x), 1);
        else
          printlist(portfile(cur_outport), compile_run(x), 1);
        message(portfile(cur_outport), "\n", "", "", "");
      }
    }
    fclose(fp);
    if (errmode == F || errmode == NIL)
        return (VOID);
    else
        return(T);
}

#ifdef RUNTIME

# ifdef BSD
#include <sys/types.h>
#include <sys/times.h>
#define CLOCKS_PER_SEC 60
# else
#include <time.h>
# endif

# if defined(BSD)
unsigned long internal_runtime()
{unsigned long total;
 struct tms b;
 times(&b);
 total = b.tms_utime;
 total += b.tms_stime;
 return(total);}

# elif defined(THINK_C)
unsigned long internal_runtime()
{ return(TickCount()); }

# else
unsigned long internal_runtime()
{ return(clock()); }
# endif

double myruntime()
{ return((double)internal_runtime() / (double)CLOCKS_PER_SEC);}
#endif
