/*
 * prims.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /home/argama/s-haya/Scheme/Mulasame/RCS/prims.c,v 0.11 1994/09/14 08:11:57 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"
#include "operator.h"
#ifdef Turtle
#include "turtlegr.c"
#endif
#ifdef THINK_C
#define getenv(env_name) getpref(env_name)
#ifndef Turtle
drawBackupScreen(){}
#endif
#endif

static long list_len(x)
pointer x;
{
    register long i;
    register pointer x1, x2;
    
    if (!ispair(x)) return ( x == NIL ? 0 : -1);

    for (x1 = x, x2 = cdr(x), i = 1;
         x1 != x2; x1 = cdr(x1), x2 = cdr(x2)) {
        if(ispair(x2)) ++i; else break;
        if(ispair(x2 = cdr(x2))) ++i; else break;
        if(x1 == x2) break;
    }
    return( x2 == NIL ? i : -1);
}

double round(x)
double x;
{
    double y, z;
    y = floor(x);
    z = x - y;
    if (z == 0.5) {
        if (y/2 == floor(y/2)) /* y is even */
            return (y);
        else return (y + 1.0);
    } else if (z < 0.5) return (y);
    else return (y + 1.0);
}

Is_Digit(ch,base)
int ch;
unsigned short base;
/* 2, 8, 10, 16 */
{
    if (base != 16) {
        if ('0' <= ch && ch <= '0' + base - 1)
            return 1;
    } else {
        if (('0' <= ch && ch <= '9') ||
            ('A' <= ch && ch <= 'F') ||
            ('a' <= ch && ch <= 'f'))
            return 1;
    }
    return 0;
}

string_eqv(s1,l1,s2,l2)
register Char *s1;
register long l1;
register Char *s2;
register long l2;
{
    register long i;
    
    if (l1 != l2) return (0);
    for (i = 0 ; i < l1 ; i++) {
        if (s1[i] != s2[i]) return (0);
    }
    return (1);
}

ToUpper(ch)
int ch;
{
    if ('a' <= ch && ch <= 'z') return (ch - 'a' + 'A');
    else return (ch);
}

ToLower(ch)
int ch;
{
    if ('A' <= ch && ch <= 'Z') return (ch - 'A' + 'a');
    else return (ch);
}

/* compare string lexicographic order */
string_compare(s1,l1,s2,l2)
register Char *s1;
register long l1;
register Char *s2;
register long l2;
{
    register long i;
    int inv = 1;
    
    if (l1 > l2) {
        Char *s = s1; long l = l1;
        s1 = s2; l1 = l2; s2 = s; l2 = l;
        inv = -1;
    }
     /* here we assume l1 <= l2 */
    for (i = 0 ; i < l1 ; i++) {
        if (s1[i] > s2[i]) return (inv);
        else if (s1[i] < s2[i]) return (inv*(-1));
    }
    if (l1 == l2) return (0);
    else return (inv*(-1));
}

/* compare string lexicographic order (ignore upper and lower) */
string_compare_ci(s1,l1,s2,l2)
register Char *s1;
register long l1;
register Char *s2;
register long l2;
{
    register long i;
    int inv = 1;
    
    if (l1 > l2) {
        Char *s = s1; long l = l1;
        s1 = s2; l1 = l2; s2 = s; l2 = l;
        inv = -1;
    }
     /* here we assume l1 <= l2 */
    for (i = 0 ; i < l1 ; i++) {
        if (ToLower((int)s1[i]) > ToLower((int)s2[i]))
            return (inv);
        else if (ToLower((int)s1[i]) < ToLower((int)s2[i]))
            return (inv*(-1));
    }
    if (l1 == l2) return (0);
    else return (inv*(-1));
}

pointer Args;  /* pointer to argument of primitive procedure */

static pointer safe_car(p)
pointer p;
{
    if(ispair(p))
        return((p)->_object._cons._car);
    else
        Error("car -- First argument is not pair", NIL);
}

static pointer safe_cdr(p)
pointer p;
{
    if(ispair(p))
        return((p)->_object._cons._cdr);
    else
        Error("cdr -- First argument is not pair", NIL);
}

#define safe_caar(p)         safe_car(safe_car(p))
#define safe_cadr(p)         safe_car(safe_cdr(p))
#define safe_cdar(p)         safe_cdr(safe_car(p))
#define safe_cddr(p)         safe_cdr(safe_cdr(p))
#define safe_caaar(p)        safe_car(safe_car(safe_car(p)))
#define safe_caadr(p)        safe_car(safe_car(safe_cdr(p)))
#define safe_cadar(p)        safe_car(safe_cdr(safe_car(p)))
#define safe_caddr(p)        safe_car(safe_cdr(safe_cdr(p)))
#define safe_cdaar(p)        safe_cdr(safe_car(safe_car(p)))
#define safe_cdadr(p)        safe_cdr(safe_car(safe_cdr(p)))
#define safe_cddar(p)        safe_cdr(safe_cdr(safe_car(p)))
#define safe_cdddr(p)        safe_cdr(safe_cdr(safe_cdr(p)))
#define safe_caaaar(p)       safe_car(safe_car(safe_car(safe_car(p))))
#define safe_caaadr(p)       safe_car(safe_car(safe_car(safe_cdr(p))))
#define safe_caadar(p)       safe_car(safe_car(safe_cdr(safe_car(p))))
#define safe_caaddr(p)       safe_car(safe_car(safe_cdr(safe_cdr(p))))
#define safe_cadaar(p)       safe_car(safe_cdr(safe_car(safe_car(p))))
#define safe_cadadr(p)       safe_car(safe_cdr(safe_car(safe_cdr(p))))
#define safe_caddar(p)       safe_car(safe_cdr(safe_cdr(safe_car(p))))
#define safe_cadddr(p)       safe_car(safe_cdr(safe_cdr(safe_cdr(p))))
#define safe_cdaaar(p)       safe_cdr(safe_car(safe_car(safe_car(p))))
#define safe_cdaadr(p)       safe_cdr(safe_car(safe_car(safe_cdr(p))))
#define safe_cdadar(p)       safe_cdr(safe_car(safe_cdr(safe_car(p))))
#define safe_cdaddr(p)       safe_cdr(safe_car(safe_cdr(safe_cdr(p))))
#define safe_cddaar(p)       safe_cdr(safe_cdr(safe_car(safe_car(p))))
#define safe_cddadr(p)       safe_cdr(safe_cdr(safe_car(safe_cdr(p))))
#define safe_cdddar(p)       safe_cdr(safe_cdr(safe_cdr(safe_car(p))))
#define safe_cddddr(p)       safe_cdr(safe_cdr(safe_cdr(safe_cdr(p))))

/* car */
static pointer PRIM_CAR()
{ return(safe_car(car(Args))); }

/* cdr */
static pointer PRIM_CDR()
{ return(safe_cdr(car(Args))); }

/* caar */
static pointer PRIM_CAAR()
{ return(safe_caar(car(Args))); }

/* cadr */
static pointer PRIM_CADR()
{ return(safe_cadr(car(Args))); }

/* cdar */
static pointer PRIM_CDAR()
{ return(safe_cdar(car(Args))); }

/* cddr */
static pointer PRIM_CDDR()
{ return(safe_cddr(car(Args))); }

/* caaar */
static pointer PRIM_CAAAR()
{ return(safe_caaar(car(Args))); }

/* caadr */
static pointer PRIM_CAADR()
{ return(safe_caadr(car(Args))); }

/* cadar */
static pointer PRIM_CADAR()
{ return(safe_cadar(car(Args))); }

/* caddr */
static pointer PRIM_CADDR()
{ return(safe_caddr(car(Args))); }

/* cdaar */
static pointer PRIM_CDAAR()
{ return(safe_cdaar(car(Args))); }

/* cdadr */
static pointer PRIM_CDADR()
{ return(safe_cdadr(car(Args))); }

/* cddar */
static pointer PRIM_CDDAR()
{ return(safe_cddar(car(Args))); }

/* cdddr */
static pointer PRIM_CDDDR()
{ return(safe_cdddr(car(Args))); }

/* caaaar */
static pointer PRIM_CAAAAR()
{ return(safe_caaaar(car(Args))); }

/* caaadr */
static pointer PRIM_CAAADR()
{ return(safe_caaadr(car(Args))); }

/* caadar */
static pointer PRIM_CAADAR()
{ return(safe_caadar(car(Args))); }

/* caaddr */
static pointer PRIM_CAADDR()
{ return(safe_caaddr(car(Args))); }

/* cadaar */
static pointer PRIM_CADAAR()
{ return(safe_cadaar(car(Args))); }

/* cadadr */
static pointer PRIM_CADADR()
{ return(safe_cadadr(car(Args))); }

/* caddar */
static pointer PRIM_CADDAR()
{ return(safe_caddar(car(Args))); }

/* cadddr */
static pointer PRIM_CADDDR()
{ return(safe_cadddr(car(Args))); }

/* cdaaar */
static pointer PRIM_CDAAAR()
{ return(safe_cdaaar(car(Args))); }

/* cdaadr */
static pointer PRIM_CDAADR()
{ return(safe_cdaadr(car(Args))); }

/* cdadar */
static pointer PRIM_CDADAR()
{ return(safe_cdadar(car(Args))); }

/* cdaddr */
static pointer PRIM_CDADDR()
{ return(safe_cdaddr(car(Args))); }

/* cddaar */
static pointer PRIM_CDDAAR()
{ return(safe_cddaar(car(Args))); }

/* cddadr */
static pointer PRIM_CDDADR()
{ return(safe_cddadr(car(Args))); }

/* cdddar */
static pointer PRIM_CDDDAR()
{ return(safe_cdddar(car(Args))); }

/* cddddr */
static pointer PRIM_CDDDDR()
{ return(safe_cddddr(car(Args))); }

/* cons */
static pointer PRIM_CONS()
{ cdr(Args) = cadr(Args); return(Args); }

/* setcar */
static pointer PRIM_SETCAR()
{
    if(!ispair(car(Args)))
        Error("set-car! -- First argument is not pair", NIL);
    caar(Args) = cadr(Args);
    return(car(Args));
}

/* setcdr */
static pointer PRIM_SETCDR()
{
    if(!ispair(car(Args)))
        Error("set-cdr! -- First argument is not pair", NIL);
    cdar(Args) = cadr(Args);
    return(car(Args));
}

/* not */
static pointer PRIM_NOT()
{ return(istrue(car(Args)) ?  F : T); }

/* bool? */
static pointer PRIM_BOOLP()
{ return((car(Args) == T || car(Args) == F) ? T : F); }

/* null? */
static pointer PRIM_NULLP()
{ return((car(Args) == NIL) ? T : F); }

/* symbol? */
static pointer PRIM_SYMBOLP()
{ return(issymbol(car(Args)) ? T : F); }

/* integer? */
static pointer PRIM_INTEGERP()
{ return(isinteger(car(Args)) ? T : F); }

/* proper-rational? */
static pointer PRIM_RATIONALP()
{ return(isrational(car(Args)) ? T : F); }

/* numerator */
static pointer PRIM_RATNUM()
{ return(mk_integer(qnvalue(car(Args)))); }

/* denominator */
static pointer PRIM_RATDEN()
{ return(mk_integer(qdvalue(car(Args)))); }

/* proper-real? */
static pointer PRIM_REALP()
{ return(isreal(car(Args)) ? T : F); }

/* string? */
static pointer PRIM_STRINGP()
{ return(isstring(car(Args)) ? T : F); }

/* primitive-procedure? */
static pointer PRIM_PROCP()
{ return(isprimitive(car(Args)) ? T : F); }

/* closure? */
static pointer PRIM_CLOSUREP()
{ return(isclosure(car(Args)) ? T : F); }
    
/* continuation? */
static pointer PRIM_CONTP()
{ return(iscontinuation(car(Args)) ? T : F); }    

/* pair? */
static pointer PRIM_PAIRP()
{ return(ispair(car(Args)) ? T : F); }

/* atom? */
static pointer PRIM_ATOMP()
{ return(ispair(car(Args)) ? F : T); }

/* eq? */
static pointer PRIM_EQ()
{ return((car(Args) == cadr(Args)) ? T : F); }

/* eqv? */
static pointer PRIM_EQV()
{ return(eqv(car(Args), cadr(Args)) ?  T : F); }

/* memq */
static pointer PRIM_MEMQ()
{
    register pointer x, y;
    
    if (!ispair(y = cadr(Args))) return(F);
    for (x = car(Args); ispair(y) ; y = cdr(y))
        if (x == car(y)) return(y);
    return(F);
}
        
/* memv */
static pointer PRIM_MEMV()
{
    register pointer x, y;

    if (!ispair(y = cadr(Args))) return(F);
    for (x = car(Args); ispair(y) ; y = cdr(y))
        if (eqv(x, car(y))) return(y);
    return(F);
}

/* assq */
static pointer PRIM_ASSQ()
{
    register pointer x, y;

    for( x = car(Args), y = cadr(Args) ; ispair(y) ; y = cdr(y))
      if (x == caar(y)) break;
    return(ispair(y) ? car(y) : F);
}

/* assv */
static pointer PRIM_ASSV()
{
    register pointer x, y;

    for( x = car(Args), y = cadr(Args) ; ispair(y) ; y = cdr(y))
      if (eqv(x, caar(y))) break;
    return(ispair(y) ? car(y) : F);
}
    
/* current-continuation */
static pointer PRIM_CONT()
{ return(mk_continuation(Current->D_Reg)); }
            
/* list */
static pointer PRIM_LIST()
{ return(Args); }
   
/* list? */
static pointer PRIM_LISTP()
{ return((list_len(car(Args))) < 0 ? F : T); }

/* list-tail */
static pointer PRIM_LISTTAIL()
{
    register pointer x;
    register long i;

    if (!ispair(x = car(Args)) && x != NIL)
        Error("list-tail -- First argument is not list", NIL);
    if (!isinteger(cadr(Args)))
        Error("list-tail -- index is not integer", NIL);
    i = ivalue(cadr(Args));
    while (i-- > 0 && ispair(x)) x = cdr(x);
    if (i != -1)
        Error("list-tail -- index is too large", NIL);
    return(x);
}

/* list-ref */
static pointer PRIM_LISTREF()
{
    register pointer x;
    register long i, j;

    if (!ispair(x = car(Args)) && x != NIL)
        Error("list-ref -- First argument is not list", NIL);
    if (!isinteger(cadr(Args)))
        Error("list-ref -- index is not integer", NIL);
    i = ivalue(cadr(Args));
    while (i-- > 0 && ispair(x)) x = cdr(x);
    if (i != -1 || !ispair(x))
        Error("list-ref -- index is too large", NIL);
    return(car(x));
}

/* length */
static pointer PRIM_LENGTH()
{
    long i;

    if ((i = list_len(car(Args))) < 0)
        Error("list-length -- First argument is not list", NIL);
    return(mk_integer(i));
}

/* reverse */
static pointer PRIM_REVERSE()
{ 
    register pointer x = car(Args), y = NIL;

    for ( ; ispair(x) ; x = cdr(x))
        y = cons(car(x), y);
    return(y);
}

/* append */
static pointer PRIM_APPEND()
{
    register pointer x = car(Args), a = NIL, b = cadr(Args);
    
    for ( ; ispair(x) ; x = cdr(x))
        a = cons(car(x), a);
    while (a != NIL) { x = cdr(a); cdr(a) = b; b = a; a = x; }
    return (b);
}

/* write */
static pointer PRIM_WRITE()
{
    register pointer x;
    
    if (cdr(Args) != NIL) {
        x = cadr(Args);
        if (!isport(x) || isinputport(x) || !isopenport(x))
            Error("last argument is not active output port", NIL);
    } else x = cur_outport;
    printlist(portfile(x), car(Args), 2);
    para_fflush(portfile(x));
    return(VOID);
}

/* display */
static pointer PRIM_DISP()
{
    register pointer x;
    
    if (cdr(Args) != NIL) {
        x = cadr(Args);
        if (!isport(x) || isinputport(x) || !isopenport(x))
            Error("last argument is not active output port", NIL);
    } else x = cur_outport;
    printlist(portfile(x), car(Args), 0);
    para_fflush(portfile(x));
    return(VOID);
}

/* print-list */
static pointer PRIM_PRINTLIST()
{
    register pointer x;
    
    if (cddr(Args) != NIL) {
        x = caddr(Args);
        if (!isport(x) || isinputport(x) || !isopenport(x))
            Error("last argument is not active output port", NIL);
    } else x = cur_outport;
    printlist(portfile(x), cadr(Args), (Char)ivalue(car(Args)));
    para_fflush(portfile(x));
    return(VOID);
}

static pointer PRIM_FORMAT() /* %format% */
{
    register pointer x;
    
    if (istrue(x = car(Args))) {
        if (!isport(x) || isinputport(x) || !isopenport(x))
            x = cur_outport;
        format_print(portfile(x), stringvalue(cadr(Args)), cddr(Args));
        para_fflush(portfile(x));
    }
    return(VOID);
}

/* spaces */
static pointer PRIM_SPACES()
{
    register pointer x;
    register long i;
    
    if (cdr(Args) != NIL) {
        x = cadr(Args);
        if (!isport(x) || isinputport(x) || !isopenport(x))
            Error("last argument is not active output port", NIL);
    } else x = cur_outport;
    i = ivalue(car(Args));
    while (i-- > 0) fputc(' ', portfile(x));
    para_fflush(portfile(x));
    return(VOID);
}

/* read */
static pointer PRIM_READ()
{
    register pointer x;
    
    if (Args != NIL) {
        x = car(Args);
        if (!isport(x) || !isinputport(x) || !isopenport(x))
            Error("last argument is not active input port", NIL);
    } else x = cur_inport;
    return(readsexpr(Current, portfile(x)));
}

/* eof-object? */
static pointer PRIM_EOFOBJP()
{ return((car(Args) == eof_obj) ? T : F); }
                
/* input-port? */
static pointer PRIM_INPUTP()
{ return((isport(car(Args)) && isinputport(car(Args))) ? T : F); }
        
/* output-port? */
static pointer PRIM_OUTPUTP()
{ return((isport(car(Args)) && !isinputport(car(Args))) ? T : F); }
        
/* current-input-port */
static pointer PRIM_CURINPUT()
{ return(cur_inport); }
        
/* current-output-port */
static pointer PRIM_CUROUTPUT()
{ return(cur_outport); }
        
/* open-input-file */
static pointer PRIM_OPENINPUT()
{ return(mk_inputport(car(Args), cdr(Args))); }
            
/* open-output-file */
static pointer PRIM_OPENOUTPUT()
{ return(mk_outputport(car(Args), cdr(Args))); }

/* close-input-port or close-output-port */
static pointer PRIM_CLOSEPORT()
{
    if (!isport(car(Args)))
        Error("argument is not port", NIL);
    return(closeport(car(Args)));
}

/* vector? */
static pointer PRIM_VECTORP()
{ return(isvector(car(Args)) ? T : F); }
    
/* make-vector */
static pointer PRIM_MKVECTOR()
{
    if (!isinteger(car(Args)))
        Error("make-vector -- first argument must be integer", NIL);
    return(mk_vector(ivalue(car(Args)), cadr(Args)));
}
        
/* vector-length */
static pointer PRIM_VECLEN()
{
    if (!isvector(car(Args)))
        Error("vector-length -- first argument is not vector", NIL);
    return(mk_integer(vectorlen(car(Args))));
}
        
/* vector-ref */
static pointer PRIM_VECREF()
{
    if (!isvector(car(Args)))
        Error("vector-ref -- first argument is not vector", NIL);
    if (ivalue(cadr(Args)) >= vectorlen(car(Args)))
        Error("vector-ref -- vector index is too large", NIL);
    return((vector(car(Args)))[ivalue(cadr(Args))]);
}
        
/* vector-set! */
static pointer PRIM_VECSET()
{
    if (!isvector(car(Args)))
        Error("vector-set! -- first argument is not vector", NIL);
    if (ivalue(cadr(Args)) >= vectorlen(car(Args)))
        Error("vector-set! -- vector index is too large", NIL);
    (vector(car(Args)))[ivalue(cadr(Args))] = caddr(Args);
    return(caddr(Args));
}
    
/* vector->list */
static pointer PRIM_VEC2LIST()
{
    if (!isvector(car(Args)))
        Error("vector->list -- first argument is not vector", NIL);
    return(vec2list(car(Args)));
}
    
/* list->vector */
static pointer PRIM_LIST2VEC()
{
    long i;

    if ((i = list_len(car(Args))) < 0)
        Error("list->vector -- First argument is not list", NIL);
    return(list2vec(car(Args)));
}

/* make-string */
static pointer PRIM_MKSTR()
{ return(alloc_string(ivalue(car(Args)),
                      (cdr(Args) != NIL) ? cvalue(cadr(Args)) : 'a')); }

/* string-length */
static pointer PRIM_STRLEN()
{
    if (!isstring(car(Args)))
        Error("string-length -- first argument must be string", NIL);
    return(mk_integer(stringlen(car(Args))));
}
        
/* string-ref */
static pointer PRIM_STRREF()
{
    register pointer x;
    register long i;
    
    if (!isstring(x = car(Args)))
        Error("string-ref -- first argument must be string", NIL);
    i = ivalue(cadr(Args));
    if (stringlen(x) <= i)
        Error("string-ref -- index is too large", NIL);
    return((pointer)&char_table[(stringvalue(car(Args)))[i]]);
}
       
/* string-set! */
static pointer PRIM_STRSET()
{
    register pointer x;
    register long i;
    
    if (!isstring(x = car(Args)))
        Error("string-set! -- first argument must be string", NIL);
    i = ivalue(cadr(Args));
    if (stringlen(x) <= i)
        Error("string-set! -- index is too large", NIL);
    (stringvalue(x))[i] = cvalue(caddr(Args));
    return(x);
}
        
/* substring */
static pointer PRIM_SUBSTR()
{
    register long i, j;
    if (!isstring(car(Args)))
        Error("substring -- first argument must be string", NIL);
    i = ivalue(cadr(Args));
    j = ivalue(caddr(Args));
    return(mk_substring(stringvalue(car(Args)), i, j));
}

/* string-append */
static pointer PRIM_STRAPPEND()
{
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string-append -- arguments must be string", NIL);
    return(string_append(car(Args), cadr(Args)));
}

/* string=? */
static pointer PRIM_STREQ()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string=? -- arguments must be string", NIL);
    else if (string_eqv(stringvalue(car(Args)), stringlen(car(Args)),
                        stringvalue(cadr(Args)), stringlen(cadr(Args))))
        return (T);
    else return (F);
}

/* string<? */
static pointer PRIM_STRLESS()
{
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string<? -- arguments must be string", NIL);
    else if (string_compare(stringvalue(car(Args)), stringlen(car(Args)),
                            stringvalue(cadr(Args)), stringlen(cadr(Args))) < 0)
      return(T);
    else return(F);
}

/* string>? */
static pointer PRIM_STRGRE()
{
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string>? -- arguments must be string", NIL);
    else if (string_compare(stringvalue(car(Args)), stringlen(car(Args)),
                            stringvalue(cadr(Args)), stringlen(cadr(Args))) > 0)
        return (T);
    else return (F);
}

/* string<=? */
static pointer PRIM_STRLEQ()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string<=? -- arguments must be string", NIL);
    else if (string_compare(stringvalue(car(Args)), stringlen(car(Args)),
                            stringvalue(cadr(Args)), stringlen(cadr(Args))) <= 0)
        return (T);
    else return (F);
}

/* string>=? */
static pointer PRIM_STRGEQ()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string>=? -- arguments must be string", NIL);
    else if (string_compare(stringvalue(car(Args)), stringlen(car(Args)),
                            stringvalue(cadr(Args)), stringlen(cadr(Args))) >= 0)
        return (T);
    else return (F);
}

/* string-ci=? */
static pointer PRIM_STRCIEQ()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string-ci=? -- arguments must be string", NIL);
    else if (string_compare_ci(stringvalue(car(Args)), stringlen(car(Args)),
                               stringvalue(cadr(Args)), stringlen(cadr(Args))) == 0)
        return (T);
    else return (F);
}

/* string-ci<? */
static pointer PRIM_STRCILESS()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string-ci<? -- arguments must be string", NIL);
    else if (string_compare_ci(stringvalue(car(Args)), stringlen(car(Args)),
                               stringvalue(cadr(Args)), stringlen(cadr(Args))) < 0)
        return (T);
    else return (F);
}

/* string-ci>? */
static pointer PRIM_STRCIGRE()
{
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string-ci>? -- arguments must be string", NIL);
    else if (string_compare_ci(stringvalue(car(Args)), stringlen(car(Args)),
                               stringvalue(cadr(Args)), stringlen(cadr(Args))) > 0)
        return (T);
    else return (F);
}

/* string-ci<=? */
static pointer PRIM_STRCILEQ()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string-ci<=? -- arguments must be string", NIL);
    else if (string_compare_ci(stringvalue(car(Args)), stringlen(car(Args)),
                               stringvalue(cadr(Args)), stringlen(cadr(Args))) <= 0)
        return (T);
    else return (F);
}

/* string-ci>=? */
static pointer PRIM_STRCIGEQ()
{    
    if (!isstring(car(Args)) || !isstring(cadr(Args)))
        Error("string-ci>=? -- arguments must be string", NIL);
    else if (string_compare_ci(stringvalue(car(Args)), stringlen(car(Args)),
                               stringvalue(cadr(Args)), stringlen(cadr(Args))) >= 0)
        return (T);
    else return (F);
}

/* symbol->string */
static pointer PRIM_SYM2STR()
{
    if (!issymbol(car(Args)))
        Error("symbol->string -- first argument must be symbol", NIL);
   return(mk_substring(stringvalue(symstring(car(Args))), (long)0,
                       stringlen(symstring(car(Args)))));
}

/* string->symbol */
static pointer PRIM_STR2SYM()
{
    if (!isstring(car(Args)))
        Error("string->symbol -- first argument must be string", NIL);
    return(_mk_symbol(stringvalue(car(Args)), stringlen(car(Args))));
}

/* string->number */
static pointer PRIM_STR2NUM()
{
    pointer x;
    unsigned short b;

    Char *p, *r;
    Char point = 0;
    
    if (!isstring(car(Args))) Error("string->number -- Illegal arg", NIL);
    if (cadr(Args) != NIL && isinteger(cadr(Args)))
      b = (unsigned short)ivalue(cadr(Args));
    else b = 10;
    if (b != 2 && b != 8 && b != 10 && b != 16)
      Error("string->number -- Illegal radix", NIL);

    if(b == 10) {
      for(p=stringvalue(car(Args)); *p != '\0'; p++) *p=ToLower(*p);

      p = stringvalue(car(Args));
      if (*p == 'e' || *p == 'E' || *p == '/') return (F);
      if (*p == '.' && !isdigit(p[1])) return (F);
      if (*p == '-' || *p == '+') ++p;
      if (*p == '\0') return (F);
      while (*p != '\0' && 
             (isdigit(*p)||*p == '.'||*p == 'e'||*p == 'E'||*p == '/')) {
        if (*p == '.') {
          if (point) return (F);
          else point = 1;
        } else if (*p == 'e'||*p == 'E') {
          ++p;
          if (*p == '-' || *p == '+') ++p;
          if (*p == '\0') return (F);
          while (*p != '\0' && isdigit(*p)) ++p;
          if (*p != '\0') return(F);
          else break;
        } else if (*p == '/') {
          if (point || p[1] == '\0') return (F);
        }
        ++p;
      }
      if (*p != '\0' && *p != '#') return(F);
    }
    if ((x=trans_base(stringvalue(car(Args)), b)) == NIL) return(F);
    else return(x);
}

/* number->string */
static pointer PRIM_NUM2STR()
{
    pointer x;
    unsigned short b;
    int rc=1;
    Char *sp;
    long ix, wx;
    int count, flg;
    int minusflg;

    if (isinteger(car(Args))) {
        if (cadr(Args) != NIL && isinteger(cadr(Args)))
            b = (unsigned short)ivalue(cadr(Args));
        else b = 10;
        if (b != 2 && b != 8 && b != 10 && b != 16)
            Error("number->string -- Illegal radix", NIL);
        switch (b) {
        case 2:
          minusflg = ((ix=ivalue(car(Args))) < 0 ? 1 : 0);
          sp=strbuff;
          if(minusflg) {
            *strbuff = '-';
            sp++;
            ix = -ix;
          }
          for(count=8*sizeof(long)-1, flg=0;
              count!=0; count--) {
            wx = ix >> (count-1);
            if((*sp= ((wx & 1) ? '1' : '0'))=='1') flg=1;
            if(flg) sp++;
          }
          *sp='\0';
          break;
        case 8:
          minusflg = ((ix=ivalue(car(Args))) < 0 ? 1 : 0);
          sp = strbuff;
          if(minusflg) {
            *strbuff = '-';
            sp++;
            ix = -ix;
          }
          sprintf(sp, "%lo", ix);
          break;
        case 10:
          minusflg = ((ix=ivalue(car(Args))) < 0 ? 1 : 0);
          sp = strbuff;
          if(minusflg) {
            *strbuff = '-';
            sp++;
            ix = -ix;
          }
          sprintf(sp, "%ld", ix);
          break;
        case 16:
          minusflg = ((ix=ivalue(car(Args))) < 0 ? 1 : 0);
          sp = strbuff;
          if(minusflg) {
            *strbuff = '-';
            sp++;
            ix = -ix;
          }
          sprintf(sp, "%lx", ix);
          break;
        }
        if(rc == 1)
          x = mk_string(strbuff);
        else
          Error("string->number -- Illegal arg", NIL);
    } else if (isrational(car(Args))) {
        if (cadr(Args) != NIL && isinteger(cadr(Args)))
            b = (unsigned short)ivalue(cadr(Args));
        else b = 10;
        if (b != 10)
            Error("number->string -- Illegal radix", NIL);
        sprintf(&strbuff[0], "%ld", qnvalue(car(Args)));
        for(sp = &strbuff[0]; *sp != '\0'; sp++);
        *sp = '/'; sp++;
        sprintf(sp, "%ld", qdvalue(car(Args)));
        if(rc == 1)
          x = mk_string(strbuff);
        else
          Error("string->number -- Illegal arg", NIL);
    } else if (isreal(car(Args))) {
        sprintf(&strbuff[0], "%g", rvalue(car(Args)));
        x = mk_string(strbuff);
    } else
        Error("number->string -- Illegal arg", NIL);
    
    return(x);
}

/* char? */
static pointer PRIM_CHARP()
{ return(ischar(car(Args)) ? T : F); }

/* char->integer */
static pointer PRIM_CHAR2INT()
{ return(mk_integer((long)cvalue(car(Args)))); }

/* integer->char */
static pointer PRIM_INT2CHAR()
{ return((pointer)&char_table[ivalue(car(Args)) & CHARMASK]); }

/* char=? */
static pointer PRIM_CHAREQ()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char=? -- Illegal args", NIL);
    if (cvalue(car(Args)) == cvalue(cadr(Args))) return(T);
    else return(F);
}
   
/* char<? */
static pointer PRIM_CHARLESS()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char<? -- Illegal args", NIL);
    if (cvalue(car(Args)) < cvalue(cadr(Args))) return(T);
    else return(F);
}

/* char>? */
static pointer PRIM_CHARGRE()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char>? -- Illegal args", NIL);
    if (cvalue(car(Args)) > cvalue(cadr(Args))) return(T);
    else return(F);
}

/* char<=? */
static pointer PRIM_CHARLEQ()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char<=? -- Illegal args", NIL);
    if (cvalue(car(Args)) <= cvalue(cadr(Args))) return(T);
    else return(F);
}

/* char>=? */
static pointer PRIM_CHARGEQ()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char>=? -- Illegal args", NIL);
    if (cvalue(car(Args)) >= cvalue(cadr(Args))) return(T);
    else return(F);
}

/* char-ci=? */
static pointer PRIM_CHARCIEQ()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char-ci=? -- Illegal args", NIL);
    if (ToLower((int)cvalue(car(Args))) == ToLower((int)cvalue(cadr(Args))))
        return(T);
    else return(F);
}
   
/* char-ci<? */
static pointer PRIM_CHARCILESS()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char-ci<? -- Illegal args", NIL);
    if (ToLower((int)cvalue(car(Args))) < ToLower((int)cvalue(cadr(Args))))
        return(T);
    else return(F);
}

/* char-ci>? */
static pointer PRIM_CHARCIGRE()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char-ci>? -- Illegal args", NIL);
    if (ToLower((int)cvalue(car(Args))) > ToLower((int)cvalue(cadr(Args))))
        return(T);
    else return(F);
}

/* char-ci<=? */
static pointer PRIM_CHARCILEQ()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char-ci<=? -- Illegal args", NIL);
    if (ToLower((int)cvalue(car(Args))) <= ToLower((int)cvalue(cadr(Args))))
        return(T);
    else return(F);
}

/* char-ci>=? */
static pointer PRIM_CHARCIGEQ()
{    
    if (!ischar(car(Args)) || !ischar(cadr(Args)))
        Error("char-ci>=? -- Illegal args", NIL);
    if (ToLower((int)cvalue(car(Args))) >= ToLower((int)cvalue(cadr(Args))))
        return(T);
    else return(F);
}

/* char-alphabetic? */
static pointer PRIM_CHARALPHA()
{
    int ch;
    if (!ischar(car(Args)))
        Error("char-alphabetic? -- arg is not char", NIL);
    ch = cvalue(car(Args));
    if (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z'))
        return (T);
    else
        return (F);
}

/* char-numeric? */
static pointer PRIM_CHARNUMP()
{
    int ch;
    if (!ischar(car(Args)))
        Error("char-numeric? -- arg is not char", NIL);
    ch = cvalue(car(Args));
    if ('0' <= ch && ch <= '9')
        return (T);
    else
        return (F);
}

/* char-whitespace? */
static pointer PRIM_CHARWHITE()
{
    int ch;
    if (!ischar(car(Args)))
        Error("char-whitespace? -- arg is not char", NIL);
    ch = cvalue(car(Args));
    if (isspace(ch))
        return (T);
    else
        return (F);
}

/* char-upper-case? */
static pointer PRIM_CHARUPPER()
{
    int ch;
    if (!ischar(car(Args)))
        Error("char-upper-case? -- arg is not char", NIL);
    ch = cvalue(car(Args));
    if ('A' <= ch && ch <= 'Z')
        return (T);
    else
        return (F);
}

/* char-lower-case? */
static pointer PRIM_CHARLOWER()
{
    int ch;
    if (!ischar(car(Args)))
        Error("char-lower-case? -- arg is not char", NIL);
    ch = cvalue(car(Args));
    if ('a' <= ch && ch <= 'z')
        return (T);
    else
        return (F);
}

/* char-upcase */
static pointer PRIM_CHARUP()
{    
    return((pointer)&char_table[ToUpper((int)cvalue(car(Args)))]);
}
    
/* char-downcase */
static pointer PRIM_CHARDOWN()
{    
    return((pointer)&char_table[ToLower((int)cvalue(car(Args)))]);
}

/* read-char */
static pointer PRIM_READCHAR()
{
    register pointer x;
    register int ch;
    
    if (Args != NIL) {
        x = car(Args);
        if (!isport(x) || !isinputport(x) || !isopenport(x))
            Error("last argument is not active input port", NIL);
    } else x = cur_inport;
    {   ch = para_getc(portfile(x));
        return((ch == EOF) ? eof_obj : (pointer)&char_table[ch]);
    }
}

/* peek-char */
static pointer PRIM_PEEKCHAR()
{
    register pointer x;
    register int ch;
    
    if (Args != NIL) {
        x = car(Args);
        if (!isport(x) || !isinputport(x) || !isopenport(x))
            Error("last argument is not active input port", NIL);
    } else x = cur_inport;
    {   ch = para_getc(portfile(x));
        if(ch != EOF)
                para_ungetc(ch, portfile(x));
        return((ch == EOF) ? eof_obj : (pointer)&char_table[ch]);
    }
}

/* write-char */
static pointer PRIM_WRITECHAR()
{
    register pointer x;
    
    if (!ischar(car(Args)))
        Error("write-char -- argument is not character", NIL);
    if (cdr(Args) != NIL) {
        x = cadr(Args);
        if (!isport(x) || isinputport(x) || !isopenport(x))
            Error("last argument is not active output port", NIL);
    } else x = cur_outport;
    fputc((Char)cvalue(car(Args)), portfile(x));
    para_fflush(portfile(x));
    return(VOID);
}

/* char-ready? */
static pointer PRIM_CHARREADY()
{
    register pointer x;
    
    if (Args != NIL) {
        x = car(Args);
        if (!isport(x) || !isinputport(x) || !isopenport(x))
            Error("last argument is not active input port", NIL);
    } else x = cur_inport;
    return(feof(portfile(x)) ? F : T);
}

/* put */
static pointer PRIM_PUT()
{
    register pointer x, y;
    
    if (!hasprop(car(Args)) || !hasprop(cadr(Args)))
        Error("Illegal use of put", NIL);
    y = cadr(Args);
    if (y == SYMBOLVALUE) symvalue(car(Args)) = caddr(Args);
    else if (y == SYMBOLNAME) {
        if (isstring(caddr(Args)))
            symstring(car(Args)) = caddr(Args);
        else
            Error("put -- Unable to put non-string value to ~S",
                  cons(SYMBOLNAME, NIL));
    } else {
        for (x = symprop(car(Args)) ; x != NIL ; x = cdr(x))
            if (caar(x) == y) break;
        if (x != NIL) cdar(x) = caddr(Args);
        else symprop(car(Args)) = cons(cons(y, caddr(Args)),
                                       symprop(car(Args)));
    }
    return(VOID);
}

/* get */
static pointer PRIM_GET()
{
    register pointer x, y;
    
    if (!hasprop(car(Args)) || !hasprop(cadr(Args)))
        Error("Illegal use of get", NIL);
    y = cadr(Args);
    if (y == SYMBOLVALUE) return(symvalue(car(Args)));
    else if (y == SYMBOLNAME) return(symstring(car(Args)));
    else {
        for (x = symprop(car(Args)) ; x != NIL ; x = cdr(x))
            if (caar(x) == y) break;
        return((x != NIL) ? cdar(x) : F);
    }
}

/* error */
static pointer PRIM_ERROR()
{
    pointer x = Args;

    for ( ; !isstring(car(x)) && x != NIL ; x = cdr(x) ) {
        printlist(stderr, car(x), 2);
        fprintf(stderr, ": ");
    }
    if (x == NIL) 
        Error("error -- arguments must contain string", NIL);
    Error(stringvalue(car(x)), cdr(x));
    return (VOID);
}

/*quit */
static pointer PRIM_QUIT()
{
    quit(0);
    return (VOID);
}
    
/* new-segment */
static pointer PRIM_NEWSEGMENT()
{
    if (!isinteger(car(Args)))
        Error("new-segment -- argument must be integer", NIL);
    fprintf(stdout, "allocate %d new segments\n",
                    alloc_cellseg((int)ivalue(car(Args))));
    return(VOID);
}

/* set-promise */
static pointer PRIM_SETPROMISE()
{
    flag(car(Args)) |= F_PROMISE;
    return(car(Args));
}

/* promise? */
static pointer PRIM_PROMISEP()
{ return((flag(car(Args))&F_PROMISE) ? T : F); }
        
#ifdef DEBUG
/* %set-future% */
static pointer PRIM_SETFUTURE()
{
    flag(car(Args)) |= F_FUTURE;
    return(car(Args));
}

/* %unset-future% */
static pointer PRIM_UNSETFUTURE()
{
    flag(car(Args)) &= (~F_FUTURE);
    return(car(Args));
}
#endif

#ifdef Mulasame
/* future? */
static pointer PRIM_FUTUREP()
{ return((flag(car(Args))&F_FUTURE) ? T : F); }

/* make-place-holder */
static pointer PRIM_MKPLACEHOLDER()
{
    return (mk_ph());
}

/* place-holder-set! */
static pointer PRIM_PLACEHOLDERSET()
{
    register pointer x;

    x = car(Args);
    if(isplaceholder(x) && (flag(x)&F_FUTURE)) {
        flag(x) &= (~F_FUTURE);
        return(phobject(x) = cadr(Args));
    } else
	Error("place-holder-set! -- first argument must be a non-instantiated place holder", NIL);
}

/* place-holder? */
static pointer PRIM_PLACEHOLDERP()
{ return(isplaceholder(car(Args)) ? T : F); }

/* %touch% */
static pointer PRIM_TOUCH()
{
    if(flag(car(Args))&F_FUTURE)
      Current->cpu_status = WAIT;
    return(car(Args));
}

/* %ph-object% */
static pointer PRIM_PHOBJECT()
{
    if(scmtype(car(Args)) == T_PLACEHOLDER)
      if(flag(car(Args))&F_FUTURE)
	Error("%ph-object% -- first argument is not an instantiated place holder", NIL);
      else
	return(phobject(car(Args)));
    return(car(Args));
}

/* wait */
static pointer PRIM_WAIT()
{
    if(!issemaphore(car(Args)))
      Error("wait -- first argument must be semaphore", NIL);
    if(semaphorenum(car(Args)) == 0)
      flag(car(Args)) |= F_WAIT;
    else
      semaphorenum(car(Args))--;
    return(car(Args));
}

/* signal */
static pointer PRIM_SIGNAL()
{
    if(!issemaphore(car(Args)))
      Error("wait -- first argument must be semaphore", NIL);
    if(semaphorenum(car(Args)) == 0)
      flag(car(Args)) |= F_SIGNAL;
    else
      semaphorenum(car(Args))++;
    return(car(Args));
}

/* %task-switch% */
static pointer PRIM_TASKSW()
{
    step = 0;
    return(VOID);
}

/* %task-switch-on% */
static pointer PRIM_TASKSWON()
{
    switchflg = 1;
    return(T);
}

/* %task-switch-off% */
static pointer PRIM_TASKSWOFF()
{
    switchflg = 0;
    return(F);
}

/* %task-switch?% */
static pointer PRIM_TASKSWP()
{
    return(switchflg ? T : F);
}

/* %cpu-step% */
static pointer PRIM_CPUSTEP()
{
    if (car(Args) == NIL)
        return(mk_integer(Master.cpu_step));
    else if (iscpu(car(Args)))
        return(mk_integer(cpustep(car(Args))));
    else
        Error("%cpu-step% -- argument is not CPU", NIL);
}

/* %cpu-step-set!% */
static pointer PRIM_CPUSTEPSET()
{
    int i;
    if (!(isinteger(cadr(Args)) && (i = ivalue(cadr(Args))) > 0))
        Error("%cpu-step-set!% -- last argument must be positive integer", NIL);
    else if (car(Args) == T)
        Master.cpu_step = i;
    else if (iscpu(car(Args)))
        cpustep(car(Args)) = i;
    else
        Error("%cpu-step-set!% -- first argument is neither CPU nor #t", NIL);
    return(VOID);
}

/* %cpu-status% */
static pointer PRIM_CPUSTATUS()
{
    if (car(Args) == NIL)
        return(mk_integer(Master.cpu_status));
    else if (iscpu(car(Args)))
        return(mk_integer(cpustatus(car(Args))));
    else
        Error("%cpu-status% -- argument is not CPU", NIL);
}
#endif

/* gc */
static pointer PRIM_GC()
{ gc(NIL, NIL); return(VOID); }

/* gc-verbose-on */
static pointer PRIM_VERBOSEON()
{ gc_verbose = 1; return(VOID); }

/* gc-verbose-off */
static pointer PRIM_VERBOSEOFF()
{ gc_verbose = 0; return(VOID); }
            
/* file-exists? */
static pointer PRIM_FILEEXISTSP()
{
    FILE * fp;
    
    if (!isstring(car(Args)))
        Error("file-exists? -- first argument must be string", NIL);
    if ((fp=fopen(stringvalue(car(Args)), "r"))==NULL)
        return(F);
    fclose(fp);
    return(T);
}
       
#ifdef THINK_C
#ifndef ASL
/* transcript-on */
static pointer PRIM_TRANSON()
{
    if (!isstring(car(Args)))
        Error("transcript-on -- first argument must be string", NIL);
    cecho2file(stringvalue(car(Args)), 0, stdout);
    return(T);
}
       
/* cgotoxy */
static pointer PRIM_CGOTOXY()
{
    if (!isinteger(car(Args)) || !isinteger(cadr(Args)))
        Error("cgotoxy -- argument must be integer", NIL);
    cgotoxy(ivalue(car(Args)), ivalue(cadr(Args)), stdout);
    return(T);
}
#endif
#ifdef Mulasame
/* sub-transcript-on */
static pointer PRIM_SUBTRANSON()
{
    if (!isstring(car(Args)))
        Error("sub-transcript-on -- first argument must be string", NIL);
    cecho2file(stringvalue(car(Args)), 0, substdout);
    return(T);
}

/* sub-cgotoxy */
static pointer PRIM_SUBCGOTOXY()
{
    if (!isinteger(car(Args)) || !isinteger(cadr(Args)))
        Error("sub-cgotoxy -- argument must be integer", NIL);
    cgotoxy(ivalue(car(Args)), ivalue(cadr(Args)), substdout);
    return(T);
}
#endif
#endif

/* load */
static pointer PRIM_LOAD()
{ return(load(car(Args), cadr(Args), caddr(Args))); }
    
/* bin-load */
static pointer PRIM_BLOAD()
{ return(bin_load(car(Args), cadr(Args), caddr(Args))); }

/* ex-load */
static pointer PRIM_ELOAD()
{ return(ex_load(car(Args), cadr(Args), caddr(Args))); }

/* eval */
static pointer PRIM_EVAL()
{
    CPU* cpu;

#ifdef Mulasame
    if (cdr(Args) == NIL)
#endif
        cpu = Current;
#ifdef Mulasame
    else if (iscpu(cadr(Args)))
        cpu = cpuregister(cadr(Args));
    else
        Error("last argument is not CPU", NIL);
#endif    
    cpu->R_Reg = car(Args);
    cpu->D_Reg = cons(cpu->S_Reg,
                        cons(cpu->E_Reg,
                             cons(cpu->C_Reg, cpu->D_Reg))); /* save registers */
    cpu->S_Reg = cons(cons(cpu->R_Reg, NIL), NIL);
    cpu->C_Reg = cons(mk_operator(OP_AP), 
             cons(mk_operator(OP_EXEC), cons(mk_operator(OP_STOP), NIL)));
    cpu->E_Reg = NIL;
    cpu->R_Reg = symvalue(COMPILE);
    SECDR_Cycle(cpu);
    /* restore registers */
    cpu->S_Reg = car(cpu->D_Reg);
    cpu->E_Reg = cadr(cpu->D_Reg);
    cpu->C_Reg = caddr(cpu->D_Reg);
    cpu->D_Reg = cdddr(cpu->D_Reg);
    return (cpu->R_Reg);
}

#ifdef DEBUG
/* %force% */
static pointer PRIM_FORCE()
{
    CPU* cpu;

    if (cdr(Args) == NIL)
        cpu = &Slave;
    else if (iscpu(cadr(Args)))
        cpu = cpuregister(cadr(Args));
    else
        Error("last argument is not CPU", NIL);
    cpu->E_Reg = cons(NIL, closure_env(car(Args)));
    cpu->S_Reg = NIL;
    cpu->D_Reg = NIL;
    cpu->R_Reg = NIL;
    cpu->C_Reg = closure_code(car(Args));
    SECDR_Cycle(cpu);
    return (cpu->R_Reg);
}
#endif

#ifdef Mulasame
/* migrate */
static pointer PRIM_FUTURE()
{
    CPU* cpu;

    if (!isclosure(car(Args)))
        Error("%future% -- first argument must be closure", NIL);
    else if (cdr(Args) != NIL && iscpu(cadr(Args)))
        cpu = cpuregister(cadr(Args));
    else
        Error("last argument is not CPU", NIL);
    cpustatus(cadr(Args)) = SUSPEND;
    cpucont(cadr(Args)) = mk_ph();
    cpu->E_Reg = cons(NIL, closure_env(car(Args)));
    cpu->S_Reg = NIL;
    cpu->D_Reg = NIL;
    cpu->R_Reg = NIL;
    cpu->C_Reg = closure_code(car(Args));
    return (cpucont(cadr(Args)));
}

/* wakeup-cpu */
static pointer PRIM_WAKEUPCPU()
{
    pointer procp;

    if (!iscpu(car(Args)))
        Error("%wakeup-cpu% --- last argument is not CPU", NIL);
    if (Current->cpu_id == 1)
        Error("%wakeup-cpu% --- Slave can not wake up the other CPU", NIL);

    if (Current->cpu_id == 0)
      if(CurrentCell != NULL) {
/*	if(checkscheduleflg) {
	  checkscheduleflg = 0; */
	  for(PreviousCell = NULL, procp = symvalue(PROCQUEUE);
	      procp != NIL && procp != CurrentCell;
	      PreviousCell = procp, procp = cdr(procp));
	  if (procp == NIL)
	    PreviousCell = NULL;
/*	} */
      } else
	PreviousCell = NULL;

    if(PreviousCell == NULL) {
      symvalue(PROCQUEUE) = cons(car(Args), symvalue(PROCQUEUE));
      PreviousCell = symvalue(PROCQUEUE);
    } else {
      cdr(PreviousCell) = cons(car(Args), CurrentCell);
      PreviousCell = cdr(PreviousCell);
    }
    return (symvalue(PROCQUEUE));
}

/* %check-scheduler% */
static pointer PRIM_CHKSCHEDULE()
{
    checkscheduleflg = 1;
    return(VOID);
}

/* peval */
static pointer PRIM_PEVAL()
{
    CPU* cpu;

    if (cdr(Args) != NIL && iscpu(cadr(Args)))
        cpu = cpuregister(cadr(Args));
    else
        Error("last argument is not CPU", NIL);
    cpustatus(cadr(Args)) = SUSPEND;
    cpucont(cadr(Args)) = mk_ph();
    cpu->R_Reg = car(Args);
    cpu->D_Reg = cons(cpu->S_Reg,
                        cons(cpu->E_Reg,
                             cons(cpu->C_Reg, cpu->D_Reg))); /* save registers */
    cpu->S_Reg = cons(cons(cpu->R_Reg, NIL), NIL);
    cpu->C_Reg = cons(mk_operator(OP_AP), 
             cons(mk_operator(OP_EXEC), cons(mk_operator(OP_STOP), NIL)));
    cpu->E_Reg = NIL;
    cpu->R_Reg = symvalue(COMPILE);
    return (cpucont(cadr(Args)));
}
#endif

/* exec */
static pointer PRIM_EXEC()
{
    CPU* cpu;

#ifdef Mulasame
    if (cdr(Args) == NIL)
#endif
        cpu = Current;
#ifdef Mulasame
    else if (iscpu(cadr(Args)))
        cpu = cpuregister(cadr(Args));
    else
        Error("last argument is not CPU", NIL);
#endif    
    cpu->R_Reg = car(Args);
    cpu->C_Reg = cons(mk_operator(OP_EXEC), cons(mk_operator(OP_STOP), cpu->C_Reg));
    SECDR_Cycle(cpu);
    return (cpu->R_Reg);
}

#ifdef Mulasame
/* gencpu */
static pointer PRIM_GENCPU()
{
    pointer newcpu=mk_cpu();
    if (Args != NIL) {
        cpuregister(newcpu)->S_Reg=car(car(Args));
        cpuregister(newcpu)->E_Reg=cadr(car(Args));
        cpuregister(newcpu)->C_Reg=caddr(car(Args));
        cpuregister(newcpu)->D_Reg=cadddr(car(Args));
        cpuregister(newcpu)->R_Reg=caddddr(car(Args));
    } else {
        cpuregister(newcpu)->S_Reg=NIL;
        cpuregister(newcpu)->E_Reg=NIL;
        cpuregister(newcpu)->C_Reg=NIL;
        cpuregister(newcpu)->D_Reg=NIL;
        cpuregister(newcpu)->R_Reg=NIL;
    }
    return(Current->R_Reg=newcpu);
}
#endif

#ifdef DEBUG
/* %reuse-cpu% */
static pointer PRIM_REUSECPU()
{
    pointer cpucell, procp;

    if(symvalue(TERMCPU) != NIL) {
      cpucell = car(symvalue(TERMCPU));
      symvalue(TERMCPU) = cdr(symvalue(TERMCPU));
    } else {
      cpucell = mk_ph();
      if((procp = symvalue(RESWAITQUEUE)) == NIL)
	symvalue(RESWAITQUEUE) = cons(cpucell, NIL);
      else {
	for(; cdr(procp) != NIL; procp = cdr(procp));
	cdr(procp) = cons(cpucell, NIL);
      }
    }
    return (cpucell);
}
#endif

/* gensym */
static pointer PRIM_GENSYM()
{
    if (Args != NIL) {
        if (!isstring(car(Args))) 
            Error("gensym -- first argument must be string", NIL);
        return (gensym(stringvalue(car(Args))));
    } else return(gensym(NULL));
}

#ifdef Mulasame
/* gensemaphore */
static pointer PRIM_GENSEMAPHORE()
{
    long  init;
    if (Args == NIL)
      init = 1;
    else if (isinteger(car(Args)))
      init = ivalue(car(Args));
    else
      Error("gensemaphore -- first argument must be integer", NIL);
    return (mk_semaphore(init));
}
#endif

/* list-width */
static pointer PRIM_LISTWIDTH()
{ return(mk_integer(list_width(car(Args)))); }

#ifdef USE_SYSTEM
/* system */
static pointer PRIM_SYSTEM()
{
    int rc;

    if (Args != NIL && !isstring(car(Args)))
        Error("system -- argument must be string", NIL);
    return(mk_integer((Args != NIL) ?
           system(stringvalue(car(Args))) : system("")));
}

/* getenv */
static pointer PRIM_GETENV()
{
  Char* sp;
    if (!isstring(car(Args)))
        Error("getenv -- argument must be string", NIL);
    else {
      if((sp = (Char*)getenv(stringvalue(car(Args)))) != 0)
        return(mk_string(sp));
      return(F);
    }
}

/* chdir */
static pointer PRIM_CHDIR()
{
  Char* sp;
    if (!isstring(car(Args)))
        Error("chdir -- argument must be string", NIL);
    else {
      if(chdir(stringvalue(car(Args))))
        return(F);
      return(T);
    }
}

/* pwd */
static pointer PRIM_PWD()
{
    getcwd(strbuff, STRBUFFSIZE);
    return(mk_string(strbuff));
}
#endif

#ifdef RUNTIME

/* get-internal-run-time */
static pointer PRIM_GIRT()
{
    return(mk_integer(internal_runtime()));
}

/* get-internal-time-units */
static pointer PRIM_GITU()
{
    return(mk_integer(CLOCKS_PER_SEC));
}

#endif

/* box */
static pointer PRIM_MKBOX()
{  return (mk_box(car(Args))); }

/* box? */
static pointer PRIM_BOXP()
{  return (isbox(car(Args)) ? T : F); }

/* unbox */
static pointer PRIM_UNBOX()
{
    if (!isbox(car(Args)))
        Error("unbox -- argument must be box", NIL);
    return (boxcontext(car(Args)));
}

/* set-box! */
static pointer PRIM_SETBOX()
{
    if (!isbox(car(Args)))
        Error("set-box! -- first argument must be box", NIL);
    boxcontext(car(Args)) = cadr(Args);
    return (cadr(Args));
}
    
/* inexact->exact */
static pointer PRIM_INEX2EX()
{
  if(isreal(car(Args)))
    return (mk_integer((long)round(rvalue(car(Args)))));
  else if (isinteger(car(Args)))
    return (mk_integer(ivalue(car(Args))));
  else if (isrational(car(Args)))
    return (mk_rational(qnvalue(car(Args)), qdvalue(car(Args))));
  else
    Error("inexact->exact -- argument must be number", NIL);
}

/* software-type */
static pointer PRIM_SOFTTYPE()
{
    return (mk_symbol(SOFTTYPE));
}

/* process-type */
static pointer PRIM_PROCESSTYPE()
{
#ifdef Mulasame
    return (mk_symbol("mulasame"));
#else
    return (mk_symbol("secdrscm"));
#endif
}

/* %master->list% */
static pointer PRIM_MREG2LST()
{
  return(cons(Master.S_Reg,
              cons(Master.E_Reg,
                   cons(Master.C_Reg,
                        cons(Master.D_Reg,
                             cons(Master.R_Reg,
                                  NIL))))));
}

#ifdef Mulasame
/* %cpu->list% */
static pointer PRIM_CPU2LST()
{
  CPU* cpu;
  
  if (iscpu(car(Args)))
    cpu = cpuregister(car(Args));
  else
    Error("last argument is not CPU", NIL);
  return(cons(cpu->S_Reg,
              cons(cpu->E_Reg,
                   cons(cpu->C_Reg,
                        cons(cpu->D_Reg,
                             cons(cpu->R_Reg,
                                  NIL))))));
}
#endif

/* %list->master% */
static pointer PRIM_LST2MREG()
{
  Master.S_Reg=car(car(Args));
  Master.E_Reg=cadr(car(Args));
  Master.C_Reg=caddr(car(Args));
  Master.D_Reg=cadddr(car(Args));
  return(Master.R_Reg=caddddr(car(Args)));
}

/* %reset-master% */
static pointer PRIM_RSETMAS()
{
  if(Current != &Master) {
    Master.S_Reg=NIL;
    Master.E_Reg=NIL;
    Master.C_Reg=NIL;
    Master.D_Reg=NIL;
    Master.R_Reg=NIL;
    return(T);
  } else
    return(F);
}

/* + */
static pointer PRIM_ADD()
{ return(numerical_operator(NUMOP_ADD)); }

/* - */
static pointer PRIM_SUB()
{ return(numerical_operator(NUMOP_SUB)); }

/* * */
static pointer PRIM_MUL()
{ return(numerical_operator(NUMOP_MUL)); }

/* / */
static pointer PRIM_DIV()
{ return(numerical_operator(NUMOP_DIV)); }

/* remainder */
static pointer PRIM_REM()
{ return(numerical_operator(NUMOP_REM)); }

/* quotient */
static pointer PRIM_QUO()
{ return(numerical_operator(NUMOP_QUO)); }

/* = */
static pointer PRIM_NEQ()
{ return(numerical_operator(NUMOP_NEQ)); }

/* < */
static pointer PRIM_LESS()
{ return(numerical_operator(NUMOP_LESS)); }

/* > */
static pointer PRIM_GRE()
{ return(numerical_operator(NUMOP_GRE)); }

/* <= */
static pointer PRIM_LEQ()
{ return(numerical_operator(NUMOP_LEQ)); }

/* >= */
static pointer PRIM_GEQ()
{ return(numerical_operator(NUMOP_GEQ)); }

/* expt */
static pointer PRIM_EXPT()
{ return(numerical_operator(NUMOP_EXPT)); }

/* zero? */
static pointer PRIM_ZEROP()
{ return(numerical_operator(NUMOP_ZEROP)); }

/* positive? */
static pointer PRIM_POSP()
{ return(numerical_operator(NUMOP_POSP)); }

/* negative? */
static pointer PRIM_NEGP()
{ return(numerical_operator(NUMOP_NEGP)); }

/* exp */
static pointer PRIM_EXP()
{ return(numerical_operator(NUMOP_EXP)); }

/* log */
static pointer PRIM_LOG()
{ return(numerical_operator(NUMOP_LOG)); }

/* sin */
static pointer PRIM_SIN()
{ return(numerical_operator(NUMOP_SIN)); }

/* cos */
static pointer PRIM_COS()
{ return(numerical_operator(NUMOP_COS)); }

/* tan */
static pointer PRIM_TAN()
{ return(numerical_operator(NUMOP_TAN)); }

/* asin */
static pointer PRIM_ASIN()
{ return(numerical_operator(NUMOP_ASIN)); }

/* acos */
static pointer PRIM_ACOS()
{ return(numerical_operator(NUMOP_ACOS)); }

/* atan */
static pointer PRIM_ATAN()
{ return(numerical_operator(NUMOP_ATAN)); }

/* sqrt */
static pointer PRIM_SQRT()
{ return(numerical_operator(NUMOP_SQRT)); }

/* floor */
static pointer PRIM_FLOOR()
{ return(numerical_operator(NUMOP_FLOOR)); }

/* 1+ */
static pointer PRIM_ONEPLUS()
{ return(numerical_operator(NUMOP_ONEPLUS)); }

/* -1+ */
static pointer PRIM_ONEMINUS()
{ return(numerical_operator(NUMOP_ONEMINUS)); }

/* modulo */
static pointer PRIM_MOD()
{ return(numerical_operator(NUMOP_MOD)); }

/* abs */
static pointer PRIM_ABS()
{ return(numerical_operator(NUMOP_ABS)); }

/* min */
static pointer PRIM_MIN()
{ return(numerical_operator(NUMOP_MIN)); }

/* max */
static pointer PRIM_MAX()
{ return(numerical_operator(NUMOP_MAX)); }

/* even? */
static pointer PRIM_EVEN()
{ return(numerical_operator(NUMOP_EVEN)); }

/* odd */
static pointer PRIM_ODD()
{ return(numerical_operator(NUMOP_ODD)); }

/* ceiling */
static pointer PRIM_CEIL()
{ return(numerical_operator(NUMOP_CEIL)); }

/* truncate */
static pointer PRIM_TRUN()
{ return(numerical_operator(NUMOP_TRUN)); }

/* round */
static pointer PRIM_ROUND()
{ return(numerical_operator(NUMOP_ROUND)); }

/* random */
static pointer PRIM_RANDOM()
{
    int x;
    pointer p;
    
    if(isinteger(p=car(Args))) {
      if((x=ivalue(p)) <= RAND_MAX)
        return(mk_integer(rand()%x));
      else
        Error("random -- First argument is too large", NIL);
    } else
      Error("random -- First argument is not integer", NIL);    
}

/* 
 * initialization of primitive procedures
 */

#ifdef ASL
struct prim_cell * prim_table;

Char** asl_name_table;

struct prim_cell mk_prim(struct cell *(*func)(), Char* name)
{
    struct prim_cell buffer;
    static int i=0;

    buffer._type=T_PRIMITIVE;
    buffer._flag=(F_ATOM | F_MARK);
    buffer._fun=func;
    strcpy(asl_name_table[i], name);
    buffer._name=asl_name_table[i++];
    return buffer;
}

void init_prim_cell()
{
    int j=0;

    prim_table[j++] = mk_prim(PRIM_CAR, "car");
    prim_table[j++] = mk_prim(PRIM_CDR, "cdr");
    prim_table[j++] = mk_prim(PRIM_CAAR, "caar");
    prim_table[j++] = mk_prim(PRIM_CADR, "cadr");
    prim_table[j++] = mk_prim(PRIM_CDAR, "cdar");
    prim_table[j++] = mk_prim(PRIM_CDDR, "cddr");
    prim_table[j++] = mk_prim(PRIM_CAAAR, "caaar");
    prim_table[j++] = mk_prim(PRIM_CAADR, "caadr");
    prim_table[j++] = mk_prim(PRIM_CADAR, "cadar");
    prim_table[j++] = mk_prim(PRIM_CADDR, "caddr");
    prim_table[j++] = mk_prim(PRIM_CDAAR, "cdaar");
    prim_table[j++] = mk_prim(PRIM_CDADR, "cdadr");
    prim_table[j++] = mk_prim(PRIM_CDDAR, "cddar");
    prim_table[j++] = mk_prim(PRIM_CDDDR, "cdddr");
    prim_table[j++] = mk_prim(PRIM_CAAAAR, "caaaar");
    prim_table[j++] = mk_prim(PRIM_CAAADR, "caaadr");
    prim_table[j++] = mk_prim(PRIM_CAADAR, "caadar");
    prim_table[j++] = mk_prim(PRIM_CAADDR, "caaddr");
    prim_table[j++] = mk_prim(PRIM_CADAAR, "cadaar");
    prim_table[j++] = mk_prim(PRIM_CADADR, "cadadr");
    prim_table[j++] = mk_prim(PRIM_CADDAR, "caddar");
    prim_table[j++] = mk_prim(PRIM_CADDDR, "cadddr");
    prim_table[j++] = mk_prim(PRIM_CDAAAR, "cdaaar");
    prim_table[j++] = mk_prim(PRIM_CDAADR, "cdaadr");
    prim_table[j++] = mk_prim(PRIM_CDADAR, "cdadar");
    prim_table[j++] = mk_prim(PRIM_CDADDR, "cdaddr");
    prim_table[j++] = mk_prim(PRIM_CDDAAR, "cddaar");
    prim_table[j++] = mk_prim(PRIM_CDDADR, "cddadr");
    prim_table[j++] = mk_prim(PRIM_CDDDAR, "cdddar");
    prim_table[j++] = mk_prim(PRIM_CDDDDR, "cddddr");
    prim_table[j++] = mk_prim(PRIM_CONS, "cons");
    prim_table[j++] = mk_prim(PRIM_SETCAR, "set-car!");
    prim_table[j++] = mk_prim(PRIM_SETCDR, "set-cdr!");
    prim_table[j++] = mk_prim(PRIM_ADD, "+");
    prim_table[j++] = mk_prim(PRIM_SUB, "-");
    prim_table[j++] = mk_prim(PRIM_MUL, "*");
    prim_table[j++] = mk_prim(PRIM_DIV, "/");
    prim_table[j++] = mk_prim(PRIM_QUO, "quotient");
    prim_table[j++] = mk_prim(PRIM_REM, "remainder");
    prim_table[j++] = mk_prim(PRIM_ONEPLUS, "1+");
    prim_table[j++] = mk_prim(PRIM_ONEMINUS, "-1+");
    prim_table[j++] = mk_prim(PRIM_EXP, "exp");
    prim_table[j++] = mk_prim(PRIM_LOG, "log");
    prim_table[j++] = mk_prim(PRIM_SIN, "sin");
    prim_table[j++] = mk_prim(PRIM_COS, "cos");
    prim_table[j++] = mk_prim(PRIM_TAN, "tan");
    prim_table[j++] = mk_prim(PRIM_ASIN, "asin");
    prim_table[j++] = mk_prim(PRIM_ACOS, "acos");
    prim_table[j++] = mk_prim(PRIM_ATAN, "atan");
    prim_table[j++] = mk_prim(PRIM_SQRT, "sqrt");
    prim_table[j++] = mk_prim(PRIM_EXPT, "expt");
    prim_table[j++] = mk_prim(PRIM_FLOOR, "floor");
    prim_table[j++] = mk_prim(PRIM_MOD, "modulo");
    prim_table[j++] = mk_prim(PRIM_ABS, "abs");
    prim_table[j++] = mk_prim(PRIM_MIN, "min");
    prim_table[j++] = mk_prim(PRIM_MAX, "max");
    prim_table[j++] = mk_prim(PRIM_EVEN, "even?");
    prim_table[j++] = mk_prim(PRIM_ODD, "odd?");
    prim_table[j++] = mk_prim(PRIM_CEIL, "ceiling");
    prim_table[j++] = mk_prim(PRIM_TRUN, "truncate");
    prim_table[j++] = mk_prim(PRIM_ROUND, "round");
    prim_table[j++] = mk_prim(PRIM_RANDOM, "random");
    prim_table[j++] = mk_prim(PRIM_NOT, "not");
    prim_table[j++] = mk_prim(PRIM_BOOLP,"boolean?");
    prim_table[j++] = mk_prim(PRIM_SYMBOLP, "symbol?");
    prim_table[j++] = mk_prim(PRIM_INTEGERP, "integer?");
    prim_table[j++] = mk_prim(PRIM_RATIONALP, "proper-rational?");
    prim_table[j++] = mk_prim(PRIM_RATNUM, "numerator");
    prim_table[j++] = mk_prim(PRIM_RATDEN, "denominator");
    prim_table[j++] = mk_prim(PRIM_REALP, "proper-real?");
    prim_table[j++] = mk_prim(PRIM_STRINGP, "string?");
    prim_table[j++] = mk_prim(PRIM_PROCP, "primitive-procedure?");
    prim_table[j++] = mk_prim(PRIM_CLOSUREP, "closure?");
    prim_table[j++] = mk_prim(PRIM_CONTP, "continuation?");
    prim_table[j++] = mk_prim(PRIM_PAIRP, "pair?");
    prim_table[j++] = mk_prim(PRIM_ATOMP, "atom?");
    prim_table[j++] = mk_prim(PRIM_EQV, "eqv?");
    prim_table[j++] = mk_prim(PRIM_EQ, "eq?");
    prim_table[j++] = mk_prim(PRIM_MEMQ, "memq");
    prim_table[j++] = mk_prim(PRIM_MEMV, "memv");
    prim_table[j++] = mk_prim(PRIM_ASSQ, "assq");
    prim_table[j++] = mk_prim(PRIM_ASSV, "assv");
    prim_table[j++] = mk_prim(PRIM_NULLP, "null?");
    prim_table[j++] = mk_prim(PRIM_ZEROP, "zero?");
    prim_table[j++] = mk_prim(PRIM_POSP, "positive?");
    prim_table[j++] = mk_prim(PRIM_NEGP, "negative?");
    prim_table[j++] = mk_prim(PRIM_NEQ, "=");
    prim_table[j++] = mk_prim(PRIM_LESS, "<");
    prim_table[j++] = mk_prim(PRIM_GRE, ">");
    prim_table[j++] = mk_prim(PRIM_LEQ, "<=");
    prim_table[j++] = mk_prim(PRIM_GEQ, ">=");
    prim_table[j++] = mk_prim(PRIM_LIST, "list");
    prim_table[j++] = mk_prim(PRIM_LISTP, "list?");
    prim_table[j++] = mk_prim(PRIM_LISTTAIL, "list-tail");
    prim_table[j++] = mk_prim(PRIM_LISTREF,  "list-ref");
    prim_table[j++] = mk_prim(PRIM_LENGTH, "length");
    prim_table[j++] = mk_prim(PRIM_REVERSE, "reverse");
    prim_table[j++] = mk_prim(PRIM_APPEND, "append");
    prim_table[j++] = mk_prim(PRIM_PUT, "put");
    prim_table[j++] = mk_prim(PRIM_GET, "get");
    prim_table[j++] = mk_prim(PRIM_ERROR, "error");
    prim_table[j++] = mk_prim(PRIM_CONT, "%current-dump%");
    prim_table[j++] = mk_prim(PRIM_INPUTP, "input-port?");
    prim_table[j++] = mk_prim(PRIM_OUTPUTP, "output-port?");
    prim_table[j++] = mk_prim(PRIM_CURINPUT, "current-input-port");
    prim_table[j++] = mk_prim(PRIM_CUROUTPUT, "current-output-port");
    prim_table[j++] = mk_prim(PRIM_OPENINPUT, "open-input-file");
    prim_table[j++] = mk_prim(PRIM_OPENOUTPUT, "open-output-file");
    prim_table[j++] = mk_prim(PRIM_CLOSEPORT, "close-input-port");
    prim_table[j++] = mk_prim(PRIM_CLOSEPORT, "close-output-port");
    prim_table[j++] = mk_prim(PRIM_EOFOBJP, "eof-object?");
    prim_table[j++] = mk_prim(PRIM_READ, "read");
    prim_table[j++] = mk_prim(PRIM_WRITE, "write");
    prim_table[j++] = mk_prim(PRIM_DISP, "display");
    prim_table[j++] = mk_prim(PRIM_PRINTLIST, "print-list");
    prim_table[j++] = mk_prim(PRIM_VECTORP, "vector?");
    prim_table[j++] = mk_prim(PRIM_MKVECTOR, "make-vector");
    prim_table[j++] = mk_prim(PRIM_VECLEN, "vector-length");
    prim_table[j++] = mk_prim(PRIM_VECREF, "vector-ref");
    prim_table[j++] = mk_prim(PRIM_VECSET, "vector-set!");
    prim_table[j++] = mk_prim(PRIM_VEC2LIST, "vector->list");
    prim_table[j++] = mk_prim(PRIM_LIST2VEC, "list->vector");
    prim_table[j++] = mk_prim(PRIM_STRLEN, "string-length");
    prim_table[j++] = mk_prim(PRIM_MKSTR, "make-string");
    prim_table[j++] = mk_prim(PRIM_STRREF, "string-ref");
    prim_table[j++] = mk_prim(PRIM_STRSET, "string-set!");
    prim_table[j++] = mk_prim(PRIM_SUBSTR, "substring");
    prim_table[j++] = mk_prim(PRIM_STRAPPEND, "string-append");
    prim_table[j++] = mk_prim(PRIM_STREQ, "string=?");
    prim_table[j++] = mk_prim(PRIM_STRLESS, "string<?");
    prim_table[j++] = mk_prim(PRIM_STRGRE, "string>?");
    prim_table[j++] = mk_prim(PRIM_STRLEQ, "string<=?");
    prim_table[j++] = mk_prim(PRIM_STRGEQ, "string>=?");
    prim_table[j++] = mk_prim(PRIM_STRCIEQ, "string-ci=?");
    prim_table[j++] = mk_prim(PRIM_STRCILESS, "string-ci<?");
    prim_table[j++] = mk_prim(PRIM_STRCIGRE, "string-ci>?");
    prim_table[j++] = mk_prim(PRIM_STRCILEQ, "string-ci<=?");
    prim_table[j++] = mk_prim(PRIM_STRCIGEQ, "string-ci>=?");
    prim_table[j++] = mk_prim(PRIM_SYM2STR, "symbol->string");
    prim_table[j++] = mk_prim(PRIM_STR2SYM, "string->symbol");
    prim_table[j++] = mk_prim(PRIM_NUM2STR, "number->string");
    prim_table[j++] = mk_prim(PRIM_STR2NUM, "string->number");
    prim_table[j++] = mk_prim(PRIM_CHARP, "char?");
    prim_table[j++] = mk_prim(PRIM_CHAR2INT, "char->integer");
    prim_table[j++] = mk_prim(PRIM_INT2CHAR, "integer->char");
    prim_table[j++] = mk_prim(PRIM_CHAREQ,  "char=?");
    prim_table[j++] = mk_prim(PRIM_CHARLESS, "char<?");
    prim_table[j++] = mk_prim(PRIM_CHARGRE,  "char>?");
    prim_table[j++] = mk_prim(PRIM_CHARLEQ,  "char<=?");
    prim_table[j++] = mk_prim(PRIM_CHARGEQ,  "char>=?");
    prim_table[j++] = mk_prim(PRIM_CHARCIEQ,  "char-ci=?");
    prim_table[j++] = mk_prim(PRIM_CHARCILESS, "char-ci<?");
    prim_table[j++] = mk_prim(PRIM_CHARCIGRE,  "char-ci>?");
    prim_table[j++] = mk_prim(PRIM_CHARCILEQ,  "char-ci<=?");
    prim_table[j++] = mk_prim(PRIM_CHARCIGEQ,  "char-ci>=?");
    prim_table[j++] = mk_prim(PRIM_CHARALPHA, "char-alphabetic?");
    prim_table[j++] = mk_prim(PRIM_CHARNUMP, "char-numeric?");
    prim_table[j++] = mk_prim(PRIM_CHARWHITE, "char-whitespace?");
    prim_table[j++] = mk_prim(PRIM_CHARUPPER, "char-upper-case?");
    prim_table[j++] = mk_prim(PRIM_CHARLOWER, "char-lower-case?");
    prim_table[j++] = mk_prim(PRIM_CHARUP, "char-upcase");
    prim_table[j++] = mk_prim(PRIM_CHARDOWN, "char-downcase");
    prim_table[j++] = mk_prim(PRIM_READCHAR, "read-char");
    prim_table[j++] = mk_prim(PRIM_PEEKCHAR, "peek-char");
    prim_table[j++] = mk_prim(PRIM_WRITECHAR, "write-char");
    prim_table[j++] = mk_prim(PRIM_CHARREADY, "char-ready?");
    prim_table[j++] = mk_prim(PRIM_NEWSEGMENT, "new-segment");
    prim_table[j++] = mk_prim(PRIM_SETPROMISE, "set-promise");
    prim_table[j++] = mk_prim(PRIM_PROMISEP, "promise?");
    prim_table[j++] = mk_prim(PRIM_GC, "gc");
    prim_table[j++] = mk_prim(PRIM_VERBOSEON, "gc-verbose-on");
    prim_table[j++] = mk_prim(PRIM_VERBOSEOFF, "gc-verbose-off");
    prim_table[j++] = mk_prim(PRIM_QUIT, "quit");
    prim_table[j++] = mk_prim(PRIM_FILEEXISTSP, "file-exists?");
    prim_table[j++] = mk_prim(PRIM_BLOAD, "bin-load");
    prim_table[j++] = mk_prim(PRIM_LOAD, "load");
    prim_table[j++] = mk_prim(PRIM_ELOAD, "ex-load");
    prim_table[j++] = mk_prim(PRIM_EVAL, "eval");
    prim_table[j++] = mk_prim(PRIM_EXEC, "exec");
    prim_table[j++] = mk_prim(PRIM_GENSYM, "gensym");
    prim_table[j++] = mk_prim(PRIM_FORMAT, "%format%");
    prim_table[j++] = mk_prim(PRIM_LISTWIDTH, "list-width");
    prim_table[j++] = mk_prim(PRIM_SPACES, "spaces");
#ifdef USE_SYSTEM
    prim_table[j++] = mk_prim(PRIM_SYSTEM, "system");
    prim_table[j++] = mk_prim(PRIM_GETENV, "getenv");
    prim_table[j++] = mk_prim(PRIM_CHDIR, "chdir");
    prim_table[j++] = mk_prim(PRIM_PWD, "pwd");
#endif
#ifdef RUNTIME
    prim_table[j++] = mk_prim(PRIM_GIRT, "get-internal-run-time");
    prim_table[j++] = mk_prim(PRIM_GITU, "get-internal-time-units");
#endif
    prim_table[j++] = mk_prim(PRIM_BOXP, "box?");
    prim_table[j++] = mk_prim(PRIM_MKBOX, "box");
    prim_table[j++] = mk_prim(PRIM_UNBOX, "unbox");
    prim_table[j++] = mk_prim(PRIM_SETBOX, "set-box!");
    prim_table[j++] = mk_prim(PRIM_INEX2EX, "inexact->exact");
    prim_table[j++] = mk_prim(PRIM_SOFTTYPE, "software-type");
    prim_table[j++] = mk_prim(PRIM_PROCESSTYPE, "process-type");
    prim_table[j++] = mk_prim(PRIM_MREG2LST, "%master->list%");
    prim_table[j++] = mk_prim(PRIM_LST2MREG, "%list->master%");
    prim_table[j++] = mk_prim(PRIM_RSETMAS, "%reset-master%");
    prim_table[j++] = mk_prim(NULL, NULL);
}

#else

#define mk_prim(func, name) \
    { T_PRIMITIVE, (F_ATOM | F_MARK), func, (Char*)name }

struct prim_cell prim_table[] = {
    mk_prim(PRIM_CAR, "car"),
    mk_prim(PRIM_CDR, "cdr"),
    mk_prim(PRIM_CAAR, "caar"),
    mk_prim(PRIM_CADR, "cadr"),
    mk_prim(PRIM_CDAR, "cdar"),
    mk_prim(PRIM_CDDR, "cddr"),
    mk_prim(PRIM_CAAAR, "caaar"),
    mk_prim(PRIM_CAADR, "caadr"),
    mk_prim(PRIM_CADAR, "cadar"),
    mk_prim(PRIM_CADDR, "caddr"),
    mk_prim(PRIM_CDAAR, "cdaar"),
    mk_prim(PRIM_CDADR, "cdadr"),
    mk_prim(PRIM_CDDAR, "cddar"),
    mk_prim(PRIM_CDDDR, "cdddr"),
    mk_prim(PRIM_CAAAAR, "caaaar"),
    mk_prim(PRIM_CAAADR, "caaadr"),
    mk_prim(PRIM_CAADAR, "caadar"),
    mk_prim(PRIM_CAADDR, "caaddr"),
    mk_prim(PRIM_CADAAR, "cadaar"),
    mk_prim(PRIM_CADADR, "cadadr"),
    mk_prim(PRIM_CADDAR, "caddar"),
    mk_prim(PRIM_CADDDR, "cadddr"),
    mk_prim(PRIM_CDAAAR, "cdaaar"),
    mk_prim(PRIM_CDAADR, "cdaadr"),
    mk_prim(PRIM_CDADAR, "cdadar"),
    mk_prim(PRIM_CDADDR, "cdaddr"),
    mk_prim(PRIM_CDDAAR, "cddaar"),
    mk_prim(PRIM_CDDADR, "cddadr"),
    mk_prim(PRIM_CDDDAR, "cdddar"),
    mk_prim(PRIM_CDDDDR, "cddddr"),
    mk_prim(PRIM_CONS, "cons"),
    mk_prim(PRIM_SETCAR, "set-car!"),
    mk_prim(PRIM_SETCDR, "set-cdr!"),
    mk_prim(PRIM_ADD, "+"),
    mk_prim(PRIM_SUB, "-"),
    mk_prim(PRIM_MUL, "*"),
    mk_prim(PRIM_DIV, "/"),
    mk_prim(PRIM_QUO, "quotient"),
    mk_prim(PRIM_REM, "remainder"),
    mk_prim(PRIM_ONEPLUS, "1+"),
    mk_prim(PRIM_ONEMINUS, "-1+"),
    mk_prim(PRIM_EXP, "exp"),
    mk_prim(PRIM_LOG, "log"),
    mk_prim(PRIM_SIN, "sin"),
    mk_prim(PRIM_COS, "cos"),
    mk_prim(PRIM_TAN, "tan"),
    mk_prim(PRIM_ASIN, "asin"),
    mk_prim(PRIM_ACOS, "acos"),
    mk_prim(PRIM_ATAN, "atan"),
    mk_prim(PRIM_SQRT, "sqrt"),
    mk_prim(PRIM_EXPT, "expt"),
    mk_prim(PRIM_FLOOR, "floor"),
    mk_prim(PRIM_MOD, "modulo"),
    mk_prim(PRIM_ABS, "abs"),
    mk_prim(PRIM_MIN, "min"),
    mk_prim(PRIM_MAX, "max"),
    mk_prim(PRIM_EVEN, "even?"),
    mk_prim(PRIM_ODD, "odd?"),
    mk_prim(PRIM_CEIL, "ceiling"),
    mk_prim(PRIM_TRUN, "truncate"),
    mk_prim(PRIM_ROUND, "round"),
    mk_prim(PRIM_RANDOM, "random"),
    mk_prim(PRIM_NOT, "not"),
    mk_prim(PRIM_BOOLP,"boolean?"),
    mk_prim(PRIM_SYMBOLP, "symbol?"),
    mk_prim(PRIM_INTEGERP, "integer?"),
    mk_prim(PRIM_RATIONALP, "proper-rational?"),
    mk_prim(PRIM_RATNUM, "numerator"),
    mk_prim(PRIM_RATDEN, "denominator"),
    mk_prim(PRIM_REALP, "proper-real?"),
    mk_prim(PRIM_STRINGP, "string?"),
    mk_prim(PRIM_PROCP, "primitive-procedure?"),
    mk_prim(PRIM_CLOSUREP, "closure?"),
    mk_prim(PRIM_CONTP, "continuation?"),
    mk_prim(PRIM_PAIRP, "pair?"),
    mk_prim(PRIM_ATOMP, "atom?"),
    mk_prim(PRIM_EQV, "eqv?"),
    mk_prim(PRIM_EQ, "eq?"),
    mk_prim(PRIM_MEMQ, "memq"),
    mk_prim(PRIM_MEMV, "memv"),
    mk_prim(PRIM_ASSQ, "assq"),
    mk_prim(PRIM_ASSV, "assv"),
    mk_prim(PRIM_NULLP, "null?"),
    mk_prim(PRIM_ZEROP, "zero?"),
    mk_prim(PRIM_POSP, "positive?"),
    mk_prim(PRIM_NEGP, "negative?"),
    mk_prim(PRIM_NEQ, "="),
    mk_prim(PRIM_LESS, "<"),
    mk_prim(PRIM_GRE, ">"),
    mk_prim(PRIM_LEQ, "<="),
    mk_prim(PRIM_GEQ, ">="),
    mk_prim(PRIM_LIST, "list"),
    mk_prim(PRIM_LISTP, "list?"),
    mk_prim(PRIM_LISTTAIL, "list-tail"),
    mk_prim(PRIM_LISTREF,  "list-ref"),
    mk_prim(PRIM_LENGTH, "length"),
    mk_prim(PRIM_REVERSE, "reverse"),
    mk_prim(PRIM_APPEND, "append"),
    mk_prim(PRIM_PUT, "put"),
    mk_prim(PRIM_GET, "get"),
    mk_prim(PRIM_ERROR, "error"),
    mk_prim(PRIM_CONT, "%current-dump%"),
    mk_prim(PRIM_INPUTP, "input-port?"),
    mk_prim(PRIM_OUTPUTP, "output-port?"),
    mk_prim(PRIM_CURINPUT, "current-input-port"),
    mk_prim(PRIM_CUROUTPUT, "current-output-port"),
    mk_prim(PRIM_OPENINPUT, "open-input-file"),
    mk_prim(PRIM_OPENOUTPUT, "open-output-file"),
    mk_prim(PRIM_CLOSEPORT, "close-input-port"),
    mk_prim(PRIM_CLOSEPORT, "close-output-port"),
    mk_prim(PRIM_EOFOBJP, "eof-object?"),
    mk_prim(PRIM_READ, "read"),
    mk_prim(PRIM_WRITE, "write"),
    mk_prim(PRIM_DISP, "display"),
    mk_prim(PRIM_PRINTLIST, "print-list"),
    mk_prim(PRIM_VECTORP, "vector?"),
    mk_prim(PRIM_MKVECTOR, "make-vector"),
    mk_prim(PRIM_VECLEN, "vector-length"),
    mk_prim(PRIM_VECREF, "vector-ref"),
    mk_prim(PRIM_VECSET, "vector-set!"),
    mk_prim(PRIM_VEC2LIST, "vector->list"),
    mk_prim(PRIM_LIST2VEC, "list->vector"),
    mk_prim(PRIM_STRLEN, "string-length"),
    mk_prim(PRIM_MKSTR, "make-string"),
    mk_prim(PRIM_STRREF, "string-ref"),
    mk_prim(PRIM_STRSET, "string-set!"),
    mk_prim(PRIM_SUBSTR, "substring"),
    mk_prim(PRIM_STRAPPEND, "string-append"),
    mk_prim(PRIM_STREQ, "string=?"),
    mk_prim(PRIM_STRLESS, "string<?"),
    mk_prim(PRIM_STRGRE, "string>?"),
    mk_prim(PRIM_STRLEQ, "string<=?"),
    mk_prim(PRIM_STRGEQ, "string>=?"),
    mk_prim(PRIM_STRCIEQ, "string-ci=?"),
    mk_prim(PRIM_STRCILESS, "string-ci<?"),
    mk_prim(PRIM_STRCIGRE, "string-ci>?"),
    mk_prim(PRIM_STRCILEQ, "string-ci<=?"),
    mk_prim(PRIM_STRCIGEQ, "string-ci>=?"),
    mk_prim(PRIM_SYM2STR, "symbol->string"),
    mk_prim(PRIM_STR2SYM, "string->symbol"),
    mk_prim(PRIM_NUM2STR, "number->string"),
    mk_prim(PRIM_STR2NUM, "string->number"),
    mk_prim(PRIM_CHARP, "char?"),
    mk_prim(PRIM_CHAR2INT, "char->integer"),
    mk_prim(PRIM_INT2CHAR, "integer->char"),
    mk_prim(PRIM_CHAREQ,  "char=?"),
    mk_prim(PRIM_CHARLESS, "char<?"),
    mk_prim(PRIM_CHARGRE,  "char>?"),
    mk_prim(PRIM_CHARLEQ,  "char<=?"),
    mk_prim(PRIM_CHARGEQ,  "char>=?"),
    mk_prim(PRIM_CHARCIEQ,  "char-ci=?"),
    mk_prim(PRIM_CHARCILESS, "char-ci<?"),
    mk_prim(PRIM_CHARCIGRE,  "char-ci>?"),
    mk_prim(PRIM_CHARCILEQ,  "char-ci<=?"),
    mk_prim(PRIM_CHARCIGEQ,  "char-ci>=?"),
    mk_prim(PRIM_CHARALPHA, "char-alphabetic?"),
    mk_prim(PRIM_CHARNUMP, "char-numeric?"),
    mk_prim(PRIM_CHARWHITE, "char-whitespace?"),
    mk_prim(PRIM_CHARUPPER, "char-upper-case?"),
    mk_prim(PRIM_CHARLOWER, "char-lower-case?"),
    mk_prim(PRIM_CHARUP, "char-upcase"),
    mk_prim(PRIM_CHARDOWN, "char-downcase"),
    mk_prim(PRIM_READCHAR, "read-char"),
    mk_prim(PRIM_PEEKCHAR, "peek-char"),
    mk_prim(PRIM_WRITECHAR, "write-char"),
    mk_prim(PRIM_CHARREADY, "char-ready?"),
    mk_prim(PRIM_NEWSEGMENT, "new-segment"),
    mk_prim(PRIM_SETPROMISE, "set-promise"),
    mk_prim(PRIM_PROMISEP, "promise?"),
#ifdef DEBUG
    mk_prim(PRIM_SETFUTURE, "%set-future%"),
    mk_prim(PRIM_UNSETFUTURE, "%unset-future%"),
#endif
#ifdef Mulasame
    mk_prim(PRIM_FUTUREP, "future?"),
    mk_prim(PRIM_MKPLACEHOLDER, "make-place-holder"),
    mk_prim(PRIM_PLACEHOLDERSET, "place-holder-set!"),
    mk_prim(PRIM_PLACEHOLDERP, "place-holder?"),
    mk_prim(PRIM_TOUCH, "%touch%"),
    mk_prim(PRIM_PHOBJECT, "%ph-object%"),
    mk_prim(PRIM_WAIT, "wait"),
    mk_prim(PRIM_SIGNAL, "signal"),
    mk_prim(PRIM_TASKSW, "%task-switch%"),
    mk_prim(PRIM_TASKSWON, "%task-switch-on%"),
    mk_prim(PRIM_TASKSWOFF, "%task-switch-off%"),
    mk_prim(PRIM_TASKSWP, "%task-switch?%"),
    mk_prim(PRIM_CPUSTEP, "%cpu-step%"),
    mk_prim(PRIM_CPUSTEPSET, "%cpu-step-set!%"),
    mk_prim(PRIM_CPUSTATUS, "%cpu-status%"),
#endif
    mk_prim(PRIM_GC, "gc"),
    mk_prim(PRIM_VERBOSEON, "gc-verbose-on"),
    mk_prim(PRIM_VERBOSEOFF, "gc-verbose-off"),
    mk_prim(PRIM_QUIT, "quit"),
    mk_prim(PRIM_FILEEXISTSP, "file-exists?"),
#ifdef THINK_C
    mk_prim(PRIM_TRANSON, "transcript-on"),
    mk_prim(PRIM_CGOTOXY, "cgotoxy"),
#ifdef Mulasame
    mk_prim(PRIM_SUBTRANSON, "sub-transcript-on"),
    mk_prim(PRIM_SUBCGOTOXY, "sub-cgotoxy"),
#endif
#endif
    mk_prim(PRIM_BLOAD, "bin-load"),
    mk_prim(PRIM_LOAD, "load"),
    mk_prim(PRIM_ELOAD, "ex-load"),
#ifdef DEBUG
    mk_prim(PRIM_FORCE, "%force%"),
#endif
#ifdef Mulasame
    mk_prim(PRIM_FUTURE, "migrate"),
    mk_prim(PRIM_WAKEUPCPU, "wakeup-cpu"),
    mk_prim(PRIM_CHKSCHEDULE, "%check-scheduler%"),
#endif
    mk_prim(PRIM_EVAL, "eval"),
#ifdef Mulasame
    mk_prim(PRIM_PEVAL, "peval"),
#endif
    mk_prim(PRIM_EXEC, "exec"),
#ifdef Mulasame
    mk_prim(PRIM_GENSEMAPHORE, "gensemaphore"),
    mk_prim(PRIM_GENCPU, "gencpu"),
#endif
#ifdef DEBUG
    mk_prim(PRIM_REUSECPU, "%reuse-cpu%"),
#endif
    mk_prim(PRIM_GENSYM, "gensym"),
    mk_prim(PRIM_FORMAT, "%format%"),
    mk_prim(PRIM_LISTWIDTH, "list-width"),
    mk_prim(PRIM_SPACES, "spaces"),
#ifdef USE_SYSTEM
    mk_prim(PRIM_SYSTEM, "system"),
    mk_prim(PRIM_GETENV, "getenv"),
    mk_prim(PRIM_CHDIR, "chdir"),
    mk_prim(PRIM_PWD, "pwd"),
#endif
#ifdef RUNTIME
    mk_prim(PRIM_GIRT, "get-internal-run-time"),
    mk_prim(PRIM_GITU, "get-internal-time-units"),
#endif
    mk_prim(PRIM_BOXP, "box?"),
    mk_prim(PRIM_MKBOX, "box"),
    mk_prim(PRIM_UNBOX, "unbox"),
    mk_prim(PRIM_SETBOX, "set-box!"),
    mk_prim(PRIM_INEX2EX, "inexact->exact"),
    mk_prim(PRIM_SOFTTYPE, "software-type"),
    mk_prim(PRIM_PROCESSTYPE, "process-type"),
    mk_prim(PRIM_MREG2LST, "%master->list%"),
#ifdef Mulasame
    mk_prim(PRIM_CPU2LST, "%cpu->list%"),
#endif
#ifdef Turtle
    mk_prim(gr_helpgr, "help-gr"),
    mk_prim(gr_helpturtlegr, "help-turtlegr"),
    mk_prim(gr_grmode, "graphics-mode!"),
    mk_prim(gr_txtmode, "text-mode!"),
    mk_prim(gr_cleargraph, "clear-graphics!"),
    mk_prim(gr_available, "graphics-avail?"),
    mk_prim(gr_maxx, "max-x"),
    mk_prim(gr_maxy, "max-y"),
    mk_prim(gr_maxc, "max-color"),
    mk_prim(gr_whatcolor, "what-color"),
    mk_prim(gr_whatdirection, "what-direction"),
    mk_prim(gr_wherex, "where-x"),
    mk_prim(gr_wherey,  "where-y"),
    mk_prim(gr_gotohome, "goto-home!"),
    mk_prim(gr_gotocenter,  "goto-center!"),
    mk_prim(gr_gotonw, "goto-nw!"),
    mk_prim(gr_gotosw,  "goto-sw!"),
    mk_prim(gr_gotone, "goto-ne!"),
    mk_prim(gr_gotose, "goto-se!"),
    mk_prim(gr_draw, "draw"),
    mk_prim(gr_move, "move"),
    mk_prim(gr_setcolor, "set-color!"),
    mk_prim(gr_turnright, "turn-right"),
    mk_prim(gr_turnleft, "turn-left"),
    mk_prim(gr_turnto, "turn-to!"),
    mk_prim(gr_getdot, "get-dot"),
    mk_prim(gr_drawTo, "draw-to!"),
    mk_prim(gr_drawto, "draw-to"),
    mk_prim(gr_moveTo, "move-to!"),
    mk_prim(gr_setdot, "set-dot!"),
    mk_prim(gr_validXYC, "valid-xyc?"),
#endif
    mk_prim(PRIM_LST2MREG, "%list->master%"),
    mk_prim(PRIM_RSETMAS, "%reset-master%"),
    mk_prim(NULL, NULL),
};
#endif

/* initialize primitive procedures */
void init_primitives()
{
    struct prim_cell *pr = prim_table;

    while (primfun(pr) != NULL && primname(pr) != NULL) {
        symvalue(mk_symbol(primname(pr))) = (pointer)pr;
        ++pr;
    }
}

