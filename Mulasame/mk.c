/*
 * mk.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/mk.c,v 0.8 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"

/* get new cons cell */
pointer cons(a,b)
register pointer a;
register pointer b;
{
    register pointer x = get_cell(a, b);

    scmtype(x) = T_PAIR;
    flag(x) = 0;
    car(x) = a;
    cdr(x) = b;
    return (x);
}

/* get integer atom */
pointer mk_integer(num)
register long num;
{
    register pointer x = get_cell(NIL, NIL);

    scmtype(x) = T_INTEGER;
    flag(x) = F_ATOM;
    ivalue(x) = num;
    return (x);
}

/* get rational atom */
pointer mk_rational(n,d)
long n;
long d;
{
    register long x, y, r;
    register pointer a;

    x = (n < 0) ? -n : n;
    y = d;
    for (r = x % y ; r != 0 ; r = x % y) { x = y; y = r; }
    n = n/y; d = d/y;
    if (d == 1) return (mk_integer(n));
    else {
        a = get_cell(NIL, NIL);
        scmtype(a) = T_RATIONAL;
        flag(a) = F_ATOM;
        qnvalue(a) = n;
        qdvalue(a) = d;
        return (a);
    }
}

/* get real atom */
pointer mk_real(r)
double r;
{
    register pointer x = get_cell(NIL, NIL);

    scmtype(x) = T_REAL;
    flag(x) = F_ATOM;
    rvalue(x) = r;
    return (x);
}

#ifdef Mulasame
/* make cpu */
pointer mk_cpu()
{
    static unsigned long id = 2;
    register CPU* p;
    register pointer x;

    p = (CPU *)gc_alloc(sizeof(CPU));
    x = get_cell(NIL, NIL);
    scmtype(x) = T_CPU;
    flag(x) = F_ATOM;
    cpuregister(x) = p;
    cpustatus(x) = TERMINATE;
    cpustep(x) = DEFAULT_CPU_STEP;
    cpuid(x) = id++;
    return (x);
}

/* make ph */
pointer mk_ph()
{
    register pointer x;

    x = get_cell(NIL, NIL);
    scmtype(x) = T_PLACEHOLDER;
    flag(x) = F_ATOM;
    flag(x) |= F_FUTURE;
    phobject(x) = NULL;
    return (x);
}

/* make semaphore */
pointer mk_semaphore(num)
register long num;
{
    static long id = 0;
    register pointer x = get_cell(NIL, NIL);

    scmtype(x) = T_SEMAPHORE;
    flag(x) = F_ATOM;
    semaphorenum(x) = num;
    semaphoreid(x) = id++;
    return (x);
}
#endif

/* allocate string */
pointer alloc_string(len,ini)
register long len;
register Char ini;
{
    register Char *p;
    pointer x;

    p = (Char *)gc_alloc(sizeof(Char)*(len + 1));
    x = get_cell(NIL, NIL);
    scmtype(x) = T_STRING;
    flag(x) = F_ATOM;
    stringvalue(x) = p;
    stringlen(x) = len;
    while (len-- > 0) *p++ = ini;
    *p = '\0';
    return (x);
}

/* get substring */
pointer mk_substring(str,b,e)
Char *str;
long b;
long e;
{
    register Char *p, *q;
    register long len;
    pointer x;
    
    if (0 > b || b > e)
        Error("Illegal range of substring", NIL);
    len = e - b;
    x = alloc_string(len, '\0');
    p = stringvalue(x);
    q = str + b;
    while (len-- > 0) *p++ = *q++;
    return (x);
}

/* get new string */
pointer mk_string(str)
Char *str;
{
    return(mk_substring(str, (long)0, (long)strlen(str)));
}

/* hash function */
int hash(name,len)
register Char *name;
register long len;
{
    register long x = 0;

    while (len-- > 0) x += *name++;
    return ((int)(x % HASHTABLESIZE));
}

/* get new symbol */
pointer _mk_symbol(name,len)
Char *name;
long len;
{
    register pointer x;
    register int hashpos;

    /* fisrt check oblist */
    hashpos = hash(name, len);
    x = oblist[hashpos];
    for ( ; x != NIL ; x = cdr(x))
        if (cmpstr(name,
                   len,
                   stringvalue(symstring(car(x))),
                   stringlen(symstring(car(x)))))
            break;

    if (x != NIL) return (car(x));
    else {
        x = cons(DUMMY, cons(mk_substring(name, (long)0, len), NIL));
        scmtype(x) = T_SYMBOL;
        flag(x) = 0;
        oblist[hashpos] = cons(x, oblist[hashpos]);
        return (x);
    }
}

pointer mk_symbol(name)
Char *name;
{
     return(_mk_symbol(name, (long)strlen(name)));
}

/* make symbol or number atom from string */
pointer mk_atom(q)
register Char *q;
{
    register Char *p;
    Char point = 0;
    long n;
    pointer rp;

    for(p=q; *p != '\0'; p++) *p=ToLower(*p);

    p = q;
    if (*p == 'e' || *p == 'E' || *p == '/') return (mk_symbol(q));
    if (*p == '.' && !isdigit(p[1])) return (mk_symbol(q));
    if (*p == '-' || *p == '+') ++p;
    if (*p == '\0') return (mk_symbol(q));
    while (*p != '\0' && 
           (isdigit(*p)||*p == '.'||*p == 'e'||*p == 'E'||*p == '/')) {
        if (*p == '.') {
            if (point) return (mk_symbol(q));
            else point = 1;
        } else if (*p == 'e'||*p == 'E') {
            ++p;
            if (*p == '-' || *p == '+') ++p;
            if (*p == '\0') return (mk_symbol(q));
            while (*p != '\0' && isdigit(*p)) ++p;
            if (*p != '\0') return(mk_symbol(q));
            else return (mk_real(atof(q)));
        } else if (*p == '/') {
            if (point || p[1] == '\0') return (mk_symbol(q));
            if ((rp = trans_base(q, (unsigned short)10)) != NIL) return(rp);
            else return(mk_symbol(q));
        }
        ++p;
    }
    if (*p != '\0') return(mk_symbol(q));
    else if (point) return (mk_real(atof(q)));
    else if (*(q+1) == '\0' || *(q+2) == '\0' || strlen(q) < maxintstrlen)
      return (mk_integer(atol(q)));
    else return (trans_base(q, (unsigned short)10));
}

pointer trans_base(strarg,base)
Char* strarg;
unsigned short base;
{
    pointer x;
    unsigned short b;
    long ix, rx;
    int rc;
    int rt=0;
    int minusflg;
    Char *sp, *rp, *numstr;
    int exactness=0;
    int radix=0;

    numstr=strarg;
    b=base;
    if (*numstr=='#') {
      switch(*(numstr+1)) {
      case 'b':
      case 'B':
        b=2;
        radix=1;
        break;
      case 'o':
      case 'O':
        b=8;
        radix=1;
        break;
      case 'd':
      case 'D':
        b=10;
        radix=1;
        break;
      case 'x':
      case 'X':
        b=16;
        radix=1;
        break;
      case 'i':
      case 'I':
        rt=INEXACT;
        exactness=1;
        break;
      case 'e':
      case 'E':
        rt=EXACT;
        exactness=1;
        break;
      default:
        return(NIL);
      }
      numstr+=2;
    }
    if(exactness) {
      if (*numstr=='#') {
        switch(*(numstr+1)) {
        case 'b':
        case 'B':
          b=2;
          break;
        case 'o':
        case 'O':
          b=8;
          break;
        case 'd':
        case 'D':
          b=10;
          break;
        case 'x':
        case 'X':
          b=16;
          break;
        default:
          return(NIL);
        }
        numstr+=2;
      }
    }
    if(radix) {
      if (*numstr=='#') {
        switch(*(numstr+1)) {
        case 'i':
        case 'I':
          rt=INEXACT;
          break;
        case 'e':
        case 'E':
          rt=EXACT;
          break;
        default:
          return(NIL);
        }
        numstr+=2;
      }
    }

    switch (numtype(numstr, b)) {
    case NUM_TYPE_INT:
        switch (b) {
        case 2:
          minusflg=0;
          for(sp = numstr; *sp == '+' || *sp == '-' || *sp == '0'; sp++)
            if(*sp == '-') minusflg = 1;
          if(strlen(sp) < 8*sizeof(long)) {
            for(ix=0; *sp=='0' || *sp=='1'; sp++) {
              ix = ix << 1; 
              if(*sp=='1') ix += 1; 
            }
            if(minusflg) ix = -ix;
            rc = (*sp=='\0' ? 1 : 0);
          } else
            rc=0;
          break;
        case 8:
          minusflg=0;
          for(sp = numstr; *sp == '+' || *sp == '-'||
              *sp == '0' && *(sp+1) != '\0'; sp++)
            if(*sp == '-') minusflg = 1;
          if((rc=strlen(sp)) < (sizeof(long) == 4 ? 11 : 5) ||
             rc==11 && 4 == sizeof(long) && *sp<='1'){
            rc = sscanf(sp, "%lo", &ix);
            if(minusflg) ix = -ix;
          } else
            rc=0;
          break;
        case 10:
          minusflg=0;
          for(sp = numstr; *sp == '+' || *sp == '-' ||
              *sp == '0' && *(sp+1) != '\0'; sp++)
            if(*sp == '-') minusflg = 1;
          if((rc=strlen(sp)) < maxintstrlen ||
             rc==maxintstrlen && *maxintstr >= *sp) {
            rc = sscanf(sp, "%ld", &ix);
            if(rc == 1 && ix < 0) rc=10;
            if(minusflg) ix = -ix;
          } else
            rc=10;
          break;
        case 16:
          minusflg=0;
          for(sp = numstr; *sp == '+' || *sp == '-' ||
              *sp == '0' && *(sp+1) != '\0'; sp++)
            if(*sp == '-') minusflg = 1;
          if((rc=strlen(sp)) < 2*sizeof(long) ||
             rc==2*sizeof(long) && *sp<='7') {
            rc = sscanf(sp, "%lx", &ix);
            if(minusflg) ix = -ix;
          } else
            rc=0;
          break;
        }
        if(rc == 1) {
          if(rt==INEXACT)
            x = mk_real((double)ix);
          else
            x = mk_integer(ix);
        } else if(rc == 10 && rt != EXACT)
          x = mk_real(atof(numstr));
        else
          return(NIL);
        break;
    case NUM_TYPE_RAT:
        minusflg=0;
        for(sp = numstr; *sp == '+' || *sp == '-' ||
            *sp == '0' && *(sp+1) != '/'; sp++)
          if(*sp == '-') minusflg = 1;
        rp = (Char*)strchr(sp, '/');
        *rp = '\0';
        if((rc=strlen(sp)) < maxintstrlen ||
           rc==maxintstrlen && *maxintstr >= *sp) {
          rc = sscanf(sp, "%ld", &ix);
          if(rc == 1 && ix < 0) {
            *rp = '/';
            return(NIL);
          }
          if(minusflg) ix = -ix;
        } else {
          *rp = '/';
          return(NIL);
        }
        *rp = '/';
        sp = rp+1;
        if(!isdigit(*sp)) return(NIL);
        if((rc=strlen(sp)) < maxintstrlen ||
           rc==maxintstrlen && *maxintstr >= *sp) {
          rc = sscanf(sp, "%ld", &rx);
          if(rc == 1 && rx < 0) return(NIL);
        } else return(NIL);
        if(rt==INEXACT)
          x = mk_real((double)ix/(double)rx);
        else
          x = mk_rational(ix, rx);
        break;
    case NUM_TYPE_REAL:
        if(rt==EXACT)
          return(NIL);
        else
          x = mk_real(atof(numstr));
        break;
    default:
        return(NIL);
    }
    
    return(x);
}

numtype(q,base)
Char *q;
unsigned short base;
{
    Char *p;
    int IsReal = 0;
    int IsRat  = 0;

    p = q;
    if (*p == '-' || *p == '+') ++p;
    if (!*p) return (NO_NUM_TYPE);
    while (*p && (Is_Digit(*p, base) || *p == '.' || *p == 'e' || '/')) {
        if (*p == '.') {
            if (IsReal || IsRat) return (NO_NUM_TYPE);
            else IsReal = 1;
        } else if (*p == '/') {
            if (IsReal || IsRat || base != 10) return (NO_NUM_TYPE);
            else IsRat = 1;
        } else if (*p == 'e' && base != 16) {
            ++p;
            if (*p == '-' || *p == '+') ++p;
            if (!*p) return (NO_NUM_TYPE);
            while (*p && isdigit(*p)) ++p;
            if (*p || IsRat) return(NO_NUM_TYPE);
            else return (NUM_TYPE_REAL);
        }
        ++p;
    }
    if (*p) return(NO_NUM_TYPE);
    else if (IsReal && !IsRat) return (NUM_TYPE_REAL);
    else if (!IsReal && IsRat) return (NUM_TYPE_RAT);
    else return (NUM_TYPE_INT);
}

/* make constant */
pointer mk_const(name)
Char *name;
{
    Char *body = &name[1];
    int x;
    pointer rp;

    if (*body=='b' || *body=='B' ||
        *body=='o' || *body=='O' ||
        *body=='d' || *body=='D' ||
        *body=='x' || *body=='X' ||
        *body=='e' || *body=='E' ||
	*body=='i' || *body=='I') {
      if((rp=trans_base(&name[0], 10)) != NIL)
        return(rp);
    }
    if (!strcmp(body, "t")) return (T);
    else if (!strcmp(body, "T")) return (T);
    else if (!strcmp(body, "f")) return (F);
    else if (!strcmp(body, "F")) return (F);
    else if (!strcmp(body, "v")) return (VOID);
    else if (!strcmp(body, "eof")) return (eof_obj);
    else if (!strcmp(body, "dummy")) return (DUMMY);
    else if (!strcmp(body, "\\space")) return ((pointer)&char_table[' ']);
    else if (!strcmp(body, "\\Space")) return ((pointer)&char_table[' ']);
    else if (!strcmp(body, "\\newline")) return ((pointer)&char_table['\n']);
    else if (!strcmp(body, "\\Newline")) return ((pointer)&char_table['\n']);
    else if (!strcmp(body, "\\ht")) return ((pointer)&char_table['\t']);
    else if (!strcmp(body, "\\Ht")) return ((pointer)&char_table['\t']);
    else if (!strcmp(body, "\\tab")) return ((pointer)&char_table['\t']);
    else if (!strcmp(body, "\\Tab")) return ((pointer)&char_table['\t']);
    else if (isdigit(*body)) return (mk_operator(atoi(body)));
    else if (*body == '\\') {
        if (*++body == '\0') return ((pointer)&char_table[' ']);
        else if (body[1] == '\0') /* #\<one char> */
            return ((pointer)&char_table[*body]);
        else { /* octal expression like #\012 */
            for (x = 0 ; *body ; body++) {
                if (*body >= '0' && *body <= '7')
                    x = 8*x + (int)*body - (int)'0';
                else return(mk_symbol(name));
            }
            return ((pointer)&char_table[x & CHARMASK]);
        }
    } else return (mk_symbol(name));
}

/* make vector */
pointer mk_vector(len,ini)
register long len;
register pointer ini;
{
    pointer x = get_cell(NIL, NIL);
    register pointer *vec;
    
    if (len > 0) vec = (pointer *)gc_alloc(len*sizeof(pointer));
    else { len = 0; vec = NULL; }

    scmtype(x) = T_VECTOR;
    flag(x) = F_ATOM;
    vectorlen(x) = len;
    vector(x) = vec;
    while (len-- > 0) *vec++ = ini;
    
    return (x);
}
    
/* make input port */
pointer mk_inputport(name,mode)
pointer name;
pointer mode;
{
    FILE *fp;
    pointer x;
  
    if (!isstring(name)) Error("argument must be string", NIL); 
    fp = (mode == NIL) ? fopen(stringvalue(name), "r") : 
                         fopen(stringvalue(name), R_BINMODE); 
    if (fp == NULL)
        Error("Unable to open input file ~S", cons(name, NIL));
    x = get_cell(NIL, NIL);
    scmtype(x) = T_PORT;
    flag(x) = F_ATOM;
    portfile(x) = fp;
    portstatus(x) = (P_OPEN | P_INPUT);
    return (x);
}

/* make output port */
pointer mk_outputport(name,mode)
pointer name;
pointer mode;
{
    FILE *fp;
    pointer x;
    Char* namestr;
  
    if (!isstring(name)) Error("argument must be string", NIL);
    namestr = stringvalue(name);
#ifdef THINK_C
    if( !strncmp(namestr+strlen(namestr)-4, ".bin", 5) ||
        !strncmp(namestr+strlen(namestr)-4, ".BIN", 5) ||
        mode != NIL )
        _fcreator = 'MulS';
#endif
    fp = (mode == NIL) ? fopen(namestr, "w") :
                         fopen(namestr, W_BINMODE); 
#ifdef THINK_C
    _fcreator = DEFAULTCREATOR;
#endif
    if (fp == NULL)
        Error("Unable to open output file ~S", cons(name, NIL));
    x = get_cell(NIL, NIL);
    scmtype(x) = T_PORT;
    flag(x) = F_ATOM;
    portfile(x) = fp;
    portstatus(x) = P_OPEN;
    return (x);
}

/* close port */
pointer closeport(x)
pointer x;
{
    if (isopenport(x)) {
        fclose(portfile(x));
        portfile(x) = NULL;
        portstatus(x) &= CLROPEN;
    }
    return (VOID);
}

/* make closure. c is code. e is environment */
pointer mk_closure(c,e)
register pointer c;
register pointer e;
{
    register pointer x = get_cell(c, e);

    scmtype(x) = T_CLOSURE;
    flag(x) = 0;
    car(x) = c;
    cdr(x) = e;
    return (x);
}

/* make continuation. */
pointer mk_continuation(d)
register pointer d;
{
    register pointer x = get_cell(NIL, d);

    scmtype(x) = T_CONTINUATION;
    flag(x) = 0;
    car(x) = NIL;
    cont_dump(x) = d;
    return (x);
}

/* make box */
pointer mk_box(a)
register pointer a;
{
    register pointer x = get_cell(a, NIL);

    scmtype(x) = T_BOX;
    flag(x) = 0;
    boxcontext(x) = a;
    cdr(x) = NIL;
    return (x);
}

