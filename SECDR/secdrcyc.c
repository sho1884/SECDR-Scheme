/*
 * secdrcyc.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/secdrcyc.c,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
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

int SECDR_Cycle( cpu )
CPU* cpu;
{
    register pointer x, y;
    register long i, j;
    CPU* Previous;

#ifdef Macintosh
    int count;
#endif

    Previous = Current;
    Current = cpu;

LOOP:
#ifdef Macintosh
if(repflg && count++ > 1000) {
    MacintoshEvent();
    count=0;
}
#endif

    switch (opnum(car(cpu->C_Reg))) {
    case OP_LD:
    /*  
     *  S E (LD (i . j) . C) D R ==> S E C D r
     *  where r = (list-ref (list-ref E i) j)
     */
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        cpu->R_Reg = car(x);
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_TLD:
    /*  
     *  S E (TLD (i . j) . C) D R ==> S E C D r
     *  where r = (list-tail (list-ref E i) j)
     */
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        cpu->R_Reg =  x;
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_GLD:
    /*
     * S E (GLD sym . C) D R ==> S E C D r
     * where x = gloabl value of sym
     */
        if ((cpu->R_Reg = symvalue(cadr(cpu->C_Reg))) == DUMMY)
            Error("Unbouned variable ~S", cons(cadr(cpu->C_Reg), NIL)); 
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_LDC:
    /*
     * S E (LDC const . C) D R ==> S E C D const
     */
        cpu->R_Reg = cadr(cpu->C_Reg);
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_LDF:
    /*
     * S E (LDF code . C) D R ==> S E C D (closure of code and E)
     */
        cpu->R_Reg = mk_closure(cadr(cpu->C_Reg), cpu->E_Reg);
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_AP:
    /*
     * (args . S) E (AP . C) D op ==>
     * case: op is closure
     *   () (args . env) code (S E C . D) NIL
     *   where code is closure code of op and
     *   env is closure environment of op.
     * case: op is primivite procedure
     *   First, execute primitive procedure op with arguments args
     *   and set registers to S E C D r, where r is the return
     *   value of primitine procedure op.
     * case: op is continuation
     *   s e c d r
     *   where r is first element of args and
     *   s e c d are saved registers in op.
     */
        if (isclosure(cpu->R_Reg)) {
            cpu->D_Reg = cons(cdr(cpu->S_Reg), cons(cpu->E_Reg,
                              cons(cdr(cpu->C_Reg), cpu->D_Reg)));
            cpu->E_Reg = cons(car(cpu->S_Reg), closure_env(cpu->R_Reg));
            cpu->C_Reg = closure_code(cpu->R_Reg);
            cpu->S_Reg = cpu->R_Reg = NIL;
            goto LOOP;
        } else if (isprimitive(cpu->R_Reg)) {
            cpu->C_Reg = cdr(cpu->C_Reg);
            Args = car(cpu->S_Reg);
            cpu->R_Reg = (*(primfun(cpu->R_Reg)))();
            cpu->S_Reg = cdr(cpu->S_Reg);
            goto LOOP;
        } else if (iscontinuation(cpu->R_Reg)) {
            x = cont_dump(cpu->R_Reg);
            cpu->R_Reg = caar(cpu->S_Reg);
            cpu->S_Reg = car(x);
            cpu->E_Reg = cadr(x);
            cpu->C_Reg = caddr(x);
            cpu->D_Reg = cdddr(x);
            goto LOOP;
        } else Error("Illegal function", NIL);

    case OP_TAP:
    /*
     * (args . S) E (TAP . C) D op ==>
     * case: op is closure
     *   () (args . env) code D NIL
     *   where code is closure code of op and
     *   env is closure environment of op.
     * case: op is primivite procedure
     *   First, execute primitive procedure op with arguments args
     *   and set registers to S E C D r, where r is the return
     *   value of primitine procedure op.
     * case: op is continuation
     *   s e c d r
     *   where r is the first element of args and
     *   s e c d are saved registers in op.
     */
        if (isclosure(cpu->R_Reg)) {
            cpu->E_Reg = cons(car(cpu->S_Reg), closure_env(cpu->R_Reg));
            cpu->C_Reg = closure_code(cpu->R_Reg);
            cpu->S_Reg = cpu->R_Reg = NIL;
            goto LOOP;
        } else if (isprimitive(cpu->R_Reg)) {
            cpu->C_Reg = cdr(cpu->C_Reg);
            Args = car(cpu->S_Reg);
            cpu->R_Reg = (*(primfun(cpu->R_Reg)))();
            cpu->S_Reg = cdr(cpu->S_Reg);
            goto LOOP;
        } else if (iscontinuation(cpu->R_Reg)) {
            x = cont_dump(cpu->R_Reg);
            cpu->R_Reg = caar(cpu->S_Reg);
            cpu->S_Reg = car(x);
            cpu->E_Reg = cadr(x);
            cpu->C_Reg = caddr(x);
            cpu->D_Reg = cdddr(x);
            goto LOOP;
        } else Error("Illegal function", NIL);

    case OP_PUSH:
    /*
     * S E (PUSH . C) D R ==> (R . S) E C D R
     */
        cpu->S_Reg = cons(cpu->R_Reg, cpu->S_Reg);
        cpu->C_Reg = cdr(cpu->C_Reg);
        goto LOOP;

    case OP_RTN:
    /*
     * S E (RTN . C) (s e c . d) R ==> s e c d R
     */
        cpu->S_Reg = car(cpu->D_Reg);
        cpu->E_Reg = cadr(cpu->D_Reg);
        cpu->C_Reg = caddr(cpu->D_Reg);
        cpu->D_Reg = cdddr(cpu->D_Reg);
        goto LOOP;

    case OP_SEL:
    /*
     * S E (SEL ct cf . C) D test ==> S E cx (S E C . D) test
     * where cx = (if test ct cf)
     */
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cdddr(cpu->C_Reg), cpu->D_Reg)));
        if (istrue(cpu->R_Reg)) cpu->C_Reg = cadr(cpu->C_Reg);
        else cpu->C_Reg = caddr(cpu->C_Reg);
        goto LOOP;

    case OP_TSEL:
    /*
     * S E (TSEL ct cf . C) D test ==> S E cx D test
     * where cx = (if test ct cf)
     */
        if (istrue(cpu->R_Reg)) cpu->C_Reg = cadr(cpu->C_Reg);
        else cpu->C_Reg = caddr(cpu->C_Reg);
        goto LOOP;

    case OP_ASSIG:
    /*  
     *  S E (ASSIG (i . j) . C) D R ==> S E' C D R
     *  where E' is made by 
     *       (set-car! (list-tail (list-ref E i) j) R)
     */
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        car(x) = cpu->R_Reg;
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_TASSIG:
    /*  
     *  S E (TASSIG (i . j) . C) D R ==> S E' C D R
     *  where E' is made by
     *     (if (zero? j)
     *       (set-car! (list-tail E i) R)
     *       (set-cdr! (list-tail (list-ref E i) (- j 1)) R))
     */
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        if (j == 0) {
          car(x) = cpu->R_Reg;
        } else {
          x = car(x);
          while (--j > 0) x = cdr(x);
          cdr(x) = cpu->R_Reg;
        }
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_GASSIG:
    /*
     * S E (GASSIG sym . C) D R ==> S E C D R
     * where global value of sym = R
     */
        if (symvalue(cadr(cpu->C_Reg)) == DUMMY)
            Error("Unbounded variable ~S", cons(cadr(cpu->C_Reg), NIL));
        symvalue(cadr(cpu->C_Reg)) = cpu->R_Reg;
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_DEF:
    /*
     * S E (DEF sym . C) D R ==> S E C D sym
     * where global value of sym = R
     */
        symvalue(cadr(cpu->C_Reg)) = cpu->R_Reg;
        cpu->R_Reg = cadr(cpu->C_Reg);
        cpu->C_Reg = cddr(cpu->C_Reg);
        goto LOOP;

    case OP_PUSHCONS:
    /*
     * (s . S) E (PUSHCONS . C) D R ==> ((R . s) . S) E C D R
     */
        cpu->S_Reg = cons(cons(cpu->R_Reg, car(cpu->S_Reg)),
                          cdr(cpu->S_Reg));  /* <----- Bug fixed by S.Hayashi
        car(cpu->S_Reg) = cons(cpu->R_Reg, car(cpu->S_Reg));*/
        cpu->C_Reg = cdr(cpu->C_Reg);
        goto LOOP;

    case OP_SAVE:
    /*
     * S E (SAVE C1 . C2) D R ==> S E C1 (S E C2 . D) R
     */
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cddr(cpu->C_Reg), cpu->D_Reg)));
        cpu->C_Reg = cadr(cpu->C_Reg);
        goto LOOP;
    
    case OP_EXEC:
    /*
     * S E (EXEC . C) D code ==> NIL NIL code (S E C . D) NIL
     */
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cdr(cpu->C_Reg), cpu->D_Reg)));
        cpu->C_Reg = cpu->R_Reg;
        cpu->S_Reg = cpu->E_Reg = cpu->R_Reg = NIL;
        goto LOOP;

    case OP_STOP:
    /* Stop SECDR execution */
        cpu->C_Reg = cdr(cpu->C_Reg);
        Current = Previous;
        return 0;

    default:
        Error("Illegal operator ~S", cons(car(cpu->C_Reg), NIL));
    }
}

