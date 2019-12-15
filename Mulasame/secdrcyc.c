/*
 * secdrcyc.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /home/argama/s-haya/Scheme/Mulasame/RCS/secdrcyc.c,v 0.10 1994/09/14 08:11:57 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"
#include "operator.h"

int SECDR_Cycle( cpu )
register CPU* cpu;
{
    register pointer x, y;
    /* register */ long i, j;
    CPU* Previous;
#ifdef Macintosh
    int count;
#endif
#ifdef THINK_C
#ifdef Mulasame
    int cif = 0;
    int cof = 0;
#endif
#endif

#ifdef Mulasame
    step = cpu->cpu_step;
#endif
    Previous = Current;
    Current = cpu;
#ifdef THINK_C
#ifdef Mulasame
	if(Current != &Master && Current != &Slave) {
		if(cur_inport == stdin_port) {
			cur_inport = substdin_port;
			cif = 1;
		}
		if(cur_outport == stdout_port) {
			cur_outport = substdout_port;
			cof = 1;
		}
	}
#endif
#endif

LOOP:

#ifdef Macintosh
if(repflg && Current == &Master && count++ > 100000) {
    MacintoshEvent();
    count=0;
}
#endif

#ifdef Mulasame
    if(cpu->cpu_status == WAIT) {
      if(cpu->cpu_id == 0) {
	if(switchflg == 0)
            Error("Deadlock detected!\nCurrent task status has become 'WAIT' in disabled task-switching mode!", NIL);
	for(scheduler(); flag(cpu->R_Reg)&F_FUTURE; scheduler());
	cpu->cpu_status = SUSPEND;
	step = cpu->cpu_step;
      } else if(cpu->cpu_id == 1) {
	cpu->cpu_status = SUSPEND;
      } else if(cpu->cpu_id > 1) {
#ifdef THINK_C
		if(cif)
			cur_inport = stdin_port;
		if(cof)
			cur_outport = stdout_port;
#endif
        Current = Previous;
        return(WAIT);
      }
    }

    if(flag(cpu->R_Reg)&F_WAIT) {
      flag(cpu->R_Reg) &= (~F_WAIT);
      if(cpu->cpu_id == 0) {
	cpu->cpu_status = SEMWAIT;
	if(switchflg == 0)
            Error("Deadlock detected!\nCurrent task status has become 'SEMWAIT' in disabled task-switching mode!", NIL);
	for(scheduler(); cpu->cpu_status != SUSPEND; scheduler());
      } else if(cpu->cpu_id > 1) {
#ifdef THINK_C
		if(cif)
			cur_inport = stdin_port;
		if(cof)
			cur_outport = stdout_port;
#endif
        Current = Previous;
        return(SEMWAIT);
      }
    }

    if(flag(cpu->R_Reg)&F_SIGNAL) {
      flag(cpu->R_Reg) &= (~F_SIGNAL);
      wakeup(cpu->R_Reg);
    }

    if (cpu->C_Reg == NIL) {
    /* Stop SECDR execution */
      if(Current->cpu_id > 1) {
	flag(Current->cont) &= (~F_FUTURE);
	phobject(Current->cont) = cpu->R_Reg;
      }
#ifdef THINK_C
		if(cif)
			cur_inport = stdin_port;
		if(cof)
			cur_outport = stdout_port;
#endif
      Current = Previous;
      return(TERMINATE);
    }

    if(switchflg && (--step < 0)) {
      if(cpu->cpu_id == 0) {
	scheduler();
	step = cpu->cpu_step;
      } else if(cpu->cpu_id > 1) {
#ifdef THINK_C
		if(cif)
			cur_inport = stdin_port;
		if(cof)
			cur_outport = stdout_port;
#endif
        Current = Previous;
        return(SUSPEND);
      }
    }
#endif

    switch (opnum(car(cpu->C_Reg))) {
    case OP_LD:
    /*  
     *  S E (LD (i . j) . C) D R ==> S E C D r
     *  where r = (list-ref (list-ref E i) j)
     */
#ifdef ORIGINAL
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        cpu->R_Reg = car(x);
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        i = ivalue(car(x=car(y=cdr(cpu->C_Reg))));
        j = ivalue(cdr(x));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        cpu->R_Reg = car(x);
        cpu->C_Reg = cdr(y);
#endif
        goto LOOP;

    case OP_TLD:
    /*  
     *  S E (TLD (i . j) . C) D R ==> S E C D r
     *  where r = (list-tail (list-ref E i) j)
     */
#ifdef ORIGINAL
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        cpu->R_Reg =  x;
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        i = ivalue(car(x=car(y=cdr(cpu->C_Reg))));
        j = ivalue(cdr(x));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        cpu->R_Reg =  x;
        cpu->C_Reg = cdr(y);
#endif
        goto LOOP;

    case OP_GLD:
    /*
     * S E (GLD sym . C) D R ==> S E C D r
     * where x = gloabl value of sym
     */
#ifdef ORIGINAL
        if ((cpu->R_Reg = symvalue(cadr(cpu->C_Reg))) == DUMMY)
            Error("Unbouned variable ~S", cons(cadr(cpu->C_Reg), NIL)); 
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        if ((cpu->R_Reg = symvalue(car(x=cdr(cpu->C_Reg)))) == DUMMY)
            Error("Unbouned variable ~S", cons(cadr(cpu->C_Reg), NIL)); 
        cpu->C_Reg = cdr(x);
#endif
        goto LOOP;

    case OP_LDC:
    /*
     * S E (LDC const . C) D R ==> S E C D const
     */
#ifdef ORIGINAL
        cpu->R_Reg = cadr(cpu->C_Reg);
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        cpu->R_Reg = car(x=cdr(cpu->C_Reg));
        cpu->C_Reg = cdr(x);
#endif
        goto LOOP;

    case OP_LDF:
    /*
     * S E (LDF code . C) D R ==> S E C D (closure of code and E)
     */
#ifdef ORIGINAL
        cpu->R_Reg = mk_closure(cadr(cpu->C_Reg), cpu->E_Reg);
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        cpu->R_Reg = mk_closure(car(x=cdr(cpu->C_Reg)), cpu->E_Reg);
        cpu->C_Reg = cdr(x);
#endif
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
#ifdef ORIGINAL
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
#else
        if (isclosure(y=cpu->R_Reg)) {
            cpu->D_Reg = cons(cdr(x=cpu->S_Reg), cons(cpu->E_Reg,
                              cons(cdr(cpu->C_Reg), cpu->D_Reg)));
            cpu->E_Reg = cons(car(x), closure_env(y));
            cpu->C_Reg = closure_code(y);
            cpu->S_Reg = cpu->R_Reg = NIL;
            goto LOOP;
        } else if (isprimitive(y)) {
            cpu->C_Reg = cdr(cpu->C_Reg);
            Args = car(x=cpu->S_Reg);
            cpu->R_Reg = (*(primfun(y)))();
            cpu->S_Reg = cdr(x);
            goto LOOP;
        } else if (iscontinuation(y)) {
            x = cont_dump(y);
            cpu->R_Reg = caar(cpu->S_Reg);
            cpu->S_Reg = car(x);
            cpu->E_Reg = car(x=cdr(x));
            cpu->C_Reg = car(x=cdr(x));
            cpu->D_Reg = cdr(x);
            goto LOOP;
        } else Error("Illegal function", NIL);
#endif

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
#ifdef ORIGINAL
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
#else
        if (isclosure(y=cpu->R_Reg)) {
            cpu->E_Reg = cons(car(x=cpu->S_Reg), closure_env(y));
            cpu->C_Reg = closure_code(y);
            cpu->S_Reg = cpu->R_Reg = NIL;
            goto LOOP;
        } else if (isprimitive(y)) {
            cpu->C_Reg = cdr(cpu->C_Reg);
            Args = car(x=cpu->S_Reg);
            cpu->R_Reg = (*(primfun(y)))();
            cpu->S_Reg = cdr(x);
            goto LOOP;
        } else if (iscontinuation(y)) {
            x = cont_dump(y);
            cpu->R_Reg = caar(cpu->S_Reg);
            cpu->S_Reg = car(x);
            cpu->E_Reg = car(x=cdr(x));
            cpu->C_Reg = car(x=cdr(x));
            cpu->D_Reg = cdr(x);
            goto LOOP;
        } else Error("Illegal function", NIL);
#endif

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
#ifdef ORIGINAL
        cpu->S_Reg = car(cpu->D_Reg);
        cpu->E_Reg = cadr(cpu->D_Reg);
        cpu->C_Reg = caddr(cpu->D_Reg);
        cpu->D_Reg = cdddr(cpu->D_Reg);
#else
        cpu->S_Reg = car(x=cpu->D_Reg);
        cpu->E_Reg = car(x=cdr(x));
        cpu->C_Reg = car(x=cdr(x));
        cpu->D_Reg = cdr(x);
#endif
        goto LOOP;

    case OP_SEL:
    /*
     * S E (SEL ct cf . C) D test ==> S E cx (S E C . D) test
     * where cx = (if test ct cf)
     */
#ifdef ORIGINAL
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cdddr(cpu->C_Reg), cpu->D_Reg)));
        if (istrue(cpu->R_Reg)) cpu->C_Reg = cadr(cpu->C_Reg);
        else cpu->C_Reg = caddr(cpu->C_Reg);
#else
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cdr(y=cdr(x=cdr(cpu->C_Reg))), cpu->D_Reg)));
        if (istrue(cpu->R_Reg)) cpu->C_Reg = car(x);
        else cpu->C_Reg = car(y);
#endif
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
#ifdef ORIGINAL
        i = ivalue(caadr(cpu->C_Reg));
        j = ivalue(cdadr(cpu->C_Reg));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        car(x) = cpu->R_Reg;
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        i = ivalue(car(x=car(y=cdr(cpu->C_Reg))));
        j = ivalue(cdr(x));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        x = car(x);
        while (j-- > 0) x = cdr(x);
        car(x) = cpu->R_Reg;
        cpu->C_Reg = cdr(y);
#endif
        goto LOOP;

    case OP_TASSIG:
    /*  
     *  S E (TASSIG (i . j) . C) D R ==> S E' C D R
     *  where E' is made by
     *     (if (zero? j)
     *       (set-car! (list-tail E i) R)
     *       (set-cdr! (list-tail (list-ref E i) (- j 1)) R))
     */
#ifdef ORIGINAL
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
#else
        i = ivalue(car(x=car(y=cdr(cpu->C_Reg))));
        j = ivalue(cdr(x));
        x = cpu->E_Reg;
        while (i-- > 0) x = cdr(x);
        if (j == 0) {
          car(x) = cpu->R_Reg;
        } else {
          x = car(x);
          while (--j > 0) x = cdr(x);
          cdr(x) = cpu->R_Reg;
        }
        cpu->C_Reg = cdr(y);
#endif
        goto LOOP;

    case OP_GASSIG:
    /*
     * S E (GASSIG sym . C) D R ==> S E C D R
     * where global value of sym = R
     */
#ifdef ORIGINAL
        if (symvalue(cadr(cpu->C_Reg)) == DUMMY)
            Error("Unbounded variable ~S", cons(cadr(cpu->C_Reg), NIL));
        symvalue(cadr(cpu->C_Reg)) = cpu->R_Reg;
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        if (symvalue(x=car(y=cdr(cpu->C_Reg))) == DUMMY)
            Error("Unbounded variable ~S", cons(cadr(cpu->C_Reg), NIL));
        symvalue(x) = cpu->R_Reg;
        cpu->C_Reg = cdr(y);
#endif
        goto LOOP;

    case OP_DEF:
    /*
     * S E (DEF sym . C) D R ==> S E C D sym
     * where global value of sym = R
     */
#ifdef ORIGINAL
        symvalue(cadr(cpu->C_Reg)) = cpu->R_Reg;
        cpu->R_Reg = cadr(cpu->C_Reg);
        cpu->C_Reg = cddr(cpu->C_Reg);
#else
        symvalue(x=car(y=cdr(cpu->C_Reg))) = cpu->R_Reg;
        cpu->R_Reg = x;
        cpu->C_Reg = cdr(y);
#endif
        goto LOOP;

    case OP_PUSHCONS:
    /*
     * (s . S) E (PUSHCONS . C) D R ==> ((R . s) . S) E C D R
     */
#ifdef ORIGINAL
        cpu->S_Reg = cons(cons(cpu->R_Reg, car(cpu->S_Reg)),
                          cdr(cpu->S_Reg));  /* <----- Bug fixed by S.Hayashi
        car(cpu->S_Reg) = cons(cpu->R_Reg, car(cpu->S_Reg));*/
        cpu->C_Reg = cdr(cpu->C_Reg);
#else
        x=cpu->S_Reg;
        cpu->S_Reg = cons(cons(cpu->R_Reg, car(x)), cdr(x));
        cpu->C_Reg = cdr(cpu->C_Reg);
#endif
        goto LOOP;

    case OP_SAVE:
    /*
     * S E (SAVE C1 . C2) D R ==> S E C1 (S E C2 . D) R
     */
#ifdef ORIGINAL
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cddr(cpu->C_Reg), cpu->D_Reg)));
        cpu->C_Reg = cadr(cpu->C_Reg);
#else
        cpu->D_Reg = cons(cpu->S_Reg, cons(cpu->E_Reg,
                          cons(cdr(x=cdr(cpu->C_Reg)), cpu->D_Reg)));
        cpu->C_Reg = car(x);
#endif
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
#ifdef Mulasame
	if(Current->cpu_id > 1) {
	  flag(Current->cont) &= (~F_FUTURE);
	  phobject(Current->cont) = cpu->R_Reg;
	}
#ifdef THINK_C
		if(cif)
			cur_inport = stdin_port;
		if(cof)
			cur_outport = stdout_port;
#endif
#endif
        Current = Previous;
#ifdef Mulasame
        return(TERMINATE);
#else
        return(0);
#endif
    default:
        Error("Illegal operator ~S", cons(car(cpu->C_Reg), NIL));
    }
}
