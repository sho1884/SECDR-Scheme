/*
 * schedule.c
 *
 * Coded by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/schedule.c,v 1.4 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Shoichi Hayashi.
*/

/* #define DEBUG */

#include "mulasame.h"

#ifdef DEBUG
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
#endif

int scheduler()
{
  CPU* cpu;
  pointer firstp = 0;
  register pointer previous;
  register pointer procp;
#ifdef DEBUG
  int i;
#endif

  if(PreviousCell==NULL) PreviousCell = NIL;
  if(PreviousCell!=NIL) {
    for(procp = symvalue(PROCQUEUE);
	procp != NIL && procp != PreviousCell;
	procp = cdr(procp));
    PreviousCell = procp;
  }

#ifdef DEBUG
  i = list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
    +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE));
#endif

  for(previous = NULL, procp = symvalue(WAITQUEUE); procp != NIL;) {
    if(iscpu(car(procp)) && cpustatus(car(procp)) == WAIT &&
       !(flag(cpuregister(car(procp))->R_Reg)&F_FUTURE)) {
      cpustatus(car(procp)) = SUSPEND;
      if(!firstp) firstp = procp;
      if(previous == NULL)
	symvalue(WAITQUEUE) = cdr(procp);
      else
	cdr(previous) = cdr(procp);
      if(PreviousCell != NIL) {
	cdr(procp) = cdr(PreviousCell);
	cdr(PreviousCell) = procp;
      } else {
	cdr(procp) = symvalue(PROCQUEUE);
	symvalue(PROCQUEUE) = procp;
      }
      if(previous == NULL)
	procp = symvalue(WAITQUEUE);
      else
	procp = cdr(previous);
    } else {
      previous = procp;
      procp = cdr(procp);
    }
  }

  if(firstp) PreviousCell = firstp;

#ifdef DEBUG
  if(i != list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
     +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE)))
    Error("Scheduling Error! code: 1.", NIL);
#endif

  if(PreviousCell == NIL)
    for(previous = NULL, procp = symvalue(PROCQUEUE); procp != NIL;
	previous = procp, procp = cdr(procp)) {
      if(iscpu(car(procp)) && cpustatus(car(procp)) == SUSPEND)
	break;
    }
  else {
    for(previous = PreviousCell, procp = cdr(PreviousCell); procp != NIL;
	previous = procp, procp = cdr(procp)) {
      if(iscpu(car(procp)) && cpustatus(car(procp)) == SUSPEND)
	break;
    }
    if(procp == NIL) {
      for(previous = NULL, procp = symvalue(PROCQUEUE);
	  procp != cdr(PreviousCell); previous = procp, procp = cdr(procp)) {
	if(iscpu(car(procp)) && cpustatus(car(procp)) == SUSPEND)
	  break;
      }
      if(procp == cdr(PreviousCell)) procp = NIL;
    }
  }

  if(procp != NIL)
    cpu = cpuregister(car(procp));
  else
    return(0);

  CurrentCell = procp;
  PreviousCell = previous;

  /* cpustatus(car(procp)) = SUSPEND; /* for DEBUG */

  cpustatus(car(procp)) = SECDR_Cycle( cpu );

#ifdef DEBUG
  i = list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
    +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE));
#endif

  switch(cpustatus(car(procp))) {
  case TERMINATE:
    if(PreviousCell != NULL)
      cdr(PreviousCell) = cdr(CurrentCell);
    else
      symvalue(PROCQUEUE) = cdr(CurrentCell);
    if(symvalue(RESWAITQUEUE) != NIL) {
      flag(car(symvalue(RESWAITQUEUE))) &= (~F_FUTURE);
      phobject(car(symvalue(RESWAITQUEUE))) = (car(CurrentCell));
      symvalue(RESWAITQUEUE) = cdr(symvalue(RESWAITQUEUE));
    } else if(symvalue(BACKUPCPU) != NIL) {
      cdr(CurrentCell) = symvalue(TERMCPU);
      symvalue(TERMCPU) = CurrentCell;
#ifdef DEBUG
      if(i != list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
	 +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE)))
        Error("Scheduling Error! code: 2.", NIL);
#endif
    }
    break;
  case SUSPEND:
    PreviousCell = CurrentCell;
    break;
  case WAIT:
    if(PreviousCell != NULL)
      cdr(PreviousCell) = cdr(CurrentCell);
    else
      symvalue(PROCQUEUE) = cdr(CurrentCell);
    cdr(CurrentCell) = symvalue(WAITQUEUE);
    symvalue(WAITQUEUE) = CurrentCell;
#ifdef DEBUG
    if(i != list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
       +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE)))
        Error("Scheduling Error! code: 3.", NIL);
#endif
    break;
  case SEMWAIT:
    if(PreviousCell != NULL)
      cdr(PreviousCell) = cdr(CurrentCell);
    else
      symvalue(PROCQUEUE) = cdr(CurrentCell);
    cdr(CurrentCell) = NIL;
    if((procp = symvalue(SEMWAITQUEUE)) == NIL) {
      symvalue(SEMWAITQUEUE) = CurrentCell;
#ifdef DEBUG
      if(i != list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
	 +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE)))
        Error("Scheduling Error! code: 4.", NIL);
#endif
    } else {
      for(; cdr(procp) != NIL; procp = cdr(procp));
      cdr(procp) = CurrentCell;
#ifdef DEBUG
      if(i != list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
	 +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE)))
        Error("Scheduling Error! code: 5.", NIL);
#endif
    }
    break;
  case ERRORSUSPEND:
    /* ERRORCPU */
    break;
  }
  return(0);
}

int wakeup(semaphore)
pointer semaphore;
{
  pointer procp, previous, cp;
#ifdef DEBUG
  int i;
#endif

  if(Master.cpu_status == SEMWAIT && Master.R_Reg == semaphore) {
    Master.cpu_status = SUSPEND;
    return(0);
  }

#ifdef DEBUG
  i = list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
    +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE));
#endif

  for(previous = NULL, procp = symvalue(SEMWAITQUEUE);
      procp != NIL; previous = procp, procp = cdr(procp)) {
    if (iscpu(car(procp)) && cpustatus(car(procp)) == SEMWAIT &&
	cpuregister(car(procp))->R_Reg == semaphore) {
      cpustatus(car(procp)) = SUSPEND;
      break;
    }
  }
  if(procp == NIL)
    semaphorenum(semaphore)++;
  else {
    if(previous == NULL)
      symvalue(SEMWAITQUEUE) = cdr(procp);
    else
      cdr(previous) = cdr(procp);

    if (Current->cpu_id == 0)
      if(CurrentCell != NULL) {
	for(PreviousCell = NULL, cp = symvalue(PROCQUEUE);
	    cp != NIL && cp != CurrentCell;
	    PreviousCell = cp, cp = cdr(cp));
	if (cp == NIL)
	  PreviousCell = NULL;
      }

    if(PreviousCell != NULL) {
      cdr(procp) = cdr(PreviousCell);
      cdr(PreviousCell) = procp;
    } else {
      cdr(procp) = symvalue(PROCQUEUE);
      symvalue(PROCQUEUE) = procp;
    }
    PreviousCell = procp;
  }
#ifdef DEBUG
  if(i != list_len(symvalue(TERMCPU))+list_len(symvalue(WAITQUEUE))
     +list_len(symvalue(PROCQUEUE))+list_len(symvalue(SEMWAITQUEUE)))
      Error("Scheduling Error! code: 6.", NIL);
#endif
  return(0);
}
