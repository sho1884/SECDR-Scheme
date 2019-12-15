/*
 * memory.c
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/memory.c,v 0.8 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"
#ifndef ASL
#include <signal.h>
#endif

#ifdef ASL
extern void rotate_cursor(int right);
#endif

pointer cell_seg[CELL_NSEGMENT];
int cell_seglast = -1;

extern pointer free_cell;  /* pointer to top of free cells */
extern long fcells;        /* # of free cells */

/* allocate new cell segment */
int alloc_cellseg(n)
int n;
{
    register pointer p;
    register long i;
    register int k;

    for( k = 0 ; k < n ; k++) {   
        if (cell_seglast >= CELL_NSEGMENT - 1) return k;
        p = (pointer)malloc((long)CELL_SEGSIZE*sizeof(struct cell));
        if (p == (pointer)0) return k;
        cell_seg[++cell_seglast] = p;
        fcells += CELL_SEGSIZE;
        for (i = 0 ; i < CELL_SEGSIZE - 1 ; i++, p++) {
            scmtype(p) = 0; flag(p) = 0; car(p) = NIL; cdr(p) = p+1;
        }
        scmtype(p) = 0; flag(p) = 0; car(p) = NIL; cdr(p) = free_cell;
        free_cell = cell_seg[cell_seglast];
    }
    return n;
}

/* get new cell.  parameters a, b are marked by gc. */
pointer get_cell(a,b)
register pointer a;
register pointer b;
{
    register pointer x;

    if ((x=free_cell) == NIL) {
        gc(a, b);
        if (free_cell == NIL)
            if (!alloc_cellseg(1)) {
                Master.S_Reg = Master.E_Reg = Master.C_Reg = Master.D_Reg
                             = Master.R_Reg = TEMP = NIL;
                Slave.S_Reg = Slave.E_Reg = Slave.C_Reg = Slave.D_Reg
                             = Slave.R_Reg = NIL;
                gc(NIL, NIL);
                if (free_cell != NIL)
                    Error("run out of cells --- rerurn to top level", NIL);
                else
                    FatalError("run out of cells --- unable to recover cells",
                               "", "", "");
            }
        x = free_cell;
    }
    free_cell = cdr(x);
    --fcells;
    return (x);
}

void *gc_alloc(size)
long size;
{
    register void *p;

    p = (void *)malloc(size);
    if (p != NULL) return (p);
    else {
        gc(NIL, NIL);
        p = (void *)malloc(size);
        if (p != NULL) return (p);
        else Error("Unable to allocate special area", NIL);
    }
}

/* ========== garbage collector ========== */
#define isatom(p)       (flag(p)&F_ATOM)
#define setatom(p)      flag(p) |= F_ATOM
#define clratom(p)      flag(p) &= CLRATOM

#define ismark(p)       (flag(p)&F_MARK)
#define setmark(p)      flag(p) |= F_MARK
#define clrmark(p)      flag(p) &= UNMARK

/*
 *  We use algorithm E (Kunuth, The Art of Computer Programming Vol.1,
 *  sec.3.5) for marking.
 */

static void mark(a)
pointer a;
{
    register pointer t, q, p;

    if (ismark(a)) return;  /* <----- Bug fixed by S.Hayashi */

E1: t = (pointer)0; p = a;
E2: setmark(p);
    if (isvector(p)) {
        /* I think recursive use of mark for vector is not so deep. */
        register long len = vectorlen(p);
        register pointer *vec = vector(p);
        while (len-- > 0) mark(*vec++);
#ifdef Mulasame
    } else if (iscpu(p)) {
        /* I think recursive use of mark for cpu is not so deep. */
        mark(cpuregister(p)->S_Reg);
        mark(cpuregister(p)->E_Reg);
        mark(cpuregister(p)->C_Reg);
        mark(cpuregister(p)->D_Reg);
        mark(cpuregister(p)->R_Reg);
        if(cpuid(p) > 1 && cpustatus(p)) mark(cpuregister(p)->cont);
    } else if (isplaceholder(p)) {
        /* I think recursive use of mark for place holder is not so deep. */
        if(phobject(p) != NULL) mark(phobject(p));
#endif
    }
E3: if (isatom(p)) goto E6;
E4: q = car(p);
    if (q != (pointer)0  && !ismark(q)) {
        setatom(p); car(p) = t; t = p; p = q; goto E2;
    }
E5: q = cdr(p);
    if (q != (pointer)0 && !ismark(q)) {
        cdr(p) = t; t = p; p = q; goto E2;
    }
E6: if (t == (pointer)0) return;
    q = t;
    if (isatom(q)) {
        clratom(q); t = car(q); car(q) = p; p = q; goto E5;
    } else {
        t = cdr(q); cdr(q) = p; p = q; goto E6;
    }
}

/* garbage collection. parameters a, b are marked. */
void gc(a,b)
register pointer a;
register pointer b;
{
    register pointer p;
    register int i;
    register long j;
    double tgc_time, tegc_time;

#ifndef ASL
    signal(SIGINT, SIG_IGN);
#endif
#ifdef RUNTIME
    tgc_time = myruntime();
#endif

    if (gc_verbose) printf("; GC...");

    /* mark system globals */
    for (i = 0 ; i < HASHTABLESIZE ; i++)
    {
        mark(oblist[i]);
#ifdef ASL
        rotate_cursor(0);
#endif
    }

    /* mark current registers */
    mark(Master.S_Reg); mark(Master.E_Reg); mark(Master.C_Reg);
    mark(Master.D_Reg); mark(Master.R_Reg);
    
    /* mark Slave registers */
    mark(Slave.S_Reg); mark(Slave.E_Reg); mark(Slave.C_Reg);
    mark(Slave.D_Reg); mark(Slave.R_Reg);
    
    /* mark temporary pointer */
    mark(TEMP);
    
    /* mark current port */
    mark(cur_inport); mark(cur_outport);

    /* mark variables a, b */
    mark(a); mark(b);

    /* garbage collect */
    fcells = 0; free_cell = NIL;
    for (i = 0 ; i <= cell_seglast ; i++) {
#ifdef ASL
        rotate_cursor(0);
#endif
        for (j = 0, p = cell_seg[i] ; j < CELL_SEGSIZE ; j++, p++) {
            if (ismark(p)) clrmark(p);
            else {
                if (isstring(p)) free(stringvalue(p));
                if (isvector(p)) free(vector(p));
                scmtype(p) = 0;
                flag(p) = 0;
                cdr(p) = free_cell;
                car(p) = NIL;
                free_cell = p;
                ++fcells;
            }
        }
    }
    
    if (gc_verbose)
        printf(" done %ld cells are recovered.\n", fcells);
#ifdef RUNTIME
    gc_time += (tegc_time = myruntime()) - tgc_time;
    if (gc_verbose)
        printf("; GC took %G Seconds.\n", tegc_time - tgc_time);
#endif
#ifndef ASL
    signal(SIGINT, breakHandler);
#endif
}
