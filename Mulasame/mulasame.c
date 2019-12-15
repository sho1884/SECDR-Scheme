/*
 * =========================================================================
 *
 *                            ----- Mulasame ----- 
 *
 *     ------------------------------------------------------------------
 *     Parallel SECDR-Scheme Version 0.9 coded by Shoichi Hayashi
 *
 *            E-MAIL :  s-haya@rst.fujixerox.co.jp
 *            MIX    :  s.hayasi
 *     ------------------------------------------------------------------
 *     Version 1.0(R4RS) modified and coded by Shoichi Hayashi (6/7/1993)
 *
 *            E-MAIL :  s-haya@rst.fujixerox.co.jp
 *            MIX    :  s.hayasi
 *     ------------------------------------------------------------------
 *     SECDR-Scheme Version 0.65 coded by Atsushi Moriwaki (3/6/1990)
 *
 *            E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 *            MIX    :  riemann
 *            NIFTY  :  PBB01074
 *
 */
#ifndef THINK_C
static char *rcsid = 
"$Header: /home/argama/s-haya/Scheme/Mulasame/RCS/mulasame.c,v 0.10 1994/09/14 08:11:57 s-haya Exp s-haya $";
#endif
/* =========================================================================
 *
 * mulasame.c
 *
    Copyright(C) 1990,1991,1992,1993,1994 Atsushi Moriwaki, Shoichi Hayashi.
*/

#include "mulasame.h"
#include "operator.h"

#ifdef THINK_C
#define getenv(env_name) getpref(env_name)
#ifdef Mulasame
extern struct __copt console_options;
struct __copt kanji_console_options = {
  50                   /* window top */
 ,10                   /* window left */
 ,"\p<< CONSOLE >>"    /* window title */
 ,8                    /* window kind */
 ,0x41A0               /* Font ID (ASL FONT+) */
 ,9                    /* Font point */
 ,0                    /* text draw mode */
 ,25                   /* row */
 ,80                   /* column */
 ,1                    /* pause before exit */
};

FILE* substdin;
FILE* substdout;
static unsigned char mainWindowTitle[] = "\pMulasame Main Window";
static unsigned char subWindowTitle[] = "\pMulasame Sub Window";
#endif
#endif
#ifdef ASL
#define ENVIRONMENT_SIZE 4096
char* Environment;
#endif

#ifdef BSDALRM
#ifdef Mulasame
struct itimerval timer = { 0, 1, 0, 1};
#endif
#endif

CPU Master;
CPU Slave;
CPU* Current;
#ifdef Mulasame
int step;
int switchflg = 1;
int checkscheduleflg = 0;
pointer CurrentCell;
pointer PreviousCell;
#endif

int maxint, maxintstrlen;
Char maxintstr[30];
jmp_buf error_jmp;
double gc_time;
int repflg;

#ifdef ASL
extern struct prim_cell * prim_table;
extern Char** asl_name_table;

extern pointer cell_seg[];
extern int cell_seglast;

extern struct cell _NIL;
extern pointer NIL;         /* special cell representing empty cell */
extern struct cell _T;
extern pointer T;           /* special cell representing #t */
extern struct cell _F;
extern pointer F;           /* special cell representing #f */
extern struct cell _VOID;
extern pointer VOID;        /* special cell representing #v (void) */
extern struct cell _DUMMY;
extern pointer DUMMY;       /* dummy cell */
extern pointer TEMP;        /* temporary pointer marked by gc */
extern pointer free_cell;   /* pointer to top of free cells */
extern long fcells;         /* # of free cells */

extern struct cell _stdin_port;
extern pointer stdin_port;       /* pointer to standart input port */
extern struct cell _stdout_port;
extern pointer stdout_port;      /* pointer to standard output port */
extern struct cell _stderr_port;
extern pointer stderr_port;      /* pointer to standard error port */
extern struct cell _eof_obj;
extern pointer eof_obj;          /* pointer to end of file object */
extern pointer cur_inport;       /* pointer to current input port */
extern pointer cur_outport;      /* pointer to current output port */

extern Char gc_verbose;       /* if gc_verbose is not zero, print gc status */

extern struct op_cell operator_table[];

extern struct char_cell char_table[];
#endif

#ifdef ASL
_main(argc,argv)
int    argc;
char **argv;
#else
void main()
#endif
{
    pointer x, result;
    extern void init_scheme();
    double start_time, end_time;
    Char BootPath[256];
    Char* sp;
    int pathlen;
#ifdef Mulasame
#ifndef THINK_C
#ifndef BSDALRM
    char* input_cp;
    int rc;
    int i;
#endif
#endif
#endif

#ifdef ASL
    extern void init_prim_cell();
    int j;

    prim_table = (struct prim_cell *)malloc(sizeof(struct prim_cell) * MAXNAMETABLE);
    asl_name_table=(Char**)malloc(MAXNAMETABLE * sizeof(Char*));
    oblist=(pointer*)malloc(HASHTABLESIZE * sizeof(pointer));
    
    for(j=0; j<MAXNAMETABLE; j++)
        asl_name_table[j]=(Char*)malloc(24);

    init_prim_cell();

    NIL = &_NIL;       /* special cell representing empty cell */
    T = &_T;           /* special cell representing #t */
    F = &_F;           /* special cell representing #f */
    VOID = &_VOID;     /* special cell representing #v (void) */
    DUMMY = &_DUMMY;   /* dummy cell */
    for(j=0; j<HASHTABLESIZE; j++)
        oblist[j]=0;  /* symbol table */
    free_cell = &_NIL; /* pointer to top of free cells */
    fcells = 0;           /* # of free cells */

    COMPILE=0;     /* pointer to symbol -*-compile-*- */
    ERRORHOOK=0;   /* pointer to symbol -*-error-hook-*- */
    EVALHOOK=0;    /* pointer to symbol -*-eval-hook-*- */
    EXECHOOK=0;    /* pointer to symbol -*-exec-hook-*- */
    SYMBOLVALUE=0; /* pointer to symbol %symbol-value% */
    SYMBOLNAME=0;  /* pointer to symbol %symbol-name% */
    QUOTE=0;       /* pointer to symbol quote */
    QQUOTE=0;      /* pointer to symbol quasiquote */
    UNQUOTE=0;     /* pointer to symbol unquote */
    UNQUOTESP=0;   /* pointer to symbol unquote-splicing */

    stdin_port = &_stdin_port;    /* pointer to standart input port */
    stdout_port = &_stdout_port;  /* pointer to standard output port */
    stderr_port = &_stderr_port;  /* pointer to standard error port */
    eof_obj = &_eof_obj;          /* pointer to end of file object */
    cur_inport = &_stdin_port;    /* pointer to current input port */
    cur_outport = &_stdout_port;  /* pointer to current output port */

    gc_verbose = 0;               /* if gc_verbose is not zero, print gc status */

    /*
    for(j=0; j<OPMAX; j++)
        operator_table[j]=0;

    for(j=0; j<CHARMAX; j++)
        char_table[j];
    */
    Environment=(char*)malloc(ENVIRONMENT_SIZE);
#endif

    srand(time(NULL));

    repflg=0;
    maxint=-1; maxint=(int)((unsigned int)maxint >> 1);
    sprintf(maxintstr, "%ld" ,maxint);
    maxintstrlen = strlen(maxintstr);

#ifdef THINK_C
    _fcreator = DEFAULTCREATOR;
#ifdef Mulasame
    console_options = kanji_console_options;
    console_options.title = mainWindowTitle;
    console_options.nrows = 25;
    console_options.top = 60;
    console_options.left = 15;
    cgotoxy(1, 1, stdout);
#endif
#endif
#ifdef Mulasame
#ifndef THINK_C
#ifndef BSDALRM
    input_rp = para_buf;
    para_buf[0] = '\0';
#endif
#endif
#endif
    message(stdout, Banner, "", "", "");
#ifdef THINK_C
#ifdef Mulasame
    console_options.title = subWindowTitle;
    console_options.nrows = 25;
    console_options.top = 40;
    console_options.left = 10;
    substdin=substdout=fopenc();
    cgotoxy(1, 1, substdout);
    fprintf(substdout, "Mulasame SubWindow!\n~~~~~~~~~~~~~~~~~~~\n");
#endif
#endif
    if (setjmp(error_jmp))
        FatalError("Error in booting process", "", "", "");
    init_scheme();
#ifdef Turtle
    init_turtlegr();
#endif
    Current = &Master;
#ifdef Mulasame
    CurrentCell = NULL;
    PreviousCell = NULL;
#endif
    Slave.S_Reg = Slave.E_Reg = Slave.C_Reg = Slave.D_Reg = Slave.R_Reg = NIL;
    Master.S_Reg = Master.E_Reg = Master.C_Reg = Master.D_Reg = Master.R_Reg = NIL;
#ifdef Mulasame
    Slave.cpu_id = 1;
    Slave.cpu_step = 0;
    Slave.cpu_status = SUSPEND;
    Master.cpu_id = 0;
    Master.cpu_step = DEFAULT_CPU_STEP;
    Master.cpu_status = SUSPEND;
#endif
    TEMP = NIL;
#ifdef Mulasame
    if((sp = (Char*)getenv("MULASAME_BOOT_PATH")) == 0)
#else
    if((sp = (Char*)getenv("SECDR_BOOT_PATH")) == 0)
#endif
        strncpy(BootPath, DefaultPath, 246);
    else
        strncpy(BootPath, sp, 246);
    if((pathlen=strlen(BootPath)) && BootPath[pathlen-1]!=Suffix) {
        BootPath[pathlen]=Suffix;
        BootPath[pathlen+1]='\0';
    }

    /* load boot system */
    printlist(stdout, bin_load(mk_string(strcat(BootPath, BootFile)), F, F), 1);
    Current->S_Reg = Current->E_Reg = Current->C_Reg = Current->D_Reg = Current->R_Reg = NIL;
    TEMP = NIL;

#ifdef Mulasame
    if((sp = (Char*)getenv("MULASAME_BOOT_PATH")) == 0)
#else
    if((sp = (Char*)getenv("SECDR_BOOT_PATH")) == 0)
#endif
        strncpy(BootPath, DefaultPath, 246);
    else
        strncpy(BootPath, sp, 246);
    if((pathlen=strlen(BootPath)) && BootPath[pathlen-1]!=Suffix) {
        BootPath[pathlen]=Suffix;
        BootPath[pathlen+1]='\0';
    }

    switch(setjmp(error_jmp)) {
    case 0:
        /* load boot init */
        printlist(stdout, load(mk_string(strcat(BootPath, InitFile)), F, F), 1);
        message(stdout, "\n", "", "", "");
        break;
    case INTERRUPT:
    case ERRORCOD:
        if((Master.S_Reg!=NIL || Master.E_Reg!=NIL || Master.C_Reg!=NIL ||
            Master.D_Reg!=NIL || Master.R_Reg!=NIL) && repflg) {
            SECDR_Cycle(&Master);
#ifdef Mulasame
	    switchflg = 1;
	    checkscheduleflg = 1;
#endif
            /* restore registers */
            Master.S_Reg = car(Master.D_Reg);
            Master.E_Reg = cadr(Master.D_Reg);
            Master.C_Reg = caddr(Master.D_Reg);
            Master.D_Reg = cdddr(Master.D_Reg);
            printlist(portfile(cur_outport), Master.R_Reg, 1);
            message(portfile(cur_outport), "\n", "", "", "");
        }
        break;
    }

    repflg = 1;
#ifndef ASL
    signal(SIGINT,breakHandler);
#endif
    for (;;) {
        if (cur_inport == stdin_port) message(stdout, prompt, "", "", "");
        Current = &Master;
        Slave.S_Reg = Slave.E_Reg = Slave.C_Reg = Slave.D_Reg = Slave.R_Reg = NIL;
        Master.S_Reg = Master.E_Reg = Master.C_Reg = Master.D_Reg = Master.R_Reg = NIL;
        TEMP = NIL;
#ifdef Mulasame
# ifdef THINK_C
	myBackgroundProcess = subCpuHandler;
# else
#  ifdef BSDALRM
	setitimer(ITIMER_REAL, &timer, NULL);
	signal(SIGALRM, SIG_IGN);
	signal(SIGALRM, subCpuHandler);
#  else
	if(*input_rp == '\n' || *input_rp == '\0'){
	  if(stdin->_cnt == 0 || stdin->_cnt == 1 && *(stdin->_ptr) == '\n')
	    para_fflush(stdin);
	  else
	    for(i = 0;  i < stdin->_cnt; i++) putchar(*(stdin->_ptr+i));
	}

	if(buf_flg == 0){
	  para_flg=1;
	  change_para_mode;
	  for(input_cp = para_buf;;){
	    if((rc = getchar()) == EOF){
	      clearerr(stdin);
	      subCpuHandler();
	    } else if(rc == 0x7f || rc == 0x08) {
	      putchar(0x08);
	      putchar(' ');
	      putchar(0x08);
	      if(input_cp != para_buf) {
	        putchar(0x08);
		putchar(' ');
		putchar(0x08);
		input_cp--;
	      }
	    } else if(rc == '\n') {
	      *input_cp = rc;
	      input_cp++;
	      *input_cp = '\0';
	      buf_flg = 1;
	      para_flg = 0;
	      change_para_mode;
	      break;
	    } else {
	      *input_cp = rc;
	      input_cp++;
	    }
	  }
	}
#  endif
# endif
#endif

        if((x = readsexpr(Current, portfile(cur_inport))) == eof_obj){
	  quit(0);
	}

#ifdef Mulasame
#ifdef BSDALRM
    signal(SIGALRM, SIG_IGN);
#endif
#ifdef THINK_C
	myBackgroundProcess = NULL;
#endif
#endif
        gc_time = 0;
#ifdef RUNTIME
        start_time = myruntime();
#endif
        result = compile_run(x);
#ifdef RUNTIME
        end_time = myruntime();
        printf("; Evaluation took %G Seconds", end_time - start_time);
        printf(" (%G in gc).\n", gc_time);
#endif
        printlist(portfile(cur_outport), result, 1);
        message(portfile(cur_outport), "\n", "", "", "");
    }
}

#ifdef ASL
void _exit(ret)
int ret;
{
    int i, j;

    if (cell_seglast != -1)        /* free cell memory */
    {
        for (i=0;i <= cell_seglast;i++)
            free(cell_seg[i]);
    }

    for(j=0; j<MAXNAMETABLE; j++)
        free(asl_name_table[j]);

    free(prim_table);
    free(asl_name_table);
    free(oblist);
    free(Environment);

/*  if (str_seglast != -1)        /* free str memory *
    {
        for (i=0;i <= str_seglast;i++)
            free(str_seg[i]);
    }*/
}
#endif

/* --------------------------------------------------------------------------
 * Interrupt handling:
 * ------------------------------------------------------------------------*/

void breakHandler()
{               /* respond to break interrupt */
#ifdef Mulasame
#ifdef THINK_C
	myBackgroundProcess = NULL;
#endif
#ifdef BSDALRM
    signal(SIGALRM, SIG_IGN);
#endif
#endif
    para_fflush(stdin);
#ifdef THINK_C
#ifdef Mulasame
    fflush(substdin);
#endif
#endif
    fflush(stdout);
#ifdef THINK_C
#ifdef Mulasame
    fflush(substdout);
#endif
#endif
#ifndef ASL
    signal(SIGINT,breakHandler);
#endif
#ifdef THINK_C
#ifdef Mulasame
	cur_inport = stdin_port;
	cur_outport = stdout_port;
#endif
#endif
    fprintf(stderr, "\n; Interrupted!\n");
    if(repflg) {
        /* Error Hook */
        Slave.S_Reg = cons(NIL, NIL);
        Slave.E_Reg = NIL;
        Slave.C_Reg = cons(mk_operator(OP_AP), cons(mk_operator(OP_STOP), NIL));
        Slave.D_Reg = NIL;
        Slave.R_Reg = symvalue(ERRORHOOK);
        SECDR_Cycle(&Slave);
    }
    flushinput();
    flushoutput();
    longjmp(error_jmp, INTERRUPT);
}

#ifdef Mulasame
#ifdef THINK_C
void subCpuHandler()
{
    int x, y;
    double start_time, end_time;

    myBackgroundProcess = NULL;
    if(switchflg)
        for(end_time = start_time = myruntime();
            end_time - start_time < 0.1;
            end_time = myruntime() )
            scheduler();
    myBackgroundProcess = subCpuHandler;
/*  cgetxy(&x, &y, stdout);
    cgotoxy(, y, stdin);*/
}
#else
void subCpuHandler()
{
#ifdef BSDALRM
    signal(SIGALRM, SIG_IGN);
#endif
    if(switchflg) scheduler();
#ifdef BSDALRM
    setitimer(ITIMER_REAL, &timer, NULL);
    signal(SIGALRM, subCpuHandler);
#endif
}
#endif
#endif
