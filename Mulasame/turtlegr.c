
/*      file    turtlegr.c                              *
 *      Copyright (C) 1992      sjm@ee.tut.fi           *
 *                              jtl@cc.tut.fi           *
 *                                                      *
 *      Turtlegraphics primitives for the               *
 *      SCM interpreter by Aubrey Jaffer                *
 *                                                      *
 *>>>   for SECDR Scheme & Mulasame (X11, Macintosh) <<<*
 *>>>   Date revised    4.8.1994 by Shoichi Hayashi  <<<*
 *                                                      *
 *      Last modification: 13.10.1992                   *
 *                                                      *
 *      Versions:                                       *
 *      12.3.1992       The first version.              *
 *      13.3.1992       Added the possibility to pass   *
 *                      floating point args.            *
 *      15.3.1992       Graphics cards other than EGA   *
 *                      are now supported.              *
 *      9.4.1992        The internal representation     *
 *                      of X & Y is now float.          *
 *      13.10.1992      Added X11 support.              *
 *                      A major rewrite of certain      *
 *                      parts.                          *
 *                      Put -DX11 -DFLOATS to CFLAGS    *
 *                      in Makefile to get it.          *
 *                                                      *
 *      REMEMBER to define INITS=init_turtlegr()        *
 *      in the Makefile.                                *
 *                                                      */
#ifndef THINK_C
static char *rcsid2 = 
"$Header: /usr/PDS/lang/Scheme/Mulasame/RCS/turtlegr.c,v 1.2 1994/09/14 02:55:46 s-haya Exp s-haya $";
#endif
/*                                                                      *
 *       This code tries to compromise between two very different       *
 *      systems: MSDOS and UNIX with the X11 windowing system.          *
 *      The MSDOS version was build first and it really shows.  :)      *
 *      The X port is a partial rewrite of the old MSDOS stuff          *
 *      and plays around with #ifdef's a lot.  The result is,           *
 *      eventually, a C source which is expected to compile             *
 *      under both MSDOS and UNIX (X11).                                *
 *       The X code handles colors emulating CGA palette. It tries      *
 *      to act sensibly even on a monochrome screen and when the        *
 *      color palette is full.                                          *
 *       X event handling is implemented with polling whenever          *
 *      appropriate. This is not The Right Way to do it in X, but       *
 *      it was easiest to adopt in this case.                           *
 *       Another solution would have been to make the X graphics        *
 *      a separate process, but I didn't want to because I wanted       *
 *      to keep it simple. I can't tell how good an example of porting  *
 *      MSDOS software to X this is, but it works.                      *
 *                                                                      *
 *       This has been tested with SunOs 4.1.2 with X11R5, Linux 0.98.1 *
 *      with Xfree86 1.1 (X11R5 port) and in MSDOS with BC 3.1.         *
 *      Because the code uses only the basic Xlib calls, it should      *
 *      compile without problems under _any_ UNIX with X11R4 or newer.  *
 *                                                                      *
 *      Please send bugreports to sjm@ee.tut.fi.                        *
 *      I'm especially interested in hearing about ports to other       *
 *      platforms than those tested by me.                              *
 *                                                                      *
 *      - sjm                                                           *
 *                                                                      */

/************************************************************************/
/************************************************************************/
#ifdef Turtle /************* SECDR-Scheme & Mulasame ********************/
/************************************************************************/
/************************************************************************/

#ifdef BSD
#define atexit( dummy )
#endif
#ifdef Macintosh
#define atexit( dummy )
#endif

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif

#ifdef X11
/****************************************************/
/*****  X11 specific includes & defines         *****/
/****************************************************/

/* Xlib include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "turtle"
#define BITMAPDEPTH 1

#define PROGNAME        "mulasame"
#define CLASSNAME       "Mulasame"
#define WINDOWNAME      "Mulasame Turtle-graphics Window"
#define ICONNAME        "Turtle"

#define GR_MAX_XSIZE    1024
#define GR_MAX_YSIZE    1024
#define GR_DEF_XSIZE    640
#define GR_DEF_YSIZE    480
#define GR_MIN_XSIZE    64
#define GR_MIN_YSIZE    64

/* Fake CGA colormap with X - yuk!                                */
#define GR_COLORS       16                /* CGA/EGA counterpart  */
#define GR_COLOR00      "black"           /* black                */
#define GR_COLOR01      "slate gray"      /* dark gray            */
#define GR_COLOR02      "gray"            /* gray                 */
#define GR_COLOR03      "gray90"          /* light gray           */
#define GR_COLOR04      "RosyBrown1"      /* light brown          */
#define GR_COLOR05      "brown3"          /* dark brown           */
#define GR_COLOR06      "green4"          /* dark green           */
#define GR_COLOR07      "green yellow"    /* light green          */
#define GR_COLOR08      "cyan"            /* cyan                 */
#define GR_COLOR09      "blue"            /* blue                 */
#define GR_COLOR10      "dark violet"     /* violet               */
#define GR_COLOR11      "magenta"         /* magenta              */
#define GR_COLOR12      "orange red"      /* red                  */
#define GR_COLOR13      "orange"          /* orange               */
#define GR_COLOR14      "yellow"          /* yellow               */
#define GR_COLOR15      "white"           /* white                */

#ifdef  __STDC__
static void     gr_events( int );
#else
static void     gr_events();
#endif
#endif

#ifdef Macintosh
#define GR_COLORS       16

extern WindowPtr myGraphicsWindow;
extern Rect grbox;

GrafPtr	backupPortPtr;
CGrafPort  backupPort;
GDHandle   maxghd;

RGBColor *currentColor;
RGBColor check_colortbl[GR_COLORS];
RGBColor gr_colortbl[16] = {	/* red, green, blue */
  /* Zero is black. We're 20,000 toward white, which is 65,535. */
  {0x0000, 0x0000, 0x0000},		/* black                */
  {0x4000, 0x4000, 0x4000},		/* dark gray            */
  {0x8000, 0x8000, 0x8000},		/* gray                 */
  {0xcccc, 0xcccc, 0xffff},		/* light gray           */
  {0xaaa8, 0x7fff, 0x2aaa},		/* light brown          */
  {0x7fff, 0x3fff, 0x0126},		/* dark brown           */
  {0x0000, 0x8000, 0x11b0},		/* dark green           */
  {0x0000, 0xd400, 0x0000},		/* light green          */
  {0x0241, 0xab54, 0xeafe},		/* cyan                 */
  {0x0000, 0x0000, 0xd400},		/* blue                 */
  {0x6ff9, 0x0000, 0xdff2},		/* violet               */
  {0xf2d7, 0x0856, 0x84ec},		/* magenta              */
  {0xdd6b, 0x08c2, 0x06a2},		/* red                  */
  {0xffff, 0x7fff, 0x024d},		/* orange               */
  {0xfc00, 0xf37d, 0x052f},		/* yellow               */
  {0xffff, 0xffff, 0xffff},		/* white                */
};
#endif

/********************************************/
/***** GENERIC code, declarations       *****/
/********************************************/
#define         SIN( x )                \
  sin( ((x)/180.0) * M_PI )
#define         COS( x )                \
  cos( ((x)/180.0) * M_PI )

static  int             gr_graphicsavail = 0;
static  int             gr_grmode_on = 0;
static  float           gr_dir = 0.0;
static  int             gr_max_x=0, gr_max_y=0, gr_max_color=0;
static  float           gr_x=0.0, gr_y=0.0;
static  int             gr_color = 0;

#ifdef __GNUC__
inline
#else
  static
#endif
  int      valid_XYC( x, y, color )
int     x, y, color;
{
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif  
  if( (x <= gr_max_x) && (y <= gr_max_y) && (color <= gr_max_color)
     && (x >= 0) && (y >= 0) && (color >= 0) )
    return( 1 );
  else
    return( 0 );
} /* valid_XYC() */

#ifdef X11
/********************************************************************/
/*****  X11 specific variable and function declarations         *****/
/********************************************************************/

static Display          *gr_display;            /* The X display        */
static int              gr_screen;              /* The X screen number  */
static Window           gr_win;                 /* The drawable Window  */
static GC               gr_gc;                  /* Graphics Context     */
static unsigned long    gr_colortbl[GR_COLORS]; /* Color table          */
static XEvent           gr_event;               /* Event structure      */

/* These are needed for XSetWMProperties */
static char     *gr_windowname  = WINDOWNAME;
static char     *gr_iconname    = ICONNAME;
static char     gr_progname[]   = PROGNAME;
static char     gr_classname[]  = CLASSNAME;
static int      gr_argc         = 1;
static char     *gr_argv[]      = { gr_progname, NULL };

static void gr_eventhandler( event )
XEvent event;
{
  switch( event.type ) {
  case ConfigureNotify:
    gr_max_x = event.xconfigure.width - 1;
    gr_max_y = event.xconfigure.height - 1;
    break;
  case MapNotify:
    break;
  case DestroyNotify:
    break;
  case UnmapNotify:
    break;
  case Expose:
    if( event.xexpose.count != 0 )
      break;
    break;
  case ClientMessage:
    break;
  default:
    /* Throw away any unknown events */
    break;
  } /* switch */
}

static void gr_events( expected )
int expected;
{
  int i;
  
  /* Get at least 'expected' events */
  for( i = 0; i < expected; ++i ) {
    XNextEvent( gr_display, &gr_event );
    gr_eventhandler( gr_event );
  }
  /* Handle all remaining events if there are any */
  /* XPending will call XFlush() if it doesn't find events at once */
  while( XPending(gr_display) ) {
    XNextEvent( gr_display, &gr_event );
    gr_eventhandler( gr_event );
  } /* while */
} /* gr_events() */

static void gr_typedevent( type )
int type;
{
  do {
    XNextEvent( gr_display, &gr_event );
    gr_eventhandler( gr_event );
  } while( gr_event.type != type );
  /* Handle all remaining events if there are any */
  /* XPending will call XFlush() if it doesn't find events at once */
  while( XPending(gr_display) ) {
    XNextEvent( gr_display, &gr_event );
    gr_eventhandler( gr_event );
  } /* while */
}
#endif

#ifdef Macintosh
drawBackupScreen()
{
  GDHandle saveghd;

  saveghd=GetGDevice();
  SetGDevice(maxghd);
  CopyBits( &(backupPortPtr->portBits),
	    &(myGraphicsWindow->portBits),&grbox,&grbox,0,0L );
  SetGDevice(saveghd);
}

void makeColorCheckTable(short colorflg){
  GDHandle saveghd;
  WindowPtr save;
  RGBColor rgb;
  PixMapHandle pmh;
  CTabHandle cth;
  ColorSpec *ctt;
  int i;

  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  pmh = backupPort.portPixMap;
  HLock(pmh);
  cth = (*pmh)->pmTable;
  HLock(cth);
  ctt = (*cth)->ctTable;
  if( (gr_max_color = (*cth)->ctSize) >= 16 && colorflg) {
    gr_max_color = GR_COLORS -1;
    for( i = 0; i < GR_COLORS; ++i ) {
      RGBForeColor(&gr_colortbl[ i ]);
      MoveTo (5, 5);
      Line (0, 0);
      GetCPixel(5, 5, &rgb); /* get Color Number */
      check_colortbl[i] = rgb;
      RGBForeColor(&gr_colortbl[ 0 ]);
      PaintRect(&grbox);
    }
  } else if( (gr_max_color = (*cth)->ctSize) >= 16) {
    check_colortbl[0] = gr_colortbl[0];
    for( i = 1; i < GR_COLORS; ++i ) {
      gr_colortbl[i] = ctt[((gr_max_color+1)/GR_COLORS)*(GR_COLORS-i)-1].rgb;
      RGBForeColor(&gr_colortbl[ i ]);
      MoveTo (5, 5);
      Line (0, 0);
      GetCPixel(5, 5, &rgb); /* get Color Number */
      check_colortbl[i] = rgb;
      RGBForeColor(&gr_colortbl[ 0 ]);
      PaintRect(&grbox);
    }
    gr_max_color = GR_COLORS -1;
  } else {
    for( i = 0; i <= gr_max_color; ++i ) {
      gr_colortbl[i] = ctt[(gr_max_color-i)].rgb;
      RGBForeColor(&gr_colortbl[ i ]);
      MoveTo (5, 5);
      Line (0, 0);
      GetCPixel(5, 5, &rgb); /* get Color Number */
      check_colortbl[i] = rgb;
      RGBForeColor(&gr_colortbl[ 0 ]);
      PaintRect(&grbox);
    }
  }
  
  HUnlock(cth);
  HUnlock(pmh);
  SetGDevice(saveghd);
  SetPort(save);
}

short chkSysDepth() {
  GDHandle      ghd;
  PixMapHandle  phd;
  SysEnvRec    sys;
  short         depth=0;
  short         maxDepth=0;

  SysEnvirons( 1,&sys );
  if( sys.hasColorQD ) {
    for(ghd=GetDeviceList(); ghd; ghd=GetNextDevice(ghd)){
      phd=(*ghd)->gdPMap;
      if ( ! ( depth=(*phd)->pixelSize ) )
        depth=1;
      if(depth>maxDepth && depth<=8){
        maxDepth = depth;
        maxghd = ghd;
      }
    }
  }
  return( maxDepth );
}

int makeGraphicsWindow()
{
  WindowPtr save;
  PixMapHandle	phd;
  GDHandle		ghd, saveghd;
  long			len,row,wh,wv;
  short			bit;
  Handle		hd;

  if( !(bit = chkSysDepth()) || bit > 8 ) return( 1 );

  GetPort(&save);
  new_graphics();
  saveghd=GetGDevice();
  SetGDevice(ghd=maxghd);

  if( !( phd=NewPixMap() ) ) {
    DisposeWindow(myGraphicsWindow);
    myGraphicsWindow = NULL;
    SetGDevice(saveghd);
    return( 1 );
  }
  CopyPixMap( (*ghd)->gdPMap,phd );
  wh=grbox.right-grbox.left;
  wv=grbox.bottom-grbox.top;
  bit=(*phd)->pixelSize;
  if( bit<=8 )
    row=((wh+3)>>2)<<2;
  else if( bit==16 )
    row=(((wh<<1)+3)>>2)<<2;
  else if( bit==32 )	
    row=wh<<2;
  len=wv*row;

  if( ! ( hd=NewHandle( len ) ) ) {
    DisposPixMap( phd );
    DisposeWindow(myGraphicsWindow);
    myGraphicsWindow = NULL;
    SetGDevice(saveghd);
    return( 1 );
  }
  HLock( hd );
  (*phd)->baseAddr=*hd;
  (*phd)->rowBytes=((*phd)->rowBytes&0x8000)+row;
  (*phd)->bounds=grbox;

  backupPortPtr = &backupPort;

  OpenCPort(backupPortPtr);
  SetPort(backupPortPtr);
  SetPortPix(phd);
  PortSize(grbox.right, grbox.bottom);
  RectRgn(backupPortPtr->visRgn, &grbox);
  EraseRect(&grbox);
  PaintRect(&grbox);

  makeColorCheckTable( (((*ghd)->gdFlags) & 0x0001) );
  
  SetGDevice(saveghd);
  SetPort(save);
  return( 0 );
}
#endif

/********************************************************************/
/********************************************************************/
/***    User callable primitive routines begin here     ***
 ***                                                    ***
 ***                                                    ***/

static pointer gr_helpgr()
{
  fputs( "\
Ret   Name               nargs    args        returns\n\
---------------------------------------------------------\n\
B  graphics-avail?         0       -          #t if graphics available\n\
B  graphics-mode!          0       -          #f if no graphics\n\
B  text-mode!              0       -          #t on success\n\
B  clear-graphics!         0       -          #f if not in graphics mode\n\
", stdout );
  fputs( "\
i  max-x                   0       -          maximum value of x\n\
i  max-y                   0       -          maximum value of y\n\
i  max-color               0       -          maximum value of color\n\
B  valid-xyc?              3       x y color  #t if valid\n\
", stdout );
  fputs( "\
B  set-dot!                3       x y color  #t on success\n\
i  get-dot                 2       x y        color of the dot in (x,y)\n\
                                              or #f if (x,y) not legal\n\
\n\
NOTE: Origin (0,0) is in the upper left corner.\n\n\
", stdout );
  return(T);
} /* gr_helpgr() */

static pointer gr_helpturtlegr()
{
  fputs( "\
Ret   Name               nargs    args        returns\n\
---------------------------------------------------------\n\
B  goto-home!              0       -          #f if not in graphics mode\n\
B  goto-center!            0       -          #f if not in graphics mode\n\
B  goto-nw!                0       -          #f if not in graphics mode\n\
B  goto-ne!                0       -          #f if not in graphics mode\n\
B  goto-sw!                0       -          #f if not in graphics mode\n\
", stdout );
  fputs( "\
B  goto-se!                0       -          #f if not in graphics mode\n\
B  draw                    1       length     #t if target within drawing area\n\
B  draw-to                 2       x y        #t if (x,y) within drawing area\n\
B  draw-to!                2       x y        #t if (x,y) within drawing area\n\
B  move                    1       length     #t if target within drawing area\n\
", stdout );
  fputs( "\
B  move-to!                2       x y        #t if (x,y) within drawing area\n\
i  where-x                 0       -          current x-coordinate\n\
i  where-y                 0       -          current y-coordinate\n\
i  turn-right              1       angle      drawing direction in degrees\n\
i  turn-left               1       angle      drawing direction in degrees\n\
", stdout );
  fputs( "\
i  turn-to!                1       angle      drawing direction in degrees\n\
i  what-direction          0       -          drawing direction in degrees\n\
B  set-color!              1       color      #t if color valid\n\
i  what-color              0       -          current drawing color\n\n\
", stdout );
  return (T);
} /* gr_helpturtlegr() */

static pointer gr_available()
{
  if( gr_graphicsavail )
    return(T);
  else
    return(F);
} /* gr_available() */

static pointer gr_maxx()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  return(mk_integer( (long)gr_max_x ));
} /* gr_maxx() */

static pointer gr_maxy()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  return(mk_integer( (long)gr_max_y ));
} /* gr_maxy() */

static pointer gr_maxc()
{
  if( !gr_grmode_on )
    return(F);
  return(mk_integer( (long)gr_max_color ));
} /* gr_maxc() */

static pointer gr_validXYC()
{
  pointer     x, y, c;
  int     xi, yi, ci;
  
  x = car(Args);
  y = cadr(Args);
  c = caddr(Args);
  
  if( !(isnumber(x) && isnumber(y) && isnumber(c)) )
    Error("valid-xyc? -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  
  if( isinteger(x) )
    xi = (int)(ivalue(x));
  else if(isrational(x))
    xi = (int)((double)qnvalue(x)/(double)qdvalue(x));
  else
    xi = (int)(rvalue(x));
  
  if( isinteger(y) )
    yi = (int)(ivalue(y));
  else if(isrational(y))
    yi = (int)((double)qnvalue(y)/(double)qdvalue(y));
  else
    yi = (int)(rvalue(y));
  
  if( isinteger(c) )
    ci = (int)(ivalue(c));
  else if(isrational(c))
    ci = (int)((double)qnvalue(c)/(double)qdvalue(c));
  else
    ci = (int)(rvalue(c));
  
  /* valid_XYC() calls gr_events() */
  if( valid_XYC( xi, yi, ci ) )
    return(T);
  else
    return(F);
} /* gr_validXYC() */

static pointer gr_grmode()
{
  if( !gr_graphicsavail )
    return(F);
#ifdef X11
  /* bwuah... but it works :) */
  if( !gr_grmode_on ) {
    XMapWindow( gr_display, gr_win );
    gr_typedevent( MapNotify );
  }
#endif
#ifdef Macintosh
  ShowWindow(myGraphicsWindow);
  SelectWindow(myGraphicsWindow);
#endif
  gr_grmode_on = 1;
  return(T);
} /* gr_grmode() */

static pointer gr_txtmode()
{
  if( !gr_graphicsavail )
    return(F);
#ifdef X11
  /* bwuah... but it works :) */
  if( gr_grmode_on ) {
    XUnmapWindow( gr_display, gr_win );
    gr_typedevent( UnmapNotify );
  }
#endif
#ifdef Macintosh
  HideWindow(myGraphicsWindow);
#endif
  gr_grmode_on = 0;
  return(T);
} /* gr_txtmode() */

static pointer gr_cleargraph()
{
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
#endif

  if( !gr_grmode_on )
    return(F);
#ifdef X11
  XClearWindow( gr_display, gr_win );
  gr_events(0);
#endif
#ifdef Macintosh
  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  RGBForeColor(&gr_colortbl[ 0 ]);
  PaintRect(&grbox);
  SetPort(myGraphicsWindow);
  RGBForeColor(&gr_colortbl[ 0 ]);
  PaintRect(&grbox);
  SetGDevice(saveghd);
  SetPort(save);
#endif
  return(T);
} /* gr_cleargraph() */

static pointer gr_setdot()
{
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
#endif
  pointer     x, y, c;
  int     xi, yi, ci;
  
  x = car(Args);
  y = cadr(Args);
  c = caddr(Args);
  
  if( !(isnumber(x) && isnumber(y) && isnumber(c)) )
    Error("set-dot! -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  
  if( isinteger(x) )
    xi = (int)(ivalue(x));
  else if(isrational(x))
    xi = (int)((double)qnvalue(x)/(double)qdvalue(x));
  else
    xi = (int)(rvalue(x));
  
  if( isinteger(y) )
    yi = (int)(ivalue(y));
  else if(isrational(y))
    yi = (int)((double)qnvalue(y)/(double)qdvalue(y));
  else
    yi = (int)(rvalue(y));
  
  if( isinteger(c) )
    ci = (int)(ivalue(c));
  else if(isrational(c))
    ci = (int)((double)qnvalue(c)/(double)qdvalue(c));
  else
    ci = (int)(rvalue(c));
  
  if( !valid_XYC( xi, yi, ci ) )
    return(F);
  
#ifdef X11
  /* Set the drawing color */
  XSetForeground( gr_display, gr_gc, gr_colortbl[ ci ] );
  XDrawPoint( gr_display, gr_win, gr_gc, xi, yi );
  /* Restore the drawing color */
  XSetForeground( gr_display, gr_gc, gr_colortbl[ gr_color ] );
  gr_events(0);
#endif
#ifdef Macintosh
  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  RGBForeColor(&gr_colortbl[ ci ]);
  MoveTo (xi, yi);
  Line (0, 0);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetPort(myGraphicsWindow);
  RGBForeColor(&gr_colortbl[ ci ]);
  MoveTo (xi, yi);
  Line (0, 0);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetGDevice(saveghd);
  SetPort(save);
#endif
  return(T);
} /* gr_setdot() */

static pointer gr_getdot()
{
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
  RGBColor rgb;
  int                     i;
#endif
  pointer     x, y;
  int                     xi, yi;
#ifdef X11
  XImage                  *xim;
  XWindowAttributes       wattr;
  unsigned long           dot;
  int                     i;
#endif
  
  x = car(Args);
  y = cadr(Args);
  
  if( !(isnumber(x) && isnumber(y)) )
    Error("get-dot -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  
  if( isinteger(x) )
    xi = (int)(ivalue(x));
  else if(isrational(x))
    xi = (int)((double)qnvalue(x)/(double)qdvalue(x));
  else
    xi = (int)(rvalue(x));
  
  if( isinteger(y) )
    yi = (int)(ivalue(y));
  else if(isrational(y))
    yi = (int)((double)qnvalue(y)/(double)qdvalue(y));
  else
    yi = (int)(rvalue(y));
  
  if( !valid_XYC( xi, yi, 0 ) )
    return(F);
  
#ifdef X11
  /* Now, this IS ugly. But it's there if you need it.            */
  
  /* Have to make sure that the window is mapped.  Tough...       */
  XGetWindowAttributes( gr_display, gr_win, &wattr );
  if( wattr.map_state == IsUnmapped ) {
    XMapWindow( gr_display, gr_win );
    gr_typedevent( MapNotify );
  }
  /* I KNOW this sucks.                                           */
#ifdef ORIGINAL
  xim = XGetImage( gr_display,gr_win, xi,yi, 1,1, AllPlanes, XYPixmap );
#else
  xim = XGetImage( gr_display,gr_win, xi,yi, 1,1, AllPlanes, ZPixmap );
#endif
  dot = XGetPixel( xim, 0, 0 );
  for( i = 0; i < GR_COLORS; ++i ) {
    if( gr_colortbl[i] == dot )
      return(mk_integer( (long)i ));
  }
  /* This should never happen. There's garbage in the window!     */
  fprintf( stderr, "%s: get-dot: Got an illegal pixel value %lu. \
Is there garbage?\n", gr_progname, dot );
  return(F);
#endif
#ifdef Macintosh
  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  GetCPixel(xi, yi, &rgb); /* get Color Number */
  SetGDevice(saveghd);
  SetPort(save);
  for( i = 0; i < GR_COLORS; ++i ) {
    if( check_colortbl[i].red == rgb.red &&
        check_colortbl[i].green == rgb.green &&
        check_colortbl[i].blue == rgb.blue )
      return(mk_integer( (long)i ));
  }
  /* This should never happen. There's garbage in the window!     */
  fprintf( stderr, "Warning: get-dot: Got an illegal pixel value. \n \
Changing characteristics of monitors, you must restart Mulasame.\n");
  return(F);
#endif
} /* gr_getdot() */

static pointer gr_draw()
{
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
#endif
  pointer     S;
  float   xf, yf;
  float   sf;
  int     ok;
  S = car(Args);
  
  if( !(isnumber(S)) )
    Error("draw -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  
  if( isinteger(S) )
    sf = (float)(ivalue(S));
  else if(isrational(S))
    sf = (float)((double)qnvalue(S)/(double)qdvalue(S));
  else
    sf = rvalue(S);
  
  ok = 1;
  xf = gr_x + ( COS( gr_dir ) * sf );
  yf = gr_y + ( SIN( gr_dir ) * sf );
  if( (int)xf > gr_max_x ) {
    xf = (float)gr_max_x;
    ok = 0;
  }
  else if( xf < 0.0 ) {
    xf = 0.0;
    ok = 0;
  }
  if( (int)yf > gr_max_y ) {
    yf = (float)gr_max_y;
    ok = 0;
  }
  else if( yf < 0.0 ) {
    yf = 0.0;
    ok = 0;
  }
  
#ifdef X11
  XDrawLine( gr_display, gr_win, gr_gc,
	    (int)gr_x,(int)gr_y,
	    (int)xf,(int)yf );
  gr_events(0);
#endif
#ifdef Macintosh
  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  RGBForeColor(currentColor);
  MoveTo ((int)gr_x,(int)gr_y);
  LineTo ((int)xf,(int)yf);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetPort(myGraphicsWindow);
  RGBForeColor(currentColor);
  MoveTo ((int)gr_x,(int)gr_y);
  LineTo ((int)xf,(int)yf);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetGDevice(saveghd);
  SetPort(save);
#endif
  gr_x = xf;
  gr_y = yf;
  if( ok )
    return(T);
  else
    return(F);
} /* gr_draw() */

static pointer gr_move()
{
  pointer     S;
  float   xf, yf;
  float   sf;
  int     ok;

  S = car(Args);
  
  if( !(isnumber(S)) )
    Error("move -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(S) )
    sf = (float)(ivalue(S));
  else if(isrational(S))
    sf = (float)((double)qnvalue(S)/(double)qdvalue(S));
  else
    sf = rvalue(S);
  ok = 1;
  xf = gr_x + ( COS( gr_dir ) * sf );
  yf = gr_y + ( SIN( gr_dir ) * sf );
  
  if( (int)xf > gr_max_x ) {
    xf = (float)gr_max_x;
    ok = 0;
  }
  else if( xf < 0.0 ) {
    xf = 0.0;
    ok = 0;
  }
  if( (int)yf > gr_max_y ) {
    yf = (float)gr_max_y;
    ok = 0;
  }
  else if( yf < 0.0 ) {
    yf = 0.0;
    ok = 0;
  }
  gr_x = xf;
  gr_y = yf;
  if( ok )
    return(T);
  else
    return(F);
} /* gr_move() */

static pointer gr_drawto()
{
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
#endif
  pointer     x, y;
  int     xi, yi;
  
  x = car(Args);
  y = cadr(Args);
  
  if( !(isnumber(x) && isnumber(y)) )
    Error("draw-to -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(x) )
    xi = (int)(ivalue(x));
  else if(isrational(x))
    xi = (int)((double)qnvalue(x)/(double)qdvalue(x));
  else
    xi = (int)(rvalue(x));
  
  if( isinteger(y) )
    yi = (int)(ivalue(y));
  else if(isrational(y))
    yi = (int)((double)qnvalue(y)/(double)qdvalue(y));
  else
    yi = (int)(rvalue(y));
  
  if( !valid_XYC( xi,yi, 0 ) )
    return(F);
  
#ifdef X11
  XDrawLine( gr_display, gr_win, gr_gc,
	    (int)gr_x,(int)gr_y, xi,yi );
  gr_events(0);
#endif
#ifdef Macintosh
  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  RGBForeColor(currentColor);
  MoveTo ((int)gr_x,(int)gr_y);
  LineTo (xi,yi);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetPort(myGraphicsWindow);
  RGBForeColor(currentColor);
  MoveTo ((int)gr_x,(int)gr_y);
  LineTo (xi,yi);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetGDevice(saveghd);
  SetPort(save);
#endif
  return(T);
} /* gr_drawto() */

static pointer gr_drawTo()
{
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
#endif
  pointer     x, y;
  float   xf, yf;

  x = car(Args);
  y = cadr(Args);
  
  if( !(isnumber(x) && isnumber(y)) )
    Error("draw-to! -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(x) )
    xf = (float)(ivalue(x));
  else if(isrational(x))
    xf = (float)((double)qnvalue(x)/(double)qdvalue(x));
  else
    xf = (float)(rvalue(x));
  
  if( isinteger(y) )
    yf = (float)(ivalue(y));
  else if(isrational(y))
    yf = (float)((double)qnvalue(y)/(double)qdvalue(y));
  else
    yf = (float)(rvalue(y));
  
  if( !valid_XYC( (int)xf,(int)yf, 0 ) )
    return(F);
#ifdef X11
  XDrawLine( gr_display, gr_win, gr_gc,
	    (int)gr_x,(int)gr_y,
	    (int)xf,(int)yf );
  gr_events(0);
#endif
#ifdef Macintosh
  GetPort(&save);
  saveghd=GetGDevice();
  SetGDevice(maxghd);
  SetPort(backupPortPtr);
  RGBForeColor(currentColor);
  MoveTo ((int)gr_x,(int)gr_y);
  LineTo ((int)xf,(int)yf);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetPort(myGraphicsWindow);
  RGBForeColor(currentColor);
  MoveTo ((int)gr_x,(int)gr_y);
  LineTo ((int)xf,(int)yf);
  RGBForeColor(&gr_colortbl[ 0 ]);
  SetGDevice(saveghd);
  SetPort(save);
#endif
  gr_x = xf;
  gr_y = yf;
  return(T);
} /* gr_drawTo() */

static pointer gr_moveTo()
{
  pointer     x, y;
  float   xf, yf;

  x = car(Args);
  y = cadr(Args);
  
  if( !(isnumber(x) && isnumber(y)) )
    Error("move-to! -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(x) )
    xf = (float)(ivalue(x));
  else if(isrational(x))
    xf = (float)((double)qnvalue(x)/(double)qdvalue(x));
  else
    xf = (float)(rvalue(x));
  
  if( isinteger(y) )
    yf = (float)(ivalue(y));
  else if(isrational(y))
    yf = (float)((double)qnvalue(y)/(double)qdvalue(y));
  else
    yf = (float)(rvalue(y));

  if( !valid_XYC( (int)xf,(int)yf, 0 ) )
    return(F);
  gr_x = xf;
  gr_y = yf;
  return(T);
} /* gr_moveTo() */

static pointer gr_setcolor()
{
  pointer     c;
  int     color;

  c = car(Args);
  
  if( !(isnumber(c)) )
    Error("set-color! -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(c) )
    color = (int)(ivalue(c));
  else if(isrational(c))
    color = (int)((double)qnvalue(c)/(double)qdvalue(c));
  else
    color = (int)(rvalue(c));
  if( !valid_XYC( 0,0, color ) )
    return(F);
  gr_color = color;
  
#ifdef X11
  /* Set the drawing color */
  XSetForeground( gr_display, gr_gc, gr_colortbl[ gr_color ] );
  gr_events(0);
#endif
#ifdef Macintosh
  currentColor = &gr_colortbl[ gr_color ];
#endif
  return(T);
} /* gr_setcolor() */

static pointer gr_turnright()
{
  pointer     d;
  float   df;

  d = car(Args);
  
  if( !(isnumber(d)) )
    Error("turn-right -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(d) )
    df = (float)(ivalue(d));
  else if(isrational(d))
    df = (float)((double)qnvalue(d)/(double)qdvalue(d));
  else
    df = rvalue(d);
  df = fmod( df, 360.0 );
  gr_dir -= df;
  gr_dir = fmod( gr_dir, 360.0 );
  return(mk_integer( (long)(gr_dir+.5) ));
} /* gr_turnright() */

static pointer gr_turnleft()
{
  pointer     d;
  float   df;

  d = car(Args);
  
  if( !(isnumber(d)) )
    Error("turn-left -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(d) )
    df = (float)(ivalue(d));
  else if(isrational(d))
    df = (float)((double)qnvalue(d)/(double)qdvalue(d));
  else
    df = rvalue(d);
  df = fmod( df, 360.0 );
  gr_dir += df;
  gr_dir = fmod( gr_dir, 360.0 );
  return(mk_integer( (long)(gr_dir+.5) ));
} /* gr_turnleft() */

static pointer gr_turnto()
{
  pointer     d;
  float   df;

  d = car(Args);
  
  if( !(isnumber(d)) )
    Error("turn-to! -- argument must be number", NIL);
  
  if( !gr_grmode_on )
    return(F);
  if( isinteger(d) )
    df = (float)(ivalue(d));
  else if(isrational(d))
    df = (float)((double)qnvalue(d)/(double)qdvalue(d));
  else
    df = rvalue(d);
  df = fmod( df, 360.0 );
  gr_dir = df;
  return(mk_integer( (long)(gr_dir+.5) ));
} /* gr_turnto() */

static pointer gr_gotohome()
{
  if( !gr_grmode_on )
    return(F);
  gr_x = gr_y = 0.0;
  return(T);
} /* gr_gotohome() */

static pointer gr_gotocenter()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  gr_x = ((float)gr_max_x+1.0) / 2.0;
  gr_y = ((float)gr_max_y+1.0) / 2.0;
  return(T);
} /* gr_gotocenter() */

static pointer gr_gotonw()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  gr_x = 0.0;
  gr_y = 0.0;
  return(T);
} /* gr_gotonw() */

static pointer gr_gotosw()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  gr_x = 0.0;
  gr_y = (float)gr_max_y;
  return(T);
} /* gr_gotosw() */

static pointer gr_gotone()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  gr_x = (float)gr_max_x;
  gr_y = 0.0;
  return(T);
} /* gr_gotone() */

static pointer gr_gotose()
{
  if( !gr_grmode_on )
    return(F);
#ifdef X11
  /* Check for changed window size */
  gr_events(0);
#endif
  gr_x = (float)gr_max_x;
  gr_y = (float)gr_max_y;
  return(T);
} /* gr_gotose() */

static pointer gr_whatcolor()
{
  if( !gr_grmode_on )
    return(F);
  return(mk_integer( (long)gr_color ));
} /* gr_whatcolor() */

static pointer gr_whatdirection()
{
  if( !gr_grmode_on )
    return(F);
  return(mk_integer( (long)(gr_dir+.5) ));
} /* gr_whatdirection() */

static pointer gr_wherex()
{
  if( !gr_grmode_on )
    return(F);
  return(mk_integer( (long)gr_x ));
} /* gr_wherex() */

static pointer gr_wherey()
{
  if( !gr_grmode_on )
    return(F);
  return(mk_integer( (long)gr_y ));
} /* gr_wherey() */

#ifdef __STDC__
void close_turtlegr()
{
#ifdef X11
  gr_events(0);
  XFreeColors( gr_display, DefaultColormap(gr_display,gr_screen),
	      gr_colortbl, GR_COLORS, AllPlanes );
  XFreeGC( gr_display, gr_gc );
  XUnmapWindow( gr_display, gr_win );
  XDestroyWindow( gr_display, gr_win );
#endif
} /* close_turtlegr() */
#endif

void init_turtlegr() /* detects if graphics is available; must be
                        called among program initializations */
{
#ifdef X11
  char                  *display_name = NULL;   /* Server to connect to      */
  Pixmap                icon_pixmap;            /* Icon                      */
  XSizeHints            size_hints;             /* Preferred sizes           */
  XSetWindowAttributes  win_attribs;            /* Window attributes         */
  XWMHints              wm_hints;               /* Window manager hints      */
  XClassHint            class_hints;            /* Class hints               */
  XTextProperty         window_name, icon_name; /* Names for Icon & Window   */
  XGCValues             gc_values;              /* Graphics Context values   */
  static char           *colorname[GR_COLORS] = {
    GR_COLOR00, GR_COLOR01, GR_COLOR02, GR_COLOR03,
    GR_COLOR04, GR_COLOR05, GR_COLOR06, GR_COLOR07,
    GR_COLOR08, GR_COLOR09, GR_COLOR10, GR_COLOR11,
    GR_COLOR12, GR_COLOR13, GR_COLOR14, GR_COLOR15
    };
  XColor                x_color;                /* X11 Color structure       */
  unsigned long         mask;                   /* Mask for selections       */
  int                   i;                      /* loop counter variable     */
#endif
#ifdef Macintosh
  WindowPtr save;
  GDHandle saveghd;
#endif
  
  /***************************/
  /* generic initializations */
  /***************************/
  gr_x = gr_y = gr_dir = 0.0;
  gr_max_x = gr_max_y = gr_max_color = 0;
  
  gr_graphicsavail = 0; /* DEFAULT is no graphics - you can do without */
  
  /********************************************/
  /*****  Initialize X11 turtlegraphics   *****/
  /********************************************/
#ifdef X11  
  /* connect to X server */
  if( (gr_display = XOpenDisplay(display_name)) != NULL )
    {
      
      /*****************************/
      /* connection to X server OK */
      /*****************************/
      
      gr_screen = DefaultScreen( gr_display );      /* X screen number */
      
      /* Create a window with Black background and border */
      gr_win
	= XCreateSimpleWindow( gr_display,
			      RootWindow( gr_display, gr_screen),
			      0, 0, /* initial placement */
			      GR_DEF_XSIZE, GR_DEF_YSIZE,
			      3, /* border width */
			      /* border pixel value */
			      BlackPixel(gr_display,gr_screen),
			      /* background pixel value */
			      BlackPixel(gr_display,gr_screen) );
      
      /* Select input (events) for the window */
      XSelectInput( gr_display, gr_win,
		   StructureNotifyMask|ExposureMask );
      
      /* Check for backing store capability */
      if( !DoesBackingStore(DefaultScreenOfDisplay(gr_display)) )
	{
	  fprintf( stderr, "%s: Warning: \
X server does not offer backing store capability.\n\
Window cannot be redrawn if obscured. Sorry...\n", gr_progname );
	}
      else
	{
	  /* Enable the backing store feature of X server
	     and set bit gravity */
	  win_attribs.bit_gravity   = NorthWestGravity;
	  win_attribs.backing_store = Always;
	  mask                      = CWBitGravity | CWBackingStore;
	  XChangeWindowAttributes( gr_display, gr_win, mask, &win_attribs );
	}
      
      /* Make names of Window and Icon for window manager */
      if( XStringListToTextProperty(&gr_windowname,1,&window_name) == 0 ) {
	(void)fprintf( stderr, "%s: Structure allocation for windowName\
 failed.\n", gr_progname );
	exit( 42 );
      }
      if( XStringListToTextProperty(&gr_iconname,1,&icon_name) == 0 ) {
	(void)fprintf( stderr, "%s: Structure allocation for iconName\
 failed.\n", gr_progname );
	exit( 42 );
      }
      
      /* Create the icon */
      icon_pixmap = XCreateBitmapFromData( gr_display, gr_win, turtle_bits,
					  turtle_width, turtle_height );
      
      /* Window size, state, icon etc. hints for the window manager */
      size_hints.flags = PPosition | PMaxSize | PMinSize | USSize;
      /* position and desired size are given to XCreateSimpleWindow call */
      size_hints.min_width          = GR_MIN_XSIZE;
      size_hints.min_height         = GR_MIN_YSIZE;
      size_hints.max_width          = GR_MAX_XSIZE;
      size_hints.max_height         = GR_MAX_YSIZE;
      wm_hints.flags = StateHint | IconPixmapHint | InputHint;
      wm_hints.initial_state        = NormalState;
      wm_hints.input                = False;
      wm_hints.icon_pixmap          = icon_pixmap;
      class_hints.res_name          = gr_progname;
      class_hints.res_class         = gr_classname;
      XSetWMProperties( gr_display, gr_win, &window_name, &icon_name,
		       gr_argv, gr_argc,
		       &size_hints, &wm_hints, &class_hints );
      
      
      /* Handle colors; this is quite complicated in X11 */
      
      if( DefaultDepth( gr_display, gr_screen ) == 1 )
	{
	  /* Only 1 bitplane, BW screen */
	  /* Emulate colors with 0 as Black and 1-15 White */
	  gr_colortbl[0] = BlackPixel( gr_display, gr_screen );
	  for( i = 1; i < GR_COLORS; ++i )
	    gr_colortbl[i] = WhitePixel( gr_display, gr_screen );
	}
      else
	{
	  /* more than 1 bitplane */
	  for( i = 0; i < GR_COLORS; ++i )
	    {
	      /* Initialize the colortable using named colors */
	      if( XParseColor( gr_display,
			      DefaultColormap(gr_display,gr_screen),
			      colorname[ i ], &x_color ) )
		{
		  if( !XAllocColor( gr_display,
				   DefaultColormap(gr_display,gr_screen),
				   &x_color ) )
		    {
		      fprintf( stderr, "%s: Can't allocate color \
\"%s\" (%d). Substituting White.\n",
			      gr_progname,
			      colorname[ i ], i );
		      gr_colortbl[i] = WhitePixel( gr_display, gr_screen );
		    }
		  else
		    {
		      /* succeeded in allocating color */
		      gr_colortbl[ i ] = x_color.pixel;
		    }
		}
	      else
		{
		  /* could not parse color */
		  fprintf( stderr,
			  "%s: Color name \"%s\" (%d) not in database. \
Substituting White.\n",
			  gr_progname, colorname[i], i );
		  gr_colortbl[i] = WhitePixel( gr_display, gr_screen );
		}
	    } /* for */
	} /* else */
      gr_max_color = GR_COLORS - 1;
      
      /* Create and initialize a default GC */
      gr_gc = XCreateGC( gr_display, gr_win, 0L, &gc_values );
      
      /* Initialize the drawing color, default's black */
      XSetForeground( gr_display, gr_gc, gr_colortbl[ 0 ] );
      XSetBackground( gr_display, gr_gc, gr_colortbl[ 0 ] );
      gr_color = 0;
      
      /* OK, we _do_ have graphics available */
      gr_graphicsavail = 1;
      
#ifdef __STDC__
      /* Let's do the Right Thing if possible :) */
      atexit( close_turtlegr );
#endif
    } /* if */
  else {
    gr_graphicsavail = 0;
  }
#endif
/***************************************************/
/*****  Initialize Macintosh turtlegraphics    *****/
/***************************************************/
#ifdef Macintosh
  if( gr_graphicsavail = !makeGraphicsWindow() ) {
    currentColor = &gr_colortbl[ 0 ];
    gr_color = 0;
//  gr_max_color = GR_COLORS - 1;
    gr_x = gr_y = 0.0;
    gr_max_x=grbox.right;
    gr_max_y=grbox.bottom;
    GetPort(&save);
    saveghd=GetGDevice();
    SetGDevice(maxghd);
    SetPort(backupPortPtr);
    RGBForeColor(currentColor);
    MoveTo (0, 0);
    SetPort(myGraphicsWindow);
    RGBForeColor(currentColor);
    MoveTo (0, 0);
    SetGDevice(saveghd);
    SetPort(save);
  }
#endif
  
  gr_grmode_on = 0;
  
  puts( "\nScheme Turtlegraphics Copyright (C) 1992 sjm@cc.tut.fi, jtl@cc.tut.fi\n\
Type `(help-gr)' or `(help-turtlegr)' for a quick reference of\n\
the new primitives.\n" );
  
  if( !gr_graphicsavail ) {
#ifdef X11
    fprintf( stderr, "%s: No X server found. \
Turtlegraphics not available.\n\n", gr_progname );
#endif
#ifdef Macintosh
    fputs( "No Color Graphics Port detected. Turtlegraphics not available.\n\
 ( The direct color mode is Not supported!! )\n\n", stderr );
#endif
  }
  else {
#ifdef X11
    gr_events(0);
#else
    ;
#endif
  }
} /* init_turtlegr() */


/************************************************************************/
/************************************************************************/
#else /*********************  ORIGINAL (for scm) ************************/
/************************************************************************/
/************************************************************************/


/****************************************************/
/*****  GENERIC includes & defines              *****/
/****************************************************/
#include        "scm.h"         /* includes scmfig.h as well    */
#include        "patchlvl.h"    /* Guess...                     */
#include        <math.h>        /* sin(), cos(), fmod()         */
#include	<stdlib.h>	/* atexit()			*/

/****************************************************/
/*****  X11 specific includes & defines         *****/
/****************************************************/
#ifdef X11

/* Xlib include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <stdio.h>

#include "turtle"
#define BITMAPDEPTH 1

#define PROGNAME        "scm"
#define CLASSNAME       "Scm"
#define WINDOWNAME      "TurtleSCM graphics window"
#define ICONNAME        "TurtleSCM"

#define GR_MAX_XSIZE    1024
#define GR_MAX_YSIZE    1024
#define GR_DEF_XSIZE    640
#define GR_DEF_YSIZE    480
#define GR_MIN_XSIZE    64
#define GR_MIN_YSIZE    64

/* Fake CGA colormap with X - yuk!                              */
#define GR_COLORS       16              /* CGA/EGA counterpart  */
#define GR_COLOR00      "black"         /* black                */
#define GR_COLOR01      "blue2"         /* blue                 */
#define GR_COLOR02      "green2"        /* green                */
#define GR_COLOR03      "cyan2"         /* cyan                 */
#define GR_COLOR04      "red3"          /* red                  */
#define GR_COLOR05      "magenta2"      /* magenta              */
#define GR_COLOR06      "yellow2"       /* brown                */
#define GR_COLOR07      "light gray"    /* white                */
#define GR_COLOR08      "gray"          /* gray                 */
#define GR_COLOR09      "blue1"         /* light blue           */
#define GR_COLOR10      "green1"        /* light green          */
#define GR_COLOR11      "cyan1"         /* light cyan           */
#define GR_COLOR12      "red1"          /* light red            */
#define GR_COLOR13      "magenta1"      /* light magenta        */
#define GR_COLOR14      "yellow1"       /* yellow               */
#define GR_COLOR15      "white"         /* bright white         */

#ifdef  __STDC__
static void     gr_events( int );
#else
static void     gr_events();
#endif

#else
/****************************************************/
/*****  PC specific includes & defines          *****/
/****************************************************/
#include        <graphics.h>
#include        <stdlib.h>      /* for getenv()                 */
#include        <stdio.h>       /* for fputs()                  */
#define         BGIDIR_ENVSTRING        "BGIDIR"
#endif

/********************************************/
/***** GENERIC code, declarations       *****/
/********************************************/
#define         SIN( x )                \
                sin( ((x)/180.0) * M_PI )
#define         COS( x )                \
                cos( ((x)/180.0) * M_PI )

static  int             gr_graphicsavail = 0;
static  int             gr_grmode_on = 0;
static  float           gr_dir = 0.0;
static  int             gr_max_x=0, gr_max_y=0, gr_max_color=0;
static  float           gr_x=0.0, gr_y=0.0;
static  int             gr_color = 0;

static  char    s_gr_draw[]             = "draw";
static  char    s_gr_move[]             = "move";
static  char    s_gr_setcolor[]         = "set-color!";
static  char    s_gr_turnright[]        = "turn-right";
static  char    s_gr_turnleft[]         = "turn-left";
static  char    s_gr_turnto[]           = "turn-to!";

static  char    s_gr_getdot[]           = "get-dot";
static  char    s_gr_drawTo[]           = "draw-to!";
static  char    s_gr_drawto[]           = "draw-to";
static  char    s_gr_moveTo[]           = "move-to!";

static  char    s_gr_setdot[]           = "set-dot!";
static  char    s_gr_validXYC[]         = "valid-xyc?";

#ifdef __GNUC__
inline
#else
static
#endif
int      valid_XYC( x, y, color )
int     x, y, color;
{
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        if( (x <= gr_max_x) && (y <= gr_max_y) && (color <= gr_max_color)
            && (x >= 0) && (y >= 0) && (color >= 0) )
                return( 1 );
        else
                return( 0 );
} /* valid_XYC() */


/********************************************************************/
/*****  X11 specific variable and function declarations         *****/
/********************************************************************/
#ifdef X11
static Display          *gr_display;            /* The X display        */
static int              gr_screen;              /* The X screen number  */
static Window           gr_win;                 /* The drawable Window  */
static GC               gr_gc;                  /* Graphics Context     */
static unsigned long    gr_colortbl[GR_COLORS]; /* Color table          */
static XEvent           gr_event;               /* Event structure      */

/* These are needed for XSetWMProperties */
static char     *gr_windowname  = WINDOWNAME;
static char     *gr_iconname    = ICONNAME;
static char     gr_progname[]   = PROGNAME;
static char     gr_classname[]  = CLASSNAME;
static int      gr_argc         = 1;
static char     *gr_argv[]      = { gr_progname, NULL };

static void     gr_eventhandler( event )
XEvent          event;
{
          switch( event.type ) {

          case ConfigureNotify:
#ifdef TESTING
            fputs( "Received ConfigureNotify event\n", stderr );
#endif
            gr_max_x = event.xconfigure.width - 1;
            gr_max_y = event.xconfigure.height - 1;
            break;

          case MapNotify:
#ifdef TESTING
            fputs( "Received MapNotify event\n", stderr );
#endif
            break;

          case DestroyNotify:
#ifdef TESTING
            fputs( "Received DestroyNotify event\n", stderr );
#endif
            break;

          case UnmapNotify:
#ifdef TESTING
            fputs( "Received UnmapNotify event\n", stderr );
#endif
            break;

          case Expose:
#ifdef TESTING
            fputs( "Received Expose event\n", stderr );
#endif
            if( event.xexpose.count != 0 )
              break;
            break;

          case ClientMessage:
#ifdef TESTING
            fputs( "Received ClientMessage event\n", stderr );
#endif
            break;

          default:
            /* Throw away any unknown events */
            break;

          } /* switch */
}

static void     gr_events( expected )
int             expected;
{
int             i;

        /* Get at least 'expected' events */
        for( i = 0; i < expected; ++i ) {
          XNextEvent( gr_display, &gr_event );
          gr_eventhandler( gr_event );
        }
        /* Handle all remaining events if there are any */
        /* XPending will call XFlush() if it doesn't find events at once */
        while( XPending(gr_display) ) {
          XNextEvent( gr_display, &gr_event );
          gr_eventhandler( gr_event );
        } /* while */
} /* gr_events() */

static void     gr_typedevent( type )
int             type;
{
        do {
          XNextEvent( gr_display, &gr_event );
          gr_eventhandler( gr_event );
        } while( gr_event.type != type );
        /* Handle all remaining events if there are any */
        /* XPending will call XFlush() if it doesn't find events at once */
        while( XPending(gr_display) ) {
          XNextEvent( gr_display, &gr_event );
          gr_eventhandler( gr_event );
        } /* while */
}


/********************************************************************/
/*****  PC specific variable and function declarations          *****/
/********************************************************************/
#else

static  int             gr_max_display_mode;
static  int             gr_drivernum;

#endif


/********************************************************************/
/********************************************************************/
/***    User callable SCM routines begin here           ***
 ***                                                    ***
 ***                                                    ***/


SCM     gr_helpgr()
{
        fputs( "\
Ret   Name               nargs    args        returns\n\
---------------------------------------------------------\n\
B  graphics-avail?         0       -          #t if graphics available\n\
B  graphics-mode!          0       -          #f if no graphics\n\
B  text-mode!              0       -          #t on success\n\
B  clear-graphics!         0       -          #f if not in graphics mode\n\
i  max-x                   0       -          maximum value of x\n\
i  max-y                   0       -          maximum value of y\n\
i  max-color               0       -          maximum value of color\n\
B  valid-xyc?              3       x y color  #t if valid\n\
B  set-dot!                3       x y color  #t on success\n\
i  get-dot                 2       x y        color of the dot in (x,y)\n\
                                              or #f if (x,y) not legal\n\
\n\
NOTE: Origin (0,0) is in the upper left corner.\n\n\
", stdout );
        return  BOOL_T;
} /* gr_helpgr() */


SCM     gr_helpturtlegr()
{
        fputs( "\
Ret   Name               nargs    args        returns\n\
---------------------------------------------------------\n\
B  goto-home!              0       -          #f if not in graphics mode\n\
B  goto-center!            0       -          #f if not in graphics mode\n\
B  goto-nw!                0       -          #f if not in graphics mode\n\
B  goto-ne!                0       -          #f if not in graphics mode\n\
B  goto-sw!                0       -          #f if not in graphics mode\n\
B  goto-se!                0       -          #f if not in graphics mode\n\
B  draw                    1       length     #t if target within drawing area\n\
B  draw-to                 2       x y        #t if (x,y) within drawing area\n\
B  draw-to!                2       x y        #t if (x,y) within drawing area\n\
B  move                    1       length     #t if target within drawing area\n\
B  move-to!                2       x y        #t if (x,y) within drawing area\n\
i  where-x                 0       -          current x-coordinate\n\
i  where-y                 0       -          current y-coordinate\n\
i  turn-right              1       angle      drawing direction in degrees\n\
i  turn-left               1       angle      drawing direction in degrees\n\
i  turn-to!                1       angle      drawing direction in degrees\n\
i  what-direction          0       -          drawing direction in degrees\n\
B  set-color!              1       color      #t if color valid\n\
i  what-color              0       -          current drawing color\n\n\
", stdout );
        return  BOOL_T;
} /* gr_helpturtlegr() */


SCM     gr_available()
{
        if( gr_graphicsavail )
                return  BOOL_T;
        else
                return  BOOL_F;
} /* gr_available() */


SCM     gr_maxx()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        return  MAKINUM( (long)gr_max_x );
} /* gr_maxx() */


SCM     gr_maxy()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        return  MAKINUM( (long)gr_max_y );
} /* gr_maxy() */

SCM     gr_maxc()
{
        if( !gr_grmode_on )
                return  BOOL_F;
        return  MAKINUM( (long)gr_max_color );
} /* gr_maxc() */


SCM     gr_validXYC( x, y, c )
SCM     x, y, c;
{
int     xi, yi, ci;

        ASSERT( NUMBERP(x),x,ARG1,s_gr_validXYC );
        ASSERT( NUMBERP(y),y,ARG2,s_gr_validXYC );
        ASSERT( NUMBERP(c),c,ARG3,s_gr_validXYC );
        if( !gr_grmode_on )
                return  BOOL_F;

        if( INUMP(x) )
                xi = (int)(INUM(x));
        else
                xi = (int)(REALPART(x));

        if( INUMP(y) )
                yi = (int)(INUM(y));
        else
                yi = (int)(REALPART(y));

        if( INUMP(c) )
                ci = (int)(INUM(c));
        else
                ci = (int)(REALPART(c));

/* valid_XYC() calls gr_events() */

        if( valid_XYC( xi, yi, ci ) )
                return  BOOL_T;
        else
                return  BOOL_F;
} /* gr_validXYC() */


SCM     gr_grmode()
{
        if( !gr_graphicsavail )
                return  BOOL_F;
#ifdef  X11
        /* bwuah... but it works :) */
        if( !gr_grmode_on ) {
            XMapWindow( gr_display, gr_win );
            gr_typedevent( MapNotify );
        }
#else   /* PC version */
        setgraphmode( gr_max_display_mode );
#endif
        gr_grmode_on = 1;
        return  BOOL_T;
} /* gr_grmode() */

SCM     gr_txtmode()
{
        if( !gr_graphicsavail )
                return  BOOL_F;
#ifdef  X11
        /* bwuah... but it works :) */
        if( gr_grmode_on ) {
            XUnmapWindow( gr_display, gr_win );
            gr_typedevent( UnmapNotify );
        }
#else   /* PC version */
        restorecrtmode();
#endif
        gr_grmode_on = 0;
        return  BOOL_T;
} /* gr_txtmode() */


SCM     gr_cleargraph()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef  X11
        XClearWindow( gr_display, gr_win );
        gr_events(0);
#else   /* PC version */
        cleardevice();
#endif
        return  BOOL_T;
} /* gr_cleargraph() */


SCM     gr_setdot( x, y, c )
SCM     x, y, c;
{
int     xi, yi, ci;

        ASSERT( NUMBERP(x),x,ARG1,s_gr_setdot );
        ASSERT( NUMBERP(y),y,ARG2,s_gr_setdot );
        ASSERT( NUMBERP(c),c,ARG3,s_gr_setdot );
        if( !gr_grmode_on )
                return  BOOL_F;

        if( INUMP(x) )
                xi = (int)(INUM(x));
        else
                xi = (int)(REALPART(x));

        if( INUMP(y) )
                yi = (int)(INUM(y));
        else
                yi = (int)(REALPART(y));

        if( INUMP(c) )
                ci = (int)(INUM(c));
        else
                ci = (int)(REALPART(c));
#ifdef TESTING
        fprintf( stderr, "set-dot! called (%d,%d,%d)\n", xi, yi, ci );
#endif
        if( !valid_XYC( xi, yi, ci ) )
                return  BOOL_F;
#ifdef  X11
        /* Set the drawing color */
        XSetForeground( gr_display, gr_gc, gr_colortbl[ ci ] );
        XDrawPoint( gr_display, gr_win, gr_gc, xi, yi );
        /* Restore the drawing color */
        XSetForeground( gr_display, gr_gc, gr_colortbl[ gr_color ] );
        gr_events(0);
#else   /* PC version */
        putpixel( xi, yi, ci );
#endif
        return  BOOL_T;
} /* gr_setdot() */


SCM     gr_getdot( x, y )
SCM     x, y;
{
int                     xi, yi;
#ifdef  X11
XImage                  *xim;
XWindowAttributes       wattr;
unsigned long           dot;
int                     i;
#endif
        ASSERT( NUMBERP(x),x,ARG1,s_gr_getdot );
        ASSERT( NUMBERP(y),y,ARG2,s_gr_getdot );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(x) )
                xi = (int)(INUM(x));
        else
                xi = (int)(REALPART(x));

        if( INUMP(y) )
                yi = (int)(INUM(y));
        else
                yi = (int)(REALPART(y));
#ifdef TESTING
        fprintf( stderr, "get-dot called (%d,%d)\n", xi, yi );
#endif
        if( !valid_XYC( xi, yi, 0 ) )
                return  BOOL_F;
#ifdef  X11
        /* Now, this IS ugly. But it's there if you need it.            */

        /* Have to make sure that the window is mapped.  Tough...       */
        XGetWindowAttributes( gr_display, gr_win, &wattr );
        if( wattr.map_state == IsUnmapped ) {
            XMapWindow( gr_display, gr_win );
            gr_typedevent( MapNotify );
        }
        /* I KNOW this sucks.                                           */
        xim = XGetImage( gr_display,gr_win, xi,yi, 1,1, AllPlanes, XYPixmap );
        dot = XGetPixel( xim, 0,0 );
        for( i = 0; i < GR_COLORS; ++i ) {
            if( gr_colortbl[i] == dot )
                return MAKINUM( (long)i );
        }
        /* This should never happen. There's garbage in the window!     */
        fprintf( stderr, "%s: %s: Got an illegal pixel value %lu. \
Is there garbage?\n", gr_progname, s_gr_getdot, dot );
        return BOOL_F;
#else   /* PC version */
        return MAKINUM( (long)getpixel( xi, yi ) );
#endif
} /* gr_getdot() */

SCM     gr_draw( S )
SCM     S;
{
float   xf, yf;
float   sf;
int     ok;

        ASSERT( NUMBERP(S),S,ARG1,s_gr_draw );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(S) )
                sf = (float)(INUM(S));
        else
                sf = REALPART(S);
#ifdef TESTING
        fprintf( stderr, "draw called (%f)\n", sf );
#endif
        ok = 1;
        xf = gr_x + ( COS( gr_dir ) * sf );
        yf = gr_y + ( SIN( gr_dir ) * sf );
        if( (int)xf > gr_max_x ) {
                xf = (float)gr_max_x;
                ok = 0;
        }
        else if( xf < 0.0 ) {
                xf = 0.0;
                ok = 0;
        }
        if( (int)yf > gr_max_y ) {
                yf = (float)gr_max_y;
                ok = 0;
        }
        else if( yf < 0.0 ) {
                yf = 0.0;
                ok = 0;
        }
#ifdef  X11
        XDrawLine( gr_display, gr_win, gr_gc,
                   (int)gr_x,(int)gr_y,
                   (int)xf,(int)yf );
        gr_events(0);
#else   /* PC version */
        line( (int)gr_x,(int)gr_y, (int)xf,(int)yf );
#endif
        gr_x = xf;
        gr_y = yf;
        if( ok )
                return  BOOL_T;
        else
                return  BOOL_F;
} /* gr_draw() */


SCM     gr_move( S )
SCM     S;
{
float   xf, yf;
float   sf;
int     ok;

        ASSERT( NUMBERP(S),S,ARG1,s_gr_move );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(S) )
                sf = (float)(INUM(S));
        else
                sf = REALPART(S);
#ifdef TESTING
        fprintf( stderr, "move called (%f)\n", sf );
#endif
        ok = 1;
        xf = gr_x + ( COS( gr_dir ) * sf );
        yf = gr_y + ( SIN( gr_dir ) * sf );

        if( (int)xf > gr_max_x ) {
                xf = (float)gr_max_x;
                ok = 0;
        }
        else if( xf < 0.0 ) {
                xf = 0.0;
                ok = 0;
        }
        if( (int)yf > gr_max_y ) {
                yf = (float)gr_max_y;
                ok = 0;
        }
        else if( yf < 0.0 ) {
                yf = 0.0;
                ok = 0;
        }
        gr_x = xf;
        gr_y = yf;
        if( ok )
                return  BOOL_T;
        else
                return  BOOL_F;
} /* gr_move() */


SCM     gr_drawto( x, y )
SCM     x, y;
{
int     xi, yi;

        ASSERT( NUMBERP(x),x,ARG1,s_gr_drawto );
        ASSERT( NUMBERP(y),y,ARG2,s_gr_drawto );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(x) )
                xi = (int)(INUM(x));
        else
                xi = (int)(REALPART(x));

        if( INUMP(y) )
                yi = (int)(INUM(y));
        else
                yi = (int)(REALPART(y));
#ifdef TESTING
        fprintf( stderr, "draw-to called (%d,%d)\n", xi, yi );
#endif
        if( !valid_XYC( xi,yi, 0 ) )
                return  BOOL_F;
#ifdef  X11
        XDrawLine( gr_display, gr_win, gr_gc,
                   (int)gr_x,(int)gr_y, xi,yi );
        gr_events(0);
#else   /* PC version */
        line( (int)gr_x,(int)gr_y, xi,yi );
#endif
        return  BOOL_T;
} /* gr_drawto() */


SCM     gr_drawTo( x, y )
SCM     x, y;
{
float   xf, yf;

        ASSERT( NUMBERP(x),x,ARG1,s_gr_drawTo );
        ASSERT( NUMBERP(y),y,ARG2,s_gr_drawTo );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(x) )
                xf = (float)(INUM(x));
        else
                xf = (REALPART(x));

        if( INUMP(y) )
                yf = (float)(INUM(y));
        else
                yf = (REALPART(y));
#ifdef TESTING
        fprintf( stderr, "draw-to! called (%d,%d)\n", (int)xf, (int)yf );
#endif
        if( !valid_XYC( (int)xf,(int)yf, 0 ) )
                return  BOOL_F;
#ifdef  X11
        XDrawLine( gr_display, gr_win, gr_gc,
                   (int)gr_x,(int)gr_y,
                   (int)xf,(int)yf );
        gr_events(0);
#else   /* PC version */
        line( (int)gr_x,(int)gr_y, (int)xf,(int)yf );
#endif
        gr_x = xf;
        gr_y = yf;
        return  BOOL_T;
} /* gr_drawTo() */


SCM     gr_moveTo( x, y )
SCM     x, y;
{
float   xf, yf;

        ASSERT( NUMBERP(x),x,ARG1,s_gr_moveTo );
        ASSERT( NUMBERP(y),y,ARG2,s_gr_moveTo );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(x) )
                xf = (float)(INUM(x));
        else
                xf = (REALPART(x));

        if( INUMP(y) )
                yf = (float)(INUM(y));
        else
                yf = (REALPART(y));
#ifdef TESTING
        fprintf( stderr, "move-to! called (%d,%d)\n", (int)xf, (int)yf );
#endif
        if( !valid_XYC( (int)xf,(int)yf, 0 ) )
                return  BOOL_F;
        gr_x = xf;
        gr_y = yf;
        return  BOOL_T;
} /* gr_moveTo() */


SCM     gr_setcolor( c )
SCM     c;
{
int     color;

        ASSERT( NUMBERP(c),c,ARG1,s_gr_setcolor );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(c) )
                color = (int)(INUM(c));
        else
                color = (int)(REALPART(c));
#ifdef TESTING
        fprintf( stderr, "set-color! called (%d)\n", color );
#endif
        if( !valid_XYC( 0,0, color ) )
                return  BOOL_F;
        gr_color = color;
#ifdef  X11
        /* Set the drawing color */
        XSetForeground( gr_display, gr_gc, gr_colortbl[ gr_color ] );
        gr_events(0);
#else   /* PC version */
        setcolor( gr_color );
#endif
        return  BOOL_T;
} /* gr_setcolor() */


SCM     gr_turnright( d )
SCM     d;
{
float   df;

        ASSERT( NUMBERP(d),d,ARG1,s_gr_turnright );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(d) )
                df = (float)(INUM(d));
        else
                df = REALPART(d);
        df = fmod( df, 360.0 );
        gr_dir -= df;
        gr_dir = fmod( gr_dir, 360.0 );
        return MAKINUM( (long)(gr_dir+.5) );
} /* gr_turnright() */


SCM     gr_turnleft( d )
SCM     d;
{
float   df;

        ASSERT( NUMBERP(d),d,ARG1,s_gr_turnleft );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(d) )
                df = (float)(INUM(d));
        else
                df = REALPART(d);
        df = fmod( df, 360.0 );
        gr_dir += df;
        gr_dir = fmod( gr_dir, 360.0 );
        return MAKINUM( (long)(gr_dir+.5) );
} /* gr_turnleft() */


SCM     gr_turnto( d )
SCM     d;
{
float   df;

        ASSERT( NUMBERP(d),d,ARG1,s_gr_turnto );
        if( !gr_grmode_on )
                return  BOOL_F;
        if( INUMP(d) )
                df = (float)(INUM(d));
        else
                df = REALPART(d);
        df = fmod( df, 360.0 );
        gr_dir = df;
        return MAKINUM( (long)(gr_dir+.5) );
} /* gr_turnto() */


SCM     gr_gotohome()
{
        if( !gr_grmode_on )
                return  BOOL_F;
        gr_x = gr_y = 0.0;
        return  BOOL_T;
} /* gr_gotohome() */


SCM     gr_gotocenter()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        gr_x = ((float)gr_max_x+1.0) / 2.0;
        gr_y = ((float)gr_max_y+1.0) / 2.0;
        return  BOOL_T;
} /* gr_gotocenter() */


SCM     gr_gotonw()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        gr_x = 0.0;
        gr_y = 0.0;
        return  BOOL_T;
} /* gr_gotonw() */


SCM     gr_gotosw()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        gr_x = 0.0;
        gr_y = (float)gr_max_y;
        return  BOOL_T;
} /* gr_gotosw() */


SCM     gr_gotone()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        gr_x = (float)gr_max_x;
        gr_y = 0.0;
        return  BOOL_T;
} /* gr_gotone() */


SCM     gr_gotose()
{
        if( !gr_grmode_on )
                return  BOOL_F;
#ifdef X11
        /* Check for changed window size */
        gr_events(0);
#endif
        gr_x = (float)gr_max_x;
        gr_y = (float)gr_max_y;
        return  BOOL_T;
} /* gr_gotose() */


SCM     gr_whatcolor()
{
        if( !gr_grmode_on )
                return  BOOL_F;
        return  MAKINUM( (long)gr_color );
} /* gr_whatcolor() */


SCM     gr_whatdirection()
{
        if( !gr_grmode_on )
                return  BOOL_F;
        return  MAKINUM( (long)(gr_dir+.5) );
} /* gr_whatdirection() */


SCM     gr_wherex()
{
        if( !gr_grmode_on )
                return  BOOL_F;
        return  MAKINUM( (long)gr_x );
} /* gr_wherex() */


SCM     gr_wherey()
{
        if( !gr_grmode_on )
                return  BOOL_F;
        return  MAKINUM( (long)gr_y );
} /* gr_wherey() */


static iproc    graph0[] = {
                { "help-gr", gr_helpgr },
                { "help-turtlegr", gr_helpturtlegr },
                { "graphics-mode!", gr_grmode },
                { "text-mode!", gr_txtmode },
                { "clear-graphics!", gr_cleargraph },
                { "graphics-avail?", gr_available },
                { "max-x", gr_maxx },
                { "max-y", gr_maxy },
                { "max-color", gr_maxc },
                { "what-color", gr_whatcolor },
                { "what-direction", gr_whatdirection },
                { "where-x", gr_wherex },
                { "where-y", gr_wherey },
                { "goto-home!", gr_gotohome },
                { "goto-center!", gr_gotocenter },
                { "goto-nw!", gr_gotonw },
                { "goto-sw!", gr_gotosw },
                { "goto-ne!", gr_gotone },
                { "goto-se!", gr_gotose },
                {0,0}
        };

static iproc    graph1[] = {
                { s_gr_draw, gr_draw },
                { s_gr_move, gr_move },
                { s_gr_setcolor, gr_setcolor },
                { s_gr_turnright, gr_turnright },
                { s_gr_turnleft, gr_turnleft },
                { s_gr_turnto, gr_turnto },
                {0,0}
        };

static iproc    graph2[] = {
                { s_gr_getdot, gr_getdot },
                { s_gr_drawTo, gr_drawTo },
                { s_gr_drawto, gr_drawto },
                { s_gr_moveTo, gr_moveTo },
                {0,0}
        };

static iproc    graph3[] = {
                { s_gr_setdot, gr_setdot },
                { s_gr_validXYC, gr_validXYC },
                {0,0}
        };

#if defined __STDC__ || defined __TURBOC__
void    close_turtlegr()
{
#ifdef  X11
        gr_events(0);
        XFreeColors( gr_display, DefaultColormap(gr_display,gr_screen),
                     gr_colortbl, GR_COLORS, AllPlanes );
        XFreeGC( gr_display, gr_gc );
        XUnmapWindow( gr_display, gr_win );
        XDestroyWindow( gr_display, gr_win );
#else   /* PC version */
        closegraph();
#endif
} /* close_turtlegr() */
#endif

void    init_turtlegr() /* detects if graphics is available; must be
                           called among program initializations */
{
#ifdef  X11
  char                  *display_name = NULL;   /* Server to connect to      */
  Pixmap                icon_pixmap;            /* Icon                      */
  XSizeHints            size_hints;             /* Preferred sizes           */
  XSetWindowAttributes  win_attribs;            /* Window attributes         */
  XWMHints              wm_hints;               /* Window manager hints      */
  XClassHint            class_hints;            /* Class hints               */
  XTextProperty         window_name, icon_name; /* Names for Icon & Window   */
  XGCValues             gc_values;              /* Graphics Context values   */
  static char           *colorname[GR_COLORS] = {
                                GR_COLOR00, GR_COLOR01, GR_COLOR02, GR_COLOR03,
                                GR_COLOR04, GR_COLOR05, GR_COLOR06, GR_COLOR07,
                                GR_COLOR08, GR_COLOR09, GR_COLOR10, GR_COLOR11,
                                GR_COLOR12, GR_COLOR13, GR_COLOR14, GR_COLOR15
                        };
  XColor                x_color;                /* X11 Color structure       */
  unsigned long         mask;                   /* Mask for selections       */
  int                   i;                      /* loop counter variable     */

#else   /* PC version */
int     errcode;
#endif

/***************************/
/* generic initializations */
/***************************/
  gr_x = gr_y = gr_dir = 0.0;
  gr_max_x = gr_max_y = gr_max_color = 0;

  gr_graphicsavail = 0; /* DEFAULT is no graphics - you can do without */

/********************************************/
/*****  Initialize X11 turtlegraphics   *****/
/********************************************/
#ifdef X11
        /* connect to X server */
        if( (gr_display = XOpenDisplay(display_name)) != NULL )
        {

          /*****************************/
          /* connection to X server OK */
          /*****************************/

          gr_screen = DefaultScreen( gr_display );      /* X screen number */

          /* Create a window with Black background and border */
          gr_win
            = XCreateSimpleWindow( gr_display,
                                  RootWindow( gr_display, gr_screen),
                                  0, 0, /* initial placement */
                                  GR_DEF_XSIZE, GR_DEF_YSIZE,
                                  3, /* border width */
                                  /* border pixel value */
                                  BlackPixel(gr_display,gr_screen),
                                  /* background pixel value */
                                  BlackPixel(gr_display,gr_screen) );

          /* Select input (events) for the window */
          XSelectInput( gr_display, gr_win,
                        StructureNotifyMask|ExposureMask );

          /* Check for backing store capability */
          if( !DoesBackingStore(DefaultScreenOfDisplay(gr_display)) )
          {
            fprintf( stderr, "%s: Warning: \
X server does not offer backing store capability.\n\
Window cannot be redrawn if obscured. Sorry...\n", gr_progname );
          }
          else
          {
              /* Enable the backing store feature of X server
                 and set bit gravity */
              win_attribs.bit_gravity   = NorthWestGravity;
              win_attribs.backing_store = Always;
              mask                      = CWBitGravity | CWBackingStore;
              XChangeWindowAttributes( gr_display, gr_win, mask, &win_attribs );
          }

          /* Make names of Window and Icon for window manager */
          if( XStringListToTextProperty(&gr_windowname,1,&window_name) == 0 ) {
            (void)fprintf( stderr, "%s: Structure allocation for windowName\
 failed.\n", gr_progname );
            exit( 42 );
          }
          if( XStringListToTextProperty(&gr_iconname,1,&icon_name) == 0 ) {
            (void)fprintf( stderr, "%s: Structure allocation for iconName\
 failed.\n", gr_progname );
            exit( 42 );
          }

          /* Create the icon */
          icon_pixmap = XCreateBitmapFromData( gr_display, gr_win, turtle_bits,
                                               turtle_width, turtle_height );

          /* Window size, state, icon etc. hints for the window manager */
          size_hints.flags = PPosition | PMaxSize | PMinSize | USSize;
          /* position and desired size are given to XCreateSimpleWindow call */
          size_hints.min_width          = GR_MIN_XSIZE;
          size_hints.min_height         = GR_MIN_YSIZE;
          size_hints.max_width          = GR_MAX_XSIZE;
          size_hints.max_height         = GR_MAX_YSIZE;
          wm_hints.flags = StateHint | IconPixmapHint | InputHint;
          wm_hints.initial_state        = NormalState;
          wm_hints.input                = False;
          wm_hints.icon_pixmap          = icon_pixmap;
          class_hints.res_name          = gr_progname;
          class_hints.res_class         = gr_classname;
          XSetWMProperties( gr_display, gr_win, &window_name, &icon_name,
                            gr_argv, gr_argc,
                            &size_hints, &wm_hints, &class_hints );


          /* Handle colors; this is quite complicated in X11 */

          if( DefaultDepth( gr_display, gr_screen ) == 1 )
          {
            /* Only 1 bitplane, BW screen */
            /* Emulate colors with 0 as Black and 1-15 White */
            gr_colortbl[0] = BlackPixel( gr_display, gr_screen );
            for( i = 1; i < GR_COLORS; ++i )
              gr_colortbl[i] = WhitePixel( gr_display, gr_screen );
#ifdef TESTING
            fprintf( stderr, "%s: 1-plane system, substituting White for \
colors 1-15.\n", gr_progname );
            fprintf( stderr, "%s: Pixel value is %lu for Black, \
%lu for White\n", gr_progname, gr_colortbl[0], gr_colortbl[1] );
#endif
          }
          else
          {
              /* more than 1 bitplane */
              for( i = 0; i < GR_COLORS; ++i )
              {
                  /* Initialize the colortable using named colors */
                  if( XParseColor( gr_display,
                                   DefaultColormap(gr_display,gr_screen),
                                   colorname[ i ], &x_color ) )
                  {
                      if( !XAllocColor( gr_display,
                                        DefaultColormap(gr_display,gr_screen),
                                        &x_color ) )
                      {
                          fprintf( stderr, "%s: Can't allocate color \
\"%s\" (%d). Substituting White.\n",
                                   gr_progname,
                                   colorname[ i ], i );
                          gr_colortbl[i] = WhitePixel( gr_display, gr_screen );
                      }
                      else
                      {
                          /* succeeded in allocating color */
                          gr_colortbl[ i ] = x_color.pixel;
#ifdef TESTING
                          fprintf( stderr, "%s: Pixel value is %lu for %s.\n",
                                   gr_progname, gr_colortbl[i], colorname[i] );
#endif
                      }
                  }
                  else
                  {
                      /* could not parse color */
                      fprintf( stderr,
                               "%s: Color name \"%s\" (%d) not in database. \
Substituting White.\n",
                               gr_progname, colorname[i], i );
                      gr_colortbl[i] = WhitePixel( gr_display, gr_screen );
                  }
              } /* for */
          } /* else */
          gr_max_color = GR_COLORS - 1;

          /* Create and initialize a default GC */
          gr_gc = XCreateGC( gr_display, gr_win, 0L, &gc_values );

          /* Initialize the drawing color, default's black */
          XSetForeground( gr_display, gr_gc, gr_colortbl[ 0 ] );
          XSetBackground( gr_display, gr_gc, gr_colortbl[ 0 ] );
          gr_color = 0;

          /* OK, we _do_ have graphics available */
          gr_graphicsavail = 1;

#ifdef __STDC__
          /* Let's do the Right Thing if possible :) */
          atexit( close_turtlegr );
#endif
      } /* if */
      else {
          gr_graphicsavail = 0;
      }
/********************************************/
/*****  Initialize PC turtlegraphics    *****/
/********************************************/
#else   /* PC version */
          gr_drivernum = DETECT;

          detectgraph( &gr_drivernum, &gr_max_display_mode );
          if( gr_drivernum != grNotDetected ) {
              if( !getenv( BGIDIR_ENVSTRING ) )
                 fprintf( stderr,
                        "You really should set the %s environment variable.\n",
                        BGIDIR_ENVSTRING );
              initgraph( &gr_drivernum, &gr_max_display_mode,
                         getenv( BGIDIR_ENVSTRING ) );
              errcode = graphresult();
              if( errcode != grOk ) {
                  fputs( "Graphics error: ", stderr );
                  fputs( grapherrormsg( errcode ), stderr );
                  exit( 1 );
              }
              moveto( 0,0 );
              gr_x = gr_y = 0.0;
              setcolor( 0 );
              gr_color = 0;
              gr_max_x = getmaxx();
              gr_max_y = getmaxy();
              gr_max_color = getmaxcolor();
              gr_max_display_mode = getmaxmode();
              restorecrtmode();
              gr_graphicsavail = 1;
              atexit( close_turtlegr );
          }
          else {
              gr_graphicsavail = 0;
          }
#endif

/* generic */
        init_iprocs( graph0, tc7_subr_0 );
        init_iprocs( graph1, tc7_subr_1 );
        init_iprocs( graph2, tc7_subr_2 );
        init_iprocs( graph3, tc7_subr_3 );
        gr_grmode_on = 0;

#ifndef X11
  /* PC version clears screen so this must be repeated */
  fputs("SCM version ",stdout);
  fputs(SCMVERSION,stdout);
  intprint((long)PATCHLEVEL,10,stdout);
  puts(", Copyright (C) 1990, 1991, 1992, 1993, 1994 Aubrey Jaffer.\n\
SCM comes with ABSOLUTELY NO WARRANTY; for details type `(terms)'.\n\
This is free software, and you are welcome to redistribute it\n\
under certain conditions; type `(terms)' for details.");
#endif

  puts( "\nSCM Turtlegraphics Copyright (C) 1992 sjm@cc.tut.fi, jtl@cc.tut.fi\n\
Type `(help-gr)' or `(help-turtlegr)' for a quick reference of\n\
the new primitives.\n" );

  if( !gr_graphicsavail ) {
#ifdef X11
        fprintf( stderr, "%s: No X server found. \
Turtlegraphics not available.\n", gr_progname );
#else
        fputs( "No graphics adapter detected. \
Turtlegraphics not available.\n", stderr );
#endif
  }
  else {
#ifdef X11
        gr_events(0);
#else
        ;
#endif
  }
} /* init_turtlegr() */

#endif
