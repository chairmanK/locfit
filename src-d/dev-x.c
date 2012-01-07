/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 */

#ifdef XWIN

#include "local.h"

#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern device devwin;
extern plots pl[MAXWIN];
static INT cw, nogr;

static Display *dpy=NULL;
static int scr, dep;
static XSetWindowAttributes	attr;
static unsigned long mask;
static Window			win[MAXWIN];
static XSizeHints		SizeHints;
static XFontStruct              *regfont, *bigfont;
static GC			myGC;
static XGCValues		myGCValues;
static Colormap                 comap;
static XEvent                   event;
static int borwid = 20;

extern struct lfcol mycol[MAXCOLOR];
unsigned long CPix[MAXCOLOR];

void winSetColor(col)
INT col;
{ XSetForeground(dpy,myGC,CPix[col]);
}

void winAddColor(name,r,g,b,p)
char *name;
INT r, g, b, p;
{ XColor rgb;
  if (nogr) return;
  rgb.red   = (r<<8)+r;
  rgb.green = (g<<8)+g;
  rgb.blue  = (b<<8)+b;
  if (XAllocColor(dpy,comap,&rgb))
  { strcpy(mycol[p].name,name);
    mycol[p].n = p;
    mycol[p].r = r;
    mycol[p].g = g;
    mycol[p].b = b;
    CPix[p] = rgb.pixel;
  }
  else
  WARN(("AddColor: Can't add %s",name));
  return;
}

void winClearScreen(col)
INT col;
{ XSetWindowBackground(dpy,win[cw],CPix[col]);
  XClearWindow(dpy,win[cw]);
}

void winDrawLine(i0,j0,i1,j1)
INT i0, j0, i1, j1;
{ XDrawLine(dpy,win[cw],myGC,i0,j0,i1,j1);
}

void winTextDim(f,z,w,h)
INT f, *w, *h;
char *z;
{ *h = (f) ? 20 : 13;
  *w = (f) ? XTextWidth(bigfont,z,strlen(z))
           : XTextWidth(regfont,z,strlen(z));
}

void winDoText(f,i,j,z,ah,av)
INT f, i, j, ah, av;
char *z;
{ INT h, w;
  winTextDim(f,z,&w,&h);
  i -= (ah+1)*w/2;  /* i, j are lower left corner of box */
  j += (av+1)*h/2;
  if (i<0) i = 0;
  if (f)
    XSetFont(dpy,myGC,bigfont->fid);
  else
    XSetFont(dpy,myGC,regfont->fid);
  XDrawString(dpy,win[cw],myGC,i,j,z,strlen(z));
}

void winDrawPoint(i,j,pch)
INT i, j, pch;
{ switch(pch)
  { case 1: /* o */
      XDrawArc(dpy,win[cw],myGC,i-2,j-2,5,5,0,360*64);
      return;
    case 2: /* + */
      XDrawLine(dpy,win[cw],myGC,i-2,j,i+2,j);
      XDrawLine(dpy,win[cw],myGC,i,j-2,i,j+2);
      return;
    case 3: /* X */
      XDrawLine(dpy,win[cw],myGC,i-2,j-2,i+2,j+2);
      XDrawLine(dpy,win[cw],myGC,i+2,j-2,i-2,j+2);
      return;
  }
}

void winDrawPatch(x,y,c1,c2)
INT *x, *y, c1, c2;
{ XPoint xp[5];
  xp[0].x = x[0]; xp[0].y = y[0];
  xp[1].x = x[1]; xp[1].y = y[1];
  xp[2].x = x[2]; xp[2].y = y[2];
  xp[3].x = x[3]; xp[3].y = y[3];
  xp[4].x = x[0]; xp[4].y = y[0];
  winSetColor(c1);
  XFillPolygon(dpy,win[cw],myGC,xp,4,1,CoordModeOrigin);
  winSetColor(c2);
  XDrawLines(dpy,win[cw],myGC,xp,5,CoordModeOrigin);
}

void winwrapup(rd)
INT rd;
{ XFlush(dpy);
}

void getwinsize(h,w)
INT *h, *w;
{ int x, y;
  unsigned int b, d;
  Window ww;
  XGetGeometry(dpy,win[cw],&ww,&x,&y,
    (unsigned int *)w,(unsigned int *)h,&b,&d);
}

INT winmakewin(w,h,wn,rd)
INT *w, *h, wn, rd;
{ char *z, wt[20];
  long em;
  if (nogr) return(0);
  cw = wn;
  if (win[cw]!=(Window)NULL)
  { getwinsize(h,w);
    return(1);
  }
  win[cw] = XCreateWindow(dpy, RootWindow(dpy, scr),
    100, 20, *w, *h, borwid, dep, InputOutput,
    CopyFromParent, mask, &attr);
  SizeHints.flags  = PPosition | PSize;    /* set mask for the hints */
  SizeHints.x      = 100;               /* x position */
  SizeHints.y      = 20;               /* y position */
  SizeHints.width  = *w;                /* width of the window */
  SizeHints.height = *h;               /* height of the window */
  XSetNormalHints (dpy,win[cw],&SizeHints);
  myGC = XCreateGC(dpy,win[cw],(unsigned long) 0,&myGCValues);
  if (myGC == 0)
  { XDestroyWindow(dpy, scr);
    ERROR(("Can't create gc"));
    return(0);
  }
  winSetColor(1);
  sprintf(wt,"Locfit Plot %d",cw);
  XStoreName(dpy,win[cw],wt);
  XMapWindow(dpy,win[cw]);
  winwrapup(rd);
  XSync(dpy,0);
  em = PointerMotionMask | ExposureMask;
  XSelectInput(dpy,win[cw],em);
  XWindowEvent(dpy,win[cw],em,&event);
  return(1);
}
 
void SetWinDev(dev)
device *dev;
{ int i, x, y;
  unsigned int h, w;
  char *z;

/*  we must set functions on graphics device even when there is nogr,
 *  to avoid problems when other parts of the code try to call the
 *  graphics device. Functions that may be called should individually
 *  check nogr.
 */
  dev->AddColor = winAddColor;
  dev->SetColor = winSetColor;
  dev->ClearScreen = winClearScreen;
  dev->TextDim = winTextDim;
  dev->DoText = winDoText;
  dev->DrawPoint = winDrawPoint;
  dev->DrawLine = winDrawLine;
  dev->DrawPatch = winDrawPatch;
  dev->wrapup = winwrapup;
  dev->makewin = winmakewin;
  dev->ticklength = 5;
  h = 400; w = 600;
  dev->defth = h;
  dev->deftw = w;

  if (nogr) return;

  scr = DefaultScreen(dpy);
  dep = DefaultDepth(dpy,scr);
  comap = DefaultColormap(dpy,scr);
  attr.background_pixel = WhitePixel(dpy,scr);
  mask = CWBackPixel;

  z = XGetDefault(dpy,"locfit","regfont");
  regfont = (z==NULL) ? XLoadQueryFont(dpy,"8x13") :
                        XLoadQueryFont(dpy,z);
  z = XGetDefault(dpy,"locfit","bigfont");
  bigfont = (z==NULL) ? XLoadQueryFont(dpy,"10x20") :
                        XLoadQueryFont(dpy,z);
  if (bigfont==NULL) bigfont = regfont;
  if (regfont==NULL) regfont = bigfont;
  if (bigfont==NULL) WARN(("couldn't load any fonts"));

  z = XGetDefault(dpy,"locfit","geometry");
  XParseGeometry(z,&x,&y,&w,&h);

  dev->defth = h;
  dev->deftw = w;
}

void check_window_events()
{ int i;
  if (nogr) return;
  for (i=0; i<MAXWIN; i++)
    if (XCheckWindowEvent(dpy,win[i],ExposureMask,&event))
      plotxwin(&pl[i],&devwin,i,1);
  while (XCheckMaskEvent(dpy,ExposureMask,&event)) {};
}

void set_x_display(display_name)
char *display_name;
{ if (display_name==NULL)
    display_name = XDisplayName(NULL);
  dpy = XOpenDisplay(display_name);

  nogr = 0;
  if (dpy==NULL)
  { WARN(("No graphics, Could not open a connection to X on display %s",
      display_name));
    nogr = 1;
  }
}

#endif
