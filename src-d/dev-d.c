/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 *
 *   Definitions for the ASCII graphics device.
 */

#ifndef XWIN

#include "local.h"

#define MAXW 100
#define MAXH 50

extern device devwin;
static char win[MAXH][MAXW+MAXW+1];
static INT cf, height=0, width=0, nowin=1;

void SetPoint(i,j,c)
INT i, j;
char c;
{ if ((i<0) | (i>=width)) return;
  if ((j<0) | (j>=height)) return;
  win[j][i] = c;
}

void winSetColor(col)
INT col;
{ }

void winAddColor(name,r,g,b,p)
char *name;
INT r, g, b, p;
{ }

void winClearScreen(col)
INT col;
{ INT i, j;
  for (i=0; i<height; i++)
  { for (j=0; j<width; j++)
      win[i][j] = ' ';
    win[i][width] = '\000';
  }
}

void winTextDim(f,z,w,h) /* must do something sensible */
INT f, *w, *h;
char *z;
{ *w = strlen(z);
  *h = 1;
}

void winDoText(f,i,j,z,ah,av)
INT f, i, j;
char *z;
{ INT h, w;
  winTextDim(f,z,&w,&h);
  i -= (ah+1)*w/2;
  if (i<0) i = 0;
  if (i+w>width) i = width-w;
  if (av==1) j++;
  while (*z!='\000')
  { SetPoint(i,j,*z);
    i++;
    z++;
  }
}

void winDrawPoint(i,j)
INT i, j;
{ SetPoint(i,j,'O');
}

double length(x0,y0,x1,y1)
INT x0, y0, x1, y1;
{ return(sqrt((double)(x0*x0+y0*y0))+sqrt((double)(x1*x1+y1*y1)));
}

void winDrawLine(i0,j0,i1,j1)
INT i0, j0, i1, j1;
{ INT i, j, di, dj;
  double l1, l2, l3;
  i = i0; j = j0;
  di = (i1>=i0) ? 1 : -1;
  dj = (j1>=j0) ? 1 : -1;
  SetPoint(i,j,'.');
  while ((i!=i1) | (j!=j1))
  { l1 = length(i+di-i0,j-j0,i1-i-di,j1-j);
    l2 = length(i-i0,j+dj-j0,i1-i,j1-j-dj);
    l3 = length(i+di-i0,j+dj-j0,i1-i-di,j1-j-dj);
    if ((l1<l2) & (l1<l3)) i += di;
      else if (l2<l3) j += dj;
        else { i += di; j += dj; }
    SetPoint(i,j,'.');
  }
}

void winDrawPatch(x,y,c1,c2)
INT *x, *y, c1, c2;
{ winDrawLine(x[0],y[0],x[1],y[1]);
  winDrawLine(x[1],y[1],x[2],y[2]);
  winDrawLine(x[2],y[2],x[3],y[3]);
  winDrawLine(x[3],y[3],x[0],y[0]);
}

void winwrapup(rd)
INT rd;
{ INT i;
  for (i=0; i<height; i++) printf("%s\n",win[i]);
}

INT winmakewin(w,h,wn,rd)
INT *w, *h, wn, rd;
{ if (*w>MAXW) *w = MAXW;
  if (*h>MAXH) *h = MAXH;
  width = *w;
  height = *h;
  nowin = 0;
  winClearScreen(0);
  return(1);
}

void SetWinDev(dev)
device *dev;
{ dev->AddColor = winAddColor;
  dev->SetColor = winSetColor;
  dev->ClearScreen = winClearScreen;
  dev->TextDim = winTextDim;
  dev->DoText = winDoText;
  dev->DrawPoint = winDrawPoint;
  dev->DrawLine = winDrawLine;
  dev->DrawPatch = winDrawPatch;
  dev->wrapup = winwrapup;
  dev->makewin = winmakewin;
  dev->ticklength = 1;
  dev->defth = 24;
  dev->deftw = 78;
}

#endif
