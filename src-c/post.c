/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 */

#include <unistd.h>
#include "local.h"

#define DPI 300

#ifdef CVERSION

FILE *psfile;
extern struct lfcol mycol[MAXCOLOR];
extern char *psfn;
extern INT ncolor;
static INT height=0, width=0, ccol;
double dpp;

void psSetColor(col)
INT col;
{ if (col!=ccol)
    fprintf(psfile,"C%d\n",col);
  ccol = col;
}

void psAddColor(name,r,g,b,p)
char *name;
INT r, g, b, p;
{ strcpy(mycol[p].name,name);
  mycol[p].n = p;
  mycol[p].r = r;
  mycol[p].g = g;
  mycol[p].b = b;
}

void psClearScreen(col)
INT col;
{
}

void psTextDim(f,z,w,h) /* must do something sensible */
INT f, *w, *h;
char *z;
{ if (f==1)
  { *w = (INT)(10*strlen(z)*dpp);
    *h = (INT)(18*dpp);
  }
  else
  { *w = (INT)(8*strlen(z)*dpp);
    *h = (INT)(12*dpp);
  }
}

void psDoText(f,i,j,z,ah,av)
INT f, i, j;
INT ah, av;
char *z;
{ INT w, h, k;
  if (f==1) fprintf(psfile,"F1\n");
  psTextDim(f,z,&w,&h);
  /* i -= w*(ah+1)/2; */
  j += h*(av+1)/2;
  putc('(',psfile);
  for (k=0; k<strlen(z); k++)
  { if ((z[k]=='(')|(z[k]==')')) putc('\\',psfile);
    putc(z[k],psfile);
  }
  fprintf(psfile,") %d %d ",i,height-j);
  switch(ah)
  { case -1: fprintf(psfile,"Lt\n"); break;
    case  0: fprintf(psfile,"Ct\n"); break;
    case  1: fprintf(psfile,"Rt\n"); break;
  }
  if (f==1) fprintf(psfile,"F0\n");
}

void psDrawLine(i0,j0,i1,j1)
INT i0, j0, i1, j1;
{ fprintf(psfile,"%d %d %d %d L\n",i0,height-j0,i1,height-j1);
}

void psDrawPoint(i,j,pch)
INT i, j, pch;
{ switch(pch)
  { case 1: /* o */
      fprintf(psfile,"%d %d P\n",i,height-j);
      return;
    case 2: /* + */
      psDrawLine(i-(INT)dpp,j,i+(INT)dpp,j);
      psDrawLine(i,j-(INT)dpp,i,j+(INT)dpp);
      return;
    case 3: /* X */
      psDrawLine(i-(INT)dpp,j-(INT)dpp,i+(INT)dpp,j+(INT)dpp);
      psDrawLine(i+(INT)dpp,j-(INT)dpp,i-(INT)dpp,j+(INT)dpp);
      return;
  }
}

void psDrawPatch(x,y,c1,c2)
INT *x, *y, c1, c2;
{ fprintf(psfile,"/Q { %d %d %d %d %d %d %d %d } bind def\n",
    x[0],height-y[0], x[1],height-y[1],
    x[2],height-y[2], x[3],height-y[3]);
  psSetColor(c1);
  fprintf(psfile,"PF\n");
  if (c1==c2) return;
  psSetColor(c2);
  fprintf(psfile,"PB\n");
}

void pswrapup(rd)
INT rd;
{ fprintf(psfile,"showpage\n");
  fclose(psfile);
}

void psgetwinsize(h,w)
INT *h, *w;
{ *h = 5*DPI; *w = 7.5*DPI;
}

INT psmakewin(w,h,wn,rd)
INT *w, *h, wn, rd;
{ int i;
  /* psgetwinsize(h,w); */
  height=*h*DPI/25.4; width=*w*DPI/25.4;
  *h = height; *w = width;
  dpp = DPI/72.0;
  ccol = -1;
  psfile = (psfn==NULL) ? fopen("lfplot.ps","w") : fopen(psfn,"w");
  fprintf(psfile,"%c!PS-Adobe-2.0 EPSF-2.0\n",'%');
  fprintf(psfile,"%c%cCreator: Locfit\n",'%','%');
  fprintf(psfile,"%c%cBoundingBox: 0 0 %d %d\n",'%','%',
    (int)(width/dpp),(int)(height/dpp));
  fprintf(psfile,"%c%cEndComments\n",'%','%');
  fprintf(psfile,"/P {%d 0 360 arc stroke} bind def\n",(int)(2*dpp));
  fprintf(psfile,"/L {moveto lineto stroke} bind def\n");
  fprintf(psfile,"/PF {Q moveto lineto lineto lineto closepath fill stroke} bind def\n");
  fprintf(psfile,"/PB {Q moveto lineto lineto lineto closepath stroke} bind def\n");
  fprintf(psfile,"/Lt {moveto show stroke} bind def\n");
  fprintf(psfile,"/Ct {moveto dup stringwidth pop 2 div neg 0 rmoveto show stroke} bind def\n");
  fprintf(psfile,"/Rt {moveto dup stringwidth pop neg 0 rmoveto show stroke} bind def\n");
  fprintf(psfile,"/F0 {/Courier findfont %d scalefont setfont} bind def\n",(int)(12*dpp));
  fprintf(psfile,"/F1 {/Courier findfont %d scalefont setfont} bind def\n",(int)(18*dpp));
  fprintf(psfile,"%f %f scale\n",1/dpp,1/dpp);
  fprintf(psfile,"F0\n");
  for (i=0; i<=18; i++)
    fprintf(psfile,"/C%d {%f %f %f setrgbcolor} bind def\n",i,
      mycol[i].r/255.0,mycol[i].g/255.0,mycol[i].b/255.0);
  return(1);
}

void SetPSDev(dev)
device *dev;
{
  dpp = DPI/72.0;
  dev->AddColor = psAddColor;
  dev->SetColor = psSetColor;
  dev->ClearScreen = psClearScreen;
  dev->TextDim = psTextDim;
  dev->DoText = psDoText;
  dev->DrawPoint = psDrawPoint;
  dev->DrawLine = psDrawLine;
  dev->DrawPatch = psDrawPatch;
  dev->wrapup = pswrapup;
  dev->makewin = psmakewin;
  dev->ticklength = (int)(5*dpp);
  dev->defth = 100;
  dev->deftw = 125;
}

#endif
