/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 *
 *
 *
  A very simple example showing how to interface to locfit.

  The function mylfcall() accepts arrays x, y and length n
  as arguments, and performs one-dimensional local regression
  with default smoothing parameters etc.

  The mypredict() function computes the fitted values at the
  points in it's x argument; values stored in the f argument.
 */

#include "local.h"

design des;
lfit lf;
INT lf_error;

void mylfcall(x,y,n)
double *x, *y;
INT n;
{
  /*
    store the data on the lfit structure. 
    Set the w (prior weights), c (censoring) and base components to NULL.
  */
  lf.x[0] = x; lf.y = y;
  lf.w = lf.c = lf.base = NULL;

  /*
    lf_error is a flag that is set whenever the fit results in an error.
  */
  lf_error = 0;

  /*
    fitdefault() sets the default smoothing parameters etc.
    The second and third arguments are usually NULL.
    The fourth and fifth arguments are data size n and
      data dimension (ie. number of x variables) d.
  */
  fitdefault(&lf,NULL,NULL,n,1);

  /*
    Finally, we call startlf() to actually compute the fit.
    procv is the vertex processing function.
    the fourth argument, if 1, inhibits subtracting the parametric
    component.
  */
  startlf(&des,&lf,procv,0);
}

void mypredict(x,f,n)
double *x, *f;
int n;
{ INT i;
  for (i=0; i<n; i++)
    x[i] = lf.fl[0]+(lf.fl[1]-lf.fl[0])*i/(n-1);
  preplot(&lf,&des,&x,f,NULL,'n',&n,1,PCOEF);
}

int main()
{ double E[88], C[88], NOx[88], x[50], f[50];
  INT i;
  FILE *aaa;

  /* open and read the input file */
  aaa = fopen("data/ethanol.dat","r");
  for (i=0; i<88; i++) fscanf(aaa,"%lf%lf%lf",&NOx[i],&C[i],&E[i]);
  fclose(aaa);

  /* call the mylfcall() function */
  mylfcall(E,NOx,88);

  /* predict the fit on a grid of 50 points. print results. */
  mypredict(x,f,50);
  for (i=0; i<50; i++)  printf("%8.5f %8.5f\n",x[i],f[i]);
  
  exit(0);
}  


