/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 *
 *   Screen output functions for stdout.
 *
 *   Do not include local.h in this file.
 */


#include <stdio.h>
#include <stdarg.h>

extern FILE *ofile;

int lfprintf(char *format, ...)
{ int z;
  va_list vp;
  va_start(vp,format);
  if (ofile != NULL)
    z = vfprintf(ofile,format,vp);
  else
    z = vprintf(format,vp);
  va_end(vp);
  return(z);
}

/* use printe for stuff that goes to stderr
   i.e. screen, even when ofile is set */
int printe(char *format, ...)
{ int z;
  va_list vp;
  va_start(vp,format);
  z = vfprintf(stderr,format,vp);
  va_end(vp);
  return(z);
}
