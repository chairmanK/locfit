/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 *
 *   To use the readline library for command line editing and history,
 *   uncomment  #define USE_READLINE  below, and add  -lreadline
 *   to the top-level Makefile.
 */

#include "local.h"
#include <unistd.h>

#ifdef XWIN
extern void check_window_events(), set_x_display();
#endif

/* #define USE_READLINE */

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

FILE *cfile;
static char prompt[50];
static INT use_cmd_file;

int main(argc,argv)
int argc;
char *argv[];
{ char cli[200], *z;
  INT i;
  if (ident==1) sprintf(prompt,"parfit> ");
           else sprintf(prompt,"locfit> ");
  i = 1; use_cmd_file = 0;

#ifdef XWIN
  z = NULL;
  while (i<argc)
  { if (strcmp(argv[i],"-display")==0)
    { z = argv[i+1];
      i += 2;
    }
    else
    { use_cmd_file = i;
      i++;
    }
  }
  set_x_display(z);
#else
  if (argc>1) use_cmd_file = 1;
#endif
  
  if (use_cmd_file>0)
  { if (access(argv[use_cmd_file],R_OK) != 0)
    { ERROR(("Cannot open file %s for reading.",argv[use_cmd_file]));
      exit(0);
    }
    cfile = fopen(argv[use_cmd_file],"r");
  }
  else cfile = stdin;
  
  setuplf();

  while(1)
  {
    if(!use_cmd_file) /* no command file */
    {
#ifdef USE_READLINE
      z = readline(prompt);
      add_history(z);
#else
      printe("%s",prompt);
      z = fgets(cli,200,cfile);
#endif
    }
    else /* command file */
      z = fgets(cli,200,cfile);

#ifdef XWIN
    check_window_events();
#endif

    /* if it's a command file, are we at the EOF? */
    if (z==NULL) return(0);

    dispatch(z);
#ifdef USE_READLINE
    if(!use_cmd_file) free(z);
#endif
  }
}
