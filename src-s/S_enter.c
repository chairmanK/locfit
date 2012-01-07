/*
 *   Copyright (c) 1996-2000 Lucent Technologies.
 *   See README file for details.
 */

#include "S.h"
#undef WARN
#undef ERROR
#undef INT

#include "local.h"

design des;
lfit lf;

INT lf_error;
extern INT cvi;

#ifdef RVERSION
typedef char * CALL_S_FUNC;
typedef void * CALL_S_ARGS;
#else
typedef void * CALL_S_FUNC;
typedef char * CALL_S_ARGS;
#endif
typedef long int CALL_S_LEN;
typedef long int CALL_S_NARG;
typedef char * CALL_S_MODE;
typedef long int CALL_S_NRES;
typedef char * CALL_S_VALS;

static CALL_S_FUNC bsfunc, bsf2;

void basis(x,t,f,dim,p)
double *x, *t, *f;
INT dim, p;
{
  CALL_S_ARGS args[2];
  CALL_S_LEN  length[2];
  CALL_S_NARG nargs;
  CALL_S_MODE mode[2];
  CALL_S_NRES nres;
  CALL_S_VALS values[1];
  double z0[1], z1[1], *vptr;
  int i;

  args[0] = (CALL_S_ARGS)x;
  mode[0] = "double";
  length[0] = dim;

  args[1] = (CALL_S_ARGS)t;
  mode[1] = "double";
  length[1] = dim;

  nargs = 2;
  nres = 1;

  call_S(bsfunc,nargs,args,mode,length,(char **)NULL,nres,values);

  vptr = (double *)values[0];
  for (i=0; i<p; i++) f[i] = vptr[i];
}

void vbasis(x,t,n,d,ind,m,p,X)
double **x, *t, *X;
int n, d, m, p, *ind;
{
  CALL_S_ARGS args[MXDIM+3];
  CALL_S_LEN  length[MXDIM+3];
  CALL_S_NARG nargs;
  CALL_S_MODE mode[MXDIM+3];
  CALL_S_NRES nres;
  CALL_S_VALS values[1];
  double *vptr;
  int i;

  args[0] = (CALL_S_ARGS)(&d);
  mode[0] = "integer";
  length[0] = 1;

  args[1] = (CALL_S_ARGS)ind;
  mode[1] = "integer";
  length[1] = m;

  args[2] = (CALL_S_ARGS)t;
  mode[2] = "double";
  length[2] = d;

  for (i=0; i<d; i++)
  { args[3+i] = (CALL_S_ARGS)x[i];
    mode[3+i] = "double";
    length[3+i] = n;
  }

  nargs = d+3;
  nres = 1;

  call_S(bsf2,nargs,args,mode,length,0,nres,values);
  vptr = (double *)values[0];
  for (i=0; i<m*p; i++) X[i] = vptr[i];
}

vari *setsvar(v,x,n)
vari *v;
double *x;
INT n;
{ v->dpr = x;
  v->n = n;
  return(v);
}

void slocfit(x,y,c,w,b,lim,mi,dp,str,sca,xev,wdes,wtre,wpc,nvc,
  iwork,lw,mg,L,kap,deriv,nd,sty,bs)
double *x, *y, *c, *w, *b, *lim, *dp, *sca, *xev, *L, *kap, *wdes, *wtre, *wpc;
INT *mi, *nvc, *iwork, *lw, *mg, *deriv, *nd, *sty;
char **str;
CALL_S_FUNC *bs;
{ INT n, d, i, kk;
  vari v1, v2, v3, vL, vi, vx, vxev;
  if (mi[MUBAS])
  { bsfunc = bs[0];
    bsf2 = bs[1];
  }
  lf_error = 0;
  n = mi[MN]; d = mi[MDIM];
  for (i=0; i<d; i++)
  { dvari(&lf,i) = &x[i*n];
    lf.sty[i] = sty[i];
  }
  lf.y = y; lf.w = w;
  lf.base = b;
  lf.c = c;

  des.dw = setsvar(&v2,wdes,lw[0]);
  vx.dpr = (double *)iwork; vx.n = n; des.index = &vx;
  iwork += n;

  lf.xxev = setsvar(&vxev,xev,d*nvc[0]);
  lf.tw   = setsvar(&v1,wtre,lw[1]);
  lf.pc.wk= setsvar(&v3,wpc,lw[3]);
  vi.dpr = (double *)iwork; vi.n = lw[2]-n; lf.iw = &vi;
  lf.mg = mg;
  vL.dpr = L; vL.n = lw[4]; lf.L = &vL;
  lf.nvm = nvc[0];
  setstrval(mi,MKER ,str[0]);
  setstrval(mi,MTG  ,str[1]);
  setstrval(mi,MLINK,str[2]);
  setstrval(mi,MIT  ,str[3]);
  setstrval(mi,MACRI,str[4]);

  lf.nd = *nd;
  for (i=0; i<*nd; i++) lf.deriv[i] = deriv[i]-1;
  if (lf_error) return;

  memcpy(lf.fl,&lim[2*d],2*d*sizeof(double));
  memcpy(lf.xl ,lim,2*d*sizeof(double));
  memcpy(lf.sca,sca,d*sizeof(double));
  memcpy(lf.mi,mi,LENM*sizeof(INT));
  memcpy(lf.dp,dp,LEND*sizeof(double));
  switch(mi[MGETH])
  { case 0: /* the standard fit */
    case 4: /* for gam.lf, return residuals */
    case 5: /* for gam.lf prediction */
      if (mi[MDEG0]==mi[MDEG])
      { startlf(&des,&lf,procv,0);
        if (!lf_error) ressumm(&lf,&des);
      }
      else startlf(&des,&lf,procvvord,0);
      break;
    case 1: /* hat matrix */
      startlf(&des,&lf,procvhatm,mi[MKER]!=WPARM);
      break;
    case 2: /* compute kappa0 */
      constants(&des,&lf,kap);
      return;
    case 3: /* regression bandwidth selection */
      rband(&des,&lf,kap,deriv,nd,&kk);
      return;
    case 6: /* lscv */
      startlf(&des,&lf,procv,1);
      dens_lscv(&des,&lf);
      return;
    case 70:
    case 71: /* scb */
    case 72:
    case 73:
    case 74:
      scb(&des,&lf);
      break;
  }

  nvc[0] = lf.nvm;
  nvc[1] = lf.ncm;
  nvc[3] = lf.nv;
  nvc[4] = lf.nce;
  memcpy(mi,lf.mi,LENM*sizeof(INT));
  memcpy(dp,lf.dp,LEND*sizeof(double));
  memcpy(&lim[2*d],lf.fl,2*d*sizeof(double));
  memcpy(lim,      lf.xl,2*d*sizeof(double));
  memcpy(sca,lf.sca,d*sizeof(double));
}

void recoef(coef,ce,d,ev,nvc)
double *coef;
INT *ce, d, ev, *nvc;
{ INT vc;
  lf.coef = coef; coef += lf.nv*(d+1);
  lf.nlx  = coef; coef += lf.nv*(d+1);
  lf.t0   = coef; coef += lf.nv*(d+1);
  lf.lik  = coef; coef += lf.nv*3;
  lf.h    = coef; coef += lf.nv;
  lf.deg  = coef; coef += lf.nv;

  switch(ev)
  { case ETREE:
    case EKDTR:
    case EGRID: vc = 1<<d; break;
    case EPHULL: vc = d+1; break;
    case EXBAR:
    case ECROS:
    case EDATA:
    case EPRES:
    case ENONE: break;
    default: ERROR(("spreplot: Invalid ev"));
  }
  lf.vc = vc;
  lf.ce = ce; ce += nvc[4]*vc;
  lf.s  = ce; ce += MAX(nvc[3],nvc[4]);
  lf.lo = ce; ce += MAX(nvc[3],nvc[4]);
  lf.hi = ce; ce += MAX(nvc[3],nvc[4]);
}

void spreplot(xev,coef,sv,ce,x,res,se,wpc,sca,m,nvc,mi,dp,mg,deriv,nd,sty,where,what,bs)
double *xev, *coef, *sv, *x, *res, *se, *wpc, *sca, *dp;
INT *ce, *m, *nvc, *mi, *mg, *deriv, *nd, *sty, *where;
char **what;
void **bs;
{ INT p, i, vc;
  double *xx[MXDIM];
  vari v3, vxev;
  if (mi[MUBAS]) bsfunc = bs[0];
  lf_error = 0; p = mi[MP];
  lf.ncm = nvc[1]; lf.nv = lf.nvm = nvc[3]; lf.nce = nvc[4];

  vxev.dpr = xev; vxev.n = mi[MDIM]*lf.nv; lf.xxev = &vxev;

  recoef(coef,ce,mi[MDIM],mi[MEV],nvc);
  memcpy(lf.sca,sca,mi[MDIM]*sizeof(double));
  memcpy(lf.mi,mi,LENM*sizeof(INT));
  memcpy(lf.dp,dp,LEND*sizeof(double));
  lf.sv = sv;
  lf.mg = mg;
  v3.dpr = wpc; lf.pc.wk = &v3;
  v3.n = pc_reqd(mi[MDIM],p);
  pcchk(&lf.pc,mi[MDIM],p,0);
  lf.pc.xtwx.st = JAC_EIGD;
  for (i=0; i<mi[MDIM]; i++) lf.sty[i] = sty[i];

  /* set up the data structures right */
  switch (*where)
  { case 2: /* grid */
      for (i=0; i<mi[MDIM]; i++)
      { xx[i] = x;
        x += m[i];
      }
      break;
    case 1: /* vector */
    case 3: /* data */
      for (i=0; i<mi[MDIM]; i++) dvari(&lf,i) = xx[i] = &x[i**m];
      break;
    case 4: /* fit points, need nothing! */
      break;
    default:
      ERROR(("unknown where in spreplot"));
  }

  lf.nd = *nd;
  for (i=0; i<*nd; i++) lf.deriv[i] = deriv[i]-1;

  if (lf_error) return;
  preplot(&lf,&des,xx,res,se,what[1][0],m,*where,ppwhat(what[0]));
}

void sfitted(x,y,w,c,ba,fit,cv,st,xev,coef,sv,ce,wpc,sca,nvc,mi,dp,mg,deriv,nd,sty,what,bs)
double *x, *y, *w, *c, *ba, *fit, *xev, *coef, *sv, *wpc, *sca, *dp;
INT *cv, *st, *ce, *nvc, *mi, *mg, *deriv, *nd, *sty;
char **what;
void **bs;
{ INT i, n;
  vari v3, vxev;
  if (mi[MUBAS]) bsfunc = bs[0];
  n = mi[MN];
  lf_error = 0; cvi = -1;
  lf.ncm = nvc[1]; lf.nv = lf.nvm = nvc[3]; lf.nce = nvc[4];
  lf.mg = mg;
  for (i=0; i<mi[MDIM]; i++)
  { dvari(&lf,i) = &x[i*n];
    lf.sty[i] = sty[i];
  }
  lf.y = y; lf.w = w; lf.c = c; lf.base = ba;
  vxev.dpr = xev; vxev.n = mi[MDIM]*lf.nv; lf.xxev = &vxev;
  recoef(coef,ce,mi[MDIM],mi[MEV],nvc);
  memcpy(lf.sca,sca,mi[MDIM]*sizeof(double));
  memcpy(lf.mi,mi,LENM*sizeof(INT));
  memcpy(lf.dp,dp,LEND*sizeof(double));
  lf.sv = sv;
  v3.dpr = wpc; lf.pc.wk = &v3;
  v3.n = pc_reqd(mi[MDIM],mi[MP]);
  pcchk(&lf.pc,mi[MDIM],mi[MP],0);
  lf.pc.xtwx.st = JAC_EIGD;
  lf.nd = *nd;
  for (i=0; i<*nd; i++) lf.deriv[i] = deriv[i]-1;
  fitted(&lf,&des,fit,ppwhat(what[0]),*cv,*st,restyp(what[1]));
}

void triterm(xev,h,ce,lo,hi,sca,nvc,mi,dp,nt,term,box)
double *xev, *h, *sca, *dp, *box;
INT *ce, *lo, *hi, *nvc, *mi, *nt, *term;
{ INT d, i, t;
  vari vxev;
  lf_error = 0;
  lf.nv = lf.nvm = nvc[3];
  vxev.dpr = xev; vxev.n = mi[MDIM]*lf.nv; lf.xxev = &vxev;
  memcpy(lf.sca,sca,mi[MDIM]*sizeof(double));
  memcpy(lf.mi,mi,LENM*sizeof(INT));
  memcpy(lf.dp,dp,LEND*sizeof(double));
  lf.h = h;
  lf.ce = ce;
  lf.lo = lo;
  lf.hi = hi;
  *nt = 0;
  d = mi[MDIM];
  if (mi[MEV]==ETREE)
    atree_grow(NULL, &lf, lf.ce, nt, term, box, &box[d]);
  else
    for (i=0; i<nvc[4]; i++)
      triang_grow(NULL,&lf,&lf.ce[i*lf.vc],nt,term);
}

void kdeb(x,mi,band,ind,h0,h1,meth,nmeth,ker)
double *x, *band, *h0, *h1;
INT *mi, *ind, *meth, *nmeth, *ker;
{ kdeselect(band,x,ind,*h0,*h1,meth,*nmeth,*ker,mi[MN]);
}
