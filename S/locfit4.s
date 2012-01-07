#"xyCall.locfit" = 
#function(x, f, xlim, m = 100, tr, ...)
#{
#  if(x@mi["d"] != 1)
#    stop("xyCall.locfit valid for d=1 only")
#  if(missing(xlim))
#    xlim <- x@box
#  if(missing(tr))
#    tr <- x@trans else function(x) x
#  if(x@mi["ev"] == 3)
#    newx <- x@eva@xev
#  else newx <- seq(xlim[1], xlim[2], length.out = m)
#  pred <- preplot.locfit(x, newx, tr = tr)
#  f(newx, pred@trans(pred@fit), ...)
#}

"addlf" = 
function(e1, e2)
{
  if(e1@mi["p"] > e2@mi["p"])
    return(addlf(e2, e1))
  if(((e1@nvc["nv"]) != (e2@nvc["nv"])) || any(e1@eva@xev != e2@eva@xev))
    stop("Cannot add locfit objects with different fit points")
  mi1 <- e1@mi
  mi2 <- e2@mi
  p1 <- mi1["p"]
  p2 <- mi2["p"]
  d <- mi1["d"]
  nv <- e1@nvc["nv"]
  cf1 <- e1@eva@coef
  cf2 <- e2@eva@coef
  cf1[1:(p1 * nv)] <- cf1[1:(p1 * nv)] + cf2[1:(p1 * nv)]
  cf1[p1 * nv + (1:((p1 + d) * nv))] <- cf1[p1 * nv + (1:((p1 + d) * nv))] + 
    cf2[p2 * nv + (1:((p1 + d) * nv))]
  cf1[(2 * p1 + d) * nv + (1:nv)] <- cf1[(2 * p1 + d) * nv + (1:nv)] + cf2[(2 * 
    p2 + d) * nv + (1:nv)]
  e1@eva@coef <- cf1
  e1
}

"adslf" = 
function(e1, e2)
{
  coef <- e2@eva@coef
  nv <- e2@nvc["nv"]
  p <- e2@mi["p"]
  d <- e2@mi["d"]
  coef[1:nv] <- e1 + coef[1:nv]
  e2@eva@coef <- coef
  e2
}

"muslf" = 
function(e1, e2)
{
  coef <- e2@eva@coef
  nv <- e2@nvc["nv"]
  p <- e2@mi["p"]
  d <- e2@mi["d"]
  coef[1:(p * nv)] <- e1 * coef[1:(p * nv)]
  coef[p * nv + (1:((p + d) * nv))] <- abs(e1) * coef[p * nv + (1:((p + d) * nv
    ))]
  coef[(2 * p + d) * nv + (1:nv)] <- e1 * coef[(2 * p + d) * nv + (1:nv)]
  e2@eva@coef <- coef
  e2
}

"lfa" = 
function(e1, e2)
{
  y1 = e1@trans(e1@fit)
  y2 = e2@trans(e2@fit)
  e1@fit = callGeneric(y1, y2)
  e1@trans = function(x)
  x
  e1
}

"lfm" = 
function(x)
{
  fit = x@trans(x@fit)
  x@fit = callGeneric(as.numeric(fit))
  x@trans = function(x)
  x
  x
}

"update.locfit" <-
function(object, formula, ..., evaluate = T, class)
{
  if(is.null(newcall <- object@call) && is.null(newcall <- attr(object, "call")
    ))
    stop("need an object with call component or attribute")
  tempcall <- match.call(expand.dots = F)$...
  #will be list(...)
  if(!missing(formula)) newcall$formula <- as.vector(update.formula(object, 
      formula, evaluate = T)) else {
    nc <- names(sys.call())
    if(length(nc) && any(pmatch(nc, "formula", 0)))
      newcall$formula <- NULL
  }
  if(!missing(class) && is.character(class))
    newcall[[1]] <- as.name(class[1])
  if(length(tempcall) > 1) {
    def <- getFunction(newcall[[1]])
    def$formula <- NULL
    TT <- match.call(def, tempcall)
    if((ndots <- length(TT)) < length(tempcall)) {
      nt <- pmatch(names(tempcall), names(def)[ - length(def)])
      # kill some args
      nt <- names(def)[nt]
      nT <- names(TT)
      for(i in nt[is.na(match(nt, nT))])
        newcall[[i]] <- NULL
    }
    if(ndots > 1) {
      ndots <- names(TT)[-1]
      newcall[ndots] <- TT[ndots]
    }
  }
  if(evaluate)
    eval(newcall, sys.parent())
  else newcall
}

setClass("lfeval", representation(xev = "numeric", coef = "matrix", scale = 
    "numeric",pc="numeric"))
setClass("kappa", representation(const = "numeric", d = "integer", cov = 
    "numeric", rdf="numeric", crit.val = "numeric"))
setClass("locfit", representation(eva = "lfeval", cell = "list", terms = 
    "ANY", nvc = "named", box = "numeric", sty = "integer", mi = "named",
    deriv="integer",mg = "integer", dp = "named", trans = "function",
    critval="kappa", vnames = "character", yname = "character",
    call = "call", frame="integer"))
setClass("preplot.locfit", representation(xev = "list", fit = "numeric", se.fit
     = "numeric", residual.scale = "numeric", critval="kappa",trans = "function", vnames = 
    "character", yname = "character", dim = "integer", data = "list"))
setClass("gcvplot", representation(alpha="matrix", cri="character",
    df="numeric", values="numeric"))


setMethod("show", signature(object = "locfit"),
    function(object) print.locfit(object))
setMethod("show", signature(object = "preplot.locfit"),
    function(object)print.preplot.locfit(object))
setMethod("show", signature(object = "gcvplot"),
    function(object)plot.gcvplot(object))

setMethod("plot", signature(x="locfit", y="missing"),
    function(x,y,...)plot.locfit(x, ...))
setMethod("plot", signature(x="preplot.locfit", y="missing"),
    function(x,y,...)plot.preplot.locfit(x, ...))
setMethod("plot", signature(x="gcvplot", y="missing"),
    function(x,y,...)plot.gcvplot(x,...))

# mathsoft changed f to FUN in xyCall. Why can't the fools
# keep their grubby paws off things that don't need changing?
#
#setMethod("xyCall", signature(x = "preplot.locfit", y = "missing"),
#    function(x, y, FUN, ..., xexpr, yexpr)
#    { if(is(x@xev, "list"))
#        xx <- x@xev[[1]]
#      else xx <- x@xev
#      FUN(xx, x@trans(x@fit), ...)
#    })
#setMethod("xyCall", signature(x = "locfit", y = "missing"),
#    function(x, y, FUN, ..., xexpr, yexpr) xyCall.locfit(x, FUN, ...))
#setMethod("xyCall", signature(x = "lfeval", y = "missing"),
#    function(x, y, FUN, ..., xexpr, yexpr)
#    { m <- length(x@xev)
#      FUN(x@xev, x@coef[1:m], ...)
#    })
#setMethod("xyCall", signature(x = "gcvplot", y = "missing"),
#    function(x, y, FUN, ..., xexpr, yexpr) FUN(x@df,x@values,...))

setMethod("Ops", signature(e1="preplot.locfit", e2="numeric"),
    function(e1, e2)
    { e1@fit = callGeneric(e1@trans(e1@fit), e2)
      e1@trans = function(x)x
      e1
    })
setMethod("Ops", signature(e1="numeric", e2="preplot.locfit"),
    function(e1, e2)
    { e2@fit = callGeneric(e1, e2@trans(e2@fit))
      e2@trans = function(x)x
      e2
    })
setMethod("Ops", signature(e1 = "preplot.locfit", e2 = "preplot.locfit"), lfa)

setMethod("Math", signature(x = "preplot.locfit"), lfm)

setMethod("+", signature(e1 = "locfit", e2 = "locfit"), addlf)
setMethod("+", signature(e1 = "numeric", e2 = "locfit"), adslf)
setMethod("+", signature(e1 = "locfit", e2 = "numeric"),
  function(e1, e2)adslf(e2, e1))

setMethod("-", signature(e1 = "locfit", e2 = "locfit"),
  function(e1, e2)addlf( e1, muslf(-1,e2)))
setMethod("-", signature(e1 = "locfit", e2 = "numeric"),
  function(e1, e2)adslf( - e2, e1))
setMethod("-", signature(e1 = "numeric", e2 = "locfit"),
  function(e1, e2)adslf(e1,muslf(-1,e2)))

setMethod("*", signature(e1 = "numeric", e2 = "locfit"), muslf)
setMethod("*", signature(e1 = "locfit", e2 = "numeric"),
  function(e1, e2)muslf(e2, e1))

setMethod("/", signature(e1 = "locfit", e2 = "numeric"), function(e1, e2)
  muslf(1/e2, e1))

