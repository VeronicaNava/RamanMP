##Savitzky-Golay smoothing

savit.gol <- function(x, filt, filt_order = 4, der_order = 0) {
  if(is.numeric(x)==F)
    stop("Argument 'x' must be numeric")
  if(is.numeric(filt)==F)
    stop("Argument 'filt' must be numeric")
  if (filt <= 1 || filt %% 2 == 0)
    stop("Argument 'filt' must be a odd integer number, > than 1")

  #Filter coefficient calculation
  filt_coef <- (filt-1)/2                          # index: window left and right
  X <- outer(-filt_coef:filt_coef, 0:filt_order, FUN="^")   # polynomial terms and coeffs

  s <- svd(X) #singular-value decomposition of the rectangular matrix
  ypp = .Machine$double.eps^(2/3)
  p <- ( s$d > max(ypp * s$d[1], 0) )
  if (all(p)) {
    mp <- s$v %*% (1/s$d * t(s$u))
  } else if (any(p)) {
    mp <- s$v[, p, drop=FALSE] %*% (1/s$d[p] * t(s$u[, p, drop=FALSE]))
  } else {
    mp <- matrix(0, nrow=ncol(X), ncol=nrow(X))
  }

  Y<-mp
  x2 <- convolve(x, rev(Y[(der_order+1),]), type="o")   # convolve(...)
  len<-length(x2)
  x2 <- x2[(filt_coef+1):(len-filt_coef)]

  final_val <- ((-1)^der_order * x2)
  return(final_val)
}
