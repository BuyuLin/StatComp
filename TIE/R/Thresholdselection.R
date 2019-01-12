#' Threshold selection
#'
#' This function will be used as threshold selection in my method, it returns a value that indicate
#' whether the threshold selected is good, using the criteria in the Tail Index Regression. It should
#' be used after the you get the quantile series and tail series.
#' And you can use the plot.uniform function to see the figure.
#' @param wn threshold quantile
#' @param tail tail index
#' @param Y data
#'
#' @return A value that indicate whether the threshold selected is good
#' @export
#' @importFrom stats runif
#'
#' @seealso \code{\link{plotuniform}}
#' @examples
#' \dontrun{
#' p = 0.9
#' wn = qr(x, p) ##x us the data
#' tail = TIE(x,...)
#' statistics(wn, tail, x)
#' }
statistics = function(wn, tail, Y){
  b = Y>wn
  Y = Y[b]
  if(length(tail)>1)
    tail = tail[b]
  if(length(wn)>1)
    wn = wn[b]
  U.hat = exp(-1/tail*log(Y/wn))
  n0 = length(U.hat)
  U.hat = as.vector(U.hat)
  U.hat = U.hat[order(U.hat)]
  Fn.hat = function(x){
    findInterval(x, U.hat)/n0
  }
  mean(sapply(1:1000, function(o){
    U = runif(n0)
    mean((U.hat-Fn.hat(U))^2)
  }))
}

#' QQ plot for fitted tail index and quantile
#'
#' You can use this method to check whether you fit the data well.
#'
#' @param wn quantile or threshold
#' @param tail tail index
#' @param Y data
#'
#' @return a figure cantians QQ plot
#' @export
#' @importFrom graphics plot
#' @importFrom graphics curve
#'
#' @examples
#' \dontrun{
#' p = 0.9
#' wn = qr(x, p) ##x is the data
#' tail = TIE.dynamic1(x,...)
#' plot.uniform(wn, tail, x)
#' }
plotuniform = function(wn,tail, Y){
  b = Y>wn
  Y = Y[b]
  if(length(tail)>1)
    tail = tail[b]
  if(length(wn)>1)
    wn = wn[b]
  U.hat = exp(-1/tail*log(Y/wn))
  n0 = length(U.hat)
  U.hat = as.vector(U.hat)
  U.hat = U.hat[order(U.hat)]
  plot(seq(0.02,0.98, 0.02), quantile(U.hat, seq(0.02,0.98,0.02)))
  g = function(x) x
  curve(g, from = 0, to = 1, add = TRUE, col = 'red')
}


