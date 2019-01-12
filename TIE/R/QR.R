#' Using CAViaR method to estimate high but not extreme quantile
#'
#' @param x time series data
#' @param p the quantile you want to estimate
#' @param tol the accuracy of your optimization
#'
#' @return A vector containing the estimated beta
#' @export
#' @importFrom stats quantile
#' @importFrom stats nlm
#'
#' @examples
#' \dontrun{
#' x = rgev(1000)
#' p = 0.9
#' qr(x, p)
#' qr(x, 0.85)
#'
#' }
qr = function(x,p,tol = 1e-4){
  n = length(x)
  q.es = c(quantile(x, p))
  beta.es = c(0.2,0.9,0.4)
  i = 1
  loss = function(beta){
    for(i in 2:n){
      q.es[i] = beta[1]+beta[2]*q.es[i-1]+beta[3]*abs(x[i-1])
    }
    mean((p-I(x<q.es))*(x-q.es))
  }
  while(TRUE){
    print(i)
    beta.es.new1 = suppressWarnings(neldermead::fminsearch(fun = loss, x0 = beta.es)$optbase$xopt)
    if(max(abs(beta.es.new1-beta.es))<tol){
      beta.es = beta.es.new1
      break
    }
    beta.es.new2 = suppressWarnings(nlm(f = loss, p = beta.es.new1)$estimate)
    if(max(abs(beta.es.new2-beta.es.new1))<tol){
      beta.es = beta.es.new2
      break
    }
    beta.es = beta.es.new2
    i = i+1
  }
  beta.es
}











