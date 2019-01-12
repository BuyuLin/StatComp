#' reconstruct tail using the estimated gamma
#'
#' This function use the estimated gamma to reconstruct the tail
#' @param gam estimated gamma
#' @param real this identify wether you use the true data, or you use the simulation data.
#' @param G the value fitted into the dynamic equation
#' @param x data
#'
#' @seealso \code{\link{recon.q}}
#' @return A vector contains the estimated tail
#' @export
#'
#' @examples
#' \dontrun{
#' x ##data
#' p = 0.9
#' q.es = qr(x, p) ##estimated quantile as threshold
#' gam.es = TIE.dynamic1(q.es, x)
#' tail.es = recon.tail(x, gam.es)
#' }
#' \dontrun{
#' ##When doing simulation, I would call TIE.dynamic2, and let the real = FALSE
#' x ##data
#' p = 0.9
#' q.es = qr(x, p) ##estimated quantile as threshold
#' gam.es = TIE.dynamic2(q.es, x)
#' tail.es = recon.tail(x, gam.es, real = False)
#' }
recon.tail = function(x,gam, real = TRUE, G = 10){
  n = length(x)
  if(real == TRUE){
    tail = c(0.3)
    for(i in 2:n){
      tail[i] = exp(gam[1]+gam[2]*log(tail[i-1])-gam[3]*exp(-gam[4]*x[i-1]))
    }
    tail
  }
  else{
    tail = c(0.3)
    for(i in 2:n){
      tail[i] = exp(gam[1]+gam[2]*log(tail[i-1])-gam[3]*exp(-G*x[i-1]))
    }
    tail
  }
}


#' reconstruct quantile use the estimated beta
#'
#' This function use the estimated beta to reconstruct the quantile
#' @param beta estimated beta
#' @param init the initial value for quantile regression
#' @param x data
#'
#' @return a vector that contains the quantile series
#' @export
#'
#' @examples
#' \dontrun{
#' x ##data
#' p = 0.9
#' init = qnorm(p)
#' beta.es = qr(x, init)
#' recon.q(beta.es, p)
#' }
recon.q = function(beta, init, x){
  n = length(x)
  q = c(init)
  for(i in 2:n){
    q[i] = beta[1]+beta[2]*q[i-1]+beta[3]*abs(x[i-1])
  }
  q
}


