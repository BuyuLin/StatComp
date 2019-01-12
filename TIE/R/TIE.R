#' Tail Index Estimation
#'
#' These method combines the extreme value theory and CAViaR to obtain the dynamic of tail index
#' we first get some high quantile estimation in the qr function, and then use this as threshold
#' and use the extreme value theory to get a likelihood type criteria.The function returns the estimated
#' gamma and then you can use the estimated gamma and fit this in the recon.tail function to get the
#' tail index series.
#'
#' Version one spend some time choosing the initial value of the optimization. If you find that
#' the TIE estimation version2 lacks the stability you can use this version, this version will
#' use different value for initiallization, so it can give more reliable solution.
#'
#' I recommend to use version two in simulation, since the dynamic of the tail index is simpler
#' in order to increase the speed in simulation.
#'
#' And TIE.fix assumes the tail index is a constant, which leads to quicker computation, but gives
#' less appealing result.
#'
#' @param q.es estimated quantile threshold
#' @param x data
#' @param init initial value for tail index
#'
#' @return a list that contains the estimated gamma
#' @export
#' @importFrom stats nlm
#'
#' @examples
#' \dontrun{
#' x ##data
#' q.es = qr(x, 0.9)
#' TIE.dynamic1(q.es, x)
#' }
TIE.dynamic1 = function(q.es,x, init = 0.3){
  n = length(x)
  tail.es = numeric(n)
  tail.es[1] = init
  loss.tail = function(beta){
    for(i in 2:n){
      tail.es[i] = exp(beta[1]+beta[2]*log(tail.es[i-1])-beta[3]*exp(-beta[4]*x[i-1]))
    }
    b = x>q.es
    x = x[b]
    if(length(q.es)>1)
      q.es = q.es[b]
    tail.es = tail.es[b]
    sum((1/tail.es*(log(x/q.es))+log(tail.es)))
  }

  G = seq(4,20,0.5)
  selection.G = numeric(length(G))
  GAM.es = matrix(0,4, length(G))
  for(j in 1:length(G)){
    gam.es = nlm(f = loss.tail, p = c(0.023, 0.7, 0.25, G[j]))$estimate
    GAM.es[,j] = gam.es
    for(i in 2:n)
      tail.es[i] = exp(gam.es[1]+gam.es[2]*log(tail.es[i-1])-gam.es[3]*exp(-gam.es[4]*x[i-1]))
    selection.G[j] = statistics(q.es, tail.es, x)
  }
  result = which(selection.G == min(selection.G))
  gam.es = GAM.es[,result]
}

#' @describeIn TIE.dynamic1 Another version of TIE.dynamic
#' @param G You can see the article for the selection of G
#' @return a vector that contains the estimated gamma which will be used to fit into the recon.tail function
#' @export
#' @importFrom stats nlm
#'
#' @examples
#' \dontrun{
#' x ##data
#' q.es = qr(x, 0.9)
#' TIE.dynamic1(q.es, x)
#' }
TIE.dynamic2 = function(x, q.es, G = 10, init = 0.3){
  n = length(x)
  tail.es = numeric(n)
  tail.es[1] = init
  loss.tail = function(beta){
    for(i in 2:n){
      tail.es[i] = exp(beta[1]+beta[2]*log(tail.es[i-1])-beta[3]*exp(-G*x[i-1]))
    }
    b = x>q.es
    x = x[b]
    if(length(q.es)>1)
      q.es = q.es[b]
    tail.es = tail.es[b]
    sum((1/tail.es*(log(x/q.es))+log(tail.es)))
  }
  gam.es = nlm(f= function(beta){
    loss.tail(beta)
  }, p = c(0.7,0.5,2))$estimate
  gam.es
}


#' @describeIn TIE.dynamic1 Fixed TIE estimation
#'
#' @return the estimated fixed tail index
#' @export
#' @importFrom stats optimize
#' @examples
#' \dontrun{
#' x ##data
#' p = 0.9
#' q.es = qr(x, p)
#' tail.es = TIE.fix(x, q.es)
#' }
TIE.fix = function(x, q.es){
  tail.es.fix = optimize(f = function(tail){
    b = x>q.es
    x = x[b]
    q.es = q.es[b]
    sum(1/tail*log(x/q.es)+log(tail))
  }, lower = 0, upper = 1)$minimum
  tail.es.fix
}
