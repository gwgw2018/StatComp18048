#' @title Rayleigh distribution sampler
#' @description generate samples from Rayleigh density distribution
#' @param m the samples number (numeric)
#' @param sigma the parameter of the distribution (numeric)
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rrrr<- Rray(m,sigma)
#' }
#' @export
RraY<-function(m,sigma){
  x<-numeric(m)
  u<-runif(m/2)#use inverse transformation method to generate
  v<-1-u #antithetic variables#
  u<-c(u,v)
  x<-sqrt(-2*(sigma^2)*log(1-u))#intagrate the Rayleigh density function to get the cdf#
  return(x)
}

