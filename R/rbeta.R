#' @title beta distribution sampler
#' @description a random sample of size 1000 from Beta(3,2) distribution
#' @param n the samples number (numeric)
#' @param a the first parameter of the distribution (numeric)
#' @param b the second parameter of the distribution (numeric)
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rrrr<- Rbeta(n,a,b)
#' }
#' @export
rbetA<-function(n,a,b){
   j<-k<-0
   y<-numeric(n)
   while(k<n){
     u<-runif(1)
     j<-j+1
     x<-runif(1)
     if(x^(a-1)*(1-x)^(b-1)>u){
       k<-k+1
       y[k]<-x
     }
   }
   return(y)
}


