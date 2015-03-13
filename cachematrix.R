## It contains two function, makeCacheMatrix and 
## cacheSolve, to calculate the inverse of a matrix
## and cache the results. Thus advoid recomputed.

## This function can set and get 
## the value of a matrix(suppose it's called x) 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xi<-NULL
  set<-function(y){
    x<<-y
    xi<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) xi<<-solve
  getinverse<-function() xi
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The second function calculates
## x's inverse(xi). If xi exists, it skips 
## calculation and search the value in global 
## environment.


cacheSolve <- function(x, ...) {
  xi<-x$getinverse()
  if(!is.null(xi)){
    message("getting cached data")
    return(xi)
  }
  data<-x$get()
  xi<-solve(data,...)
  x$setinverse(xi)
  xi
  ## Return a matrix that is the inverse of 'x'
}
