## This 2 functions are destiny to make the matrix inversion process which usually is
## a heavy processing task

## Function 1: makeCacheMatrix
## put the matrix input in the cache memory

makeCacheMatrix<- function(x=matrix()) {
  inverse<- NULL
  set<-function(y) {
    x<<- y
    inverse<<- NULL
  }
  get<- function() x
  setinverse<- function(solve) inverse<<- solve
  getinverse<- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function 2: cacheSolve
## return the inverse of the matrix input

cacheSolve<- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached value for inv")
    return(inverse)
  }
  data<-x$get()
  inverse<- solve(data,...)
  x$setinverse(inverse)
  inverse
}

## testing
##  x = rbind(c(1, 2), c(3, 4))
## > mc = makeCacheMatrix(x)
## > mc$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## No cache in the first run
## > cacheSolve(mc)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Retrieving from the cache in the second run
## > cacheSolve(mc)
## getting cached value for inv
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
