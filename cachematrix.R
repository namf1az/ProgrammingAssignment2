## Put comments here that give an overall description of what your
## functions do
## 
## The functions in this file were tested as follows:
##    source("cachematrix.R")
##    mcm<-makeCacheMatrix()
##    mcm$set(rbind(c(1,-1/4),c(-1/4,1)))
##  > cacheSolve(mcm)
##            [,1]      [,2]
##  [1,] 1.0666667 0.2666667
##  [2,] 0.2666667 1.0666667
##  > cacheSolve(mcm)
##  getting cached matrix inverse
##            [,1]      [,2]
##  [1,] 1.0666667 0.2666667
##  [2,] 0.2666667 1.0666667

## Write a short comment describing this function
## This function creates a matrix and stores/caches its respective inverse
makeCacheMatrix <- function(x = matrix()) {
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function retrieves the "cached" inverse if it exists or creates and caches it for future reference
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached matrix inverse")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
