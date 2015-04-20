## From R Programming on coursera.org
## 
## Programming Assignment 2
## due on Sun 26 Apr 4:30 pm

## Caching the Inverse of a Matrix
##   Matrix inversion is usually a costly computation and there may be some 
##   benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize an empty matrix for the cache
  m <- NULL
  
  set <- function(y){
    ## Scoping assignment operator of <<- used here in nested function
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  
  setmatrix <-function(solve) m <<- solve
  getmatrix <-function() m
  
  ## List Get/Set of the cached matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  
  m <- x$getmatrix()
  if(!is.null(m)){
    ## Check to see if inverse has already been cached, and if so, return it
    message("getting cached data")
    return(m)
  }
  
  ## No cached matrix, so attempt to solve() 
  ## Do the inverse magic here
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  
  ## Return the inverse matrix
  m
}
