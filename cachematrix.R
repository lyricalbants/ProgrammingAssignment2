## The two functions below return the inverse of a matrix, x, stored in memory.
## If the inverse of the matrix hasn't been computed, it is computed and then 
## cached to memory.
## 
## The makeCacheMatrix does the job of retrieving the cached inverse of x. It's 
## where the calling functions are defined, get, set, getsolve and setsolve.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

## cacheSolve stores the computed inverse of x. If the inverse of x is null
## it computes it and caches it to memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if (!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}