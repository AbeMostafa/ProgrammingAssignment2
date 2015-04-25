## These functions speed-up processes 
## where the inverse of a Matrix needs to be calculated many times
## for a single matrix by caching

## The makeCahceMatrix function creates a special "vector", which is a list containing functions that cache a Matrix

makeCacheMatrix <- function(y = matrix()) {
  m <- NULL
  
  get <- function() y
  
  set <- function(z){
    x <<- z
    
    m <<- NULL
  }
  
  setCache <- function(cache)
    m <<- cache
  
  
  getCache <- function() m
  
  list(get = get,
       setCache = setCache,
       getCache = getCache)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else{
    data <- x$get()
    m <- solve(data, ...)
    x$setCache(m)
    print(m)
  }
}