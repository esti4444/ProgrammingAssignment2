## Functions in this file are used to calculate the inverse of a given matrix.
## Calculated value is saved in "cache" to save computing time on next attempts 

## makeCacheMatrix function creates a special "matrix", which is really a list containing functions to set/get the matrix, 
## and set/get the matrix's cached inverse
makeCacheMatrix <- function(x = matrix()) {
  x_cache_inv <- NULL
  
  ## set the value of the matrix
  set <- function(m) {
    x <<- m
    x_cache_inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse value in "cache"
  setCacheInv <- function(inv) x_cache_inv <<- inv
  
  ## get the "chaced" inverse value
  getCacheInv <- function() x_cache_inv
  
  list(set = set, get = get,
       setCacheInv = setCacheInv,
       getCacheInv = getCacheInv)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve returns the inverse from the cache.
cacheSolve <- function(x, ...) {
  x_inv <- x$getCacheInv()
  ## If incerse was already cached, return the cached value
  if(!is.null(x_inv)) {
    message("getting cached inverse")
    return(x_inv)
  }
  ## otherwise calculate matrix inverse, set it in cache and retrun the inverse value
  x_matrix <- x$get()
  x_inv <- solve(x_matrix, ...)
  x$setCacheInv(x_inv)
  x_inv
}

## test example
## d = c(1,5,6,2,4,8,7,3,2)
## dim(d) = c(3,3)
## k = makeCacheMatrix(d)
## cacheSolve(k)  # calculate first time 
## cacheSolve(k)  # get cached value


