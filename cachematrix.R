## The pair of functions makeCacheMatrix and cacheSolve can cache 
## the inverse of a matrix and retrieve the result from the cache 
## if it was already calculated. It is assumed that the matrix passed 
## can be inverted
## Usage:
## First create the 'CacheMatrix' from a invertible matrix.
## Example of invertible matrix: 
## x <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow = 3, ncol = 3)
## Create the 'CacheMatrix'
## xcm <- makeCacheMatrix(x) 
## Then call cacheSolve to get the inverse from the cache or 
## calculate the inverse and cache it.
## If the inverse is retrieved from the cache then a message 'getting cached data' is output.
## cacheSolve(xcm)

## This function creates a special "matrix" object that can cache its inverse. 
## A list of four functions is returned: set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
  # Initialize m, the inverse matrix
  m <- NULL
  ## Assigning a new matrix to the x of the get function and initializing m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Return the initial matrix 
  get <- function() x
  ## Store the inverse matrix as m (cache) for getinv
  setinv <- function(inv) m <<- inv
  ## Return the cached inverse matrix
  getinv <- function() m
  # Return the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Call function getinv from the 'CacheMatrix'
  m <-  x$getinv()
  ## If m, that is the inverse cached matrix is not empty,
  ## then we have a cache hit which is returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If we don't have a cache hit, then we get the initial matrix
  data <- x$get()
  ## Calculate the inverse
  m <- solve(data, ...)
  ## Pass the result back to the'CacheMatrix' to store the inverse in cache
  x$setinv(m)
  ## Return the inverse
  m
}
