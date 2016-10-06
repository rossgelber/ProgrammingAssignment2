## This pair of functions works to return the inverse of a matrix in an efficient manner.
## In order to save computing power, these functions can create a cache of matrix inverses as well as check that
## cache to see if the inverse of a matrix has already been calculated and cached before calculating the inverse again.


## This function can perform four actions on a given matrix. It can get put a matrix in a cache,
## get a matrix from a cache, set the inverse of a matrix in a cache, or get the inverse of a matrix
## from a cache.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix. However, before doing the calculation, it checks to see
## if the inverse has previously been calculated and stored in the cache. If so, it pulls the inverse from the
## cache and returns it. If not, it calculates and returns the inverse itself and stores it in the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
