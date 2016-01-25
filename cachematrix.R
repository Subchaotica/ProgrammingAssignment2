## The following complementary functions set up a matrix for inversion, using cached storage capabilities to 
## avoid redundant calculations and providing persistence in the various stages of the process.

## The following function transforms a matrix into a special list that binds it with methods for setting up the matrix,
## fetching it once it has been set up, storing its inverse, and fetching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  setmatrix <- function(y) {
    x <<- y
    m <<- matrix()
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}
  



## The following function invokes functions in makeCacheMatrix to check if the inverse has
## already been calculated, and if so, fetching it from cached storage. If not, it calculates the inverse and 
## sets it in cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.na(m)) {
    message("getting cached inverse")
    return(m)
  }
  initmatrix <- x$getmatrix()
  m <- solve(initmatrix)
  x$setinverse(m)
  m
}
