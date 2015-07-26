## This function makes a cache of inverted forms of matrices and 
## checks the cache to see whether the calculation has already been done
## and either returns the previously calculated result or calculates a 
## new result and adds it to the cache.

## Creates cache to store matrices and their inverses

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## default inverse value is empty (not calculated)
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse ## creates function for using solve
  getinverse <- function() m ## returns inverse from using solve
  list(set = set, get = get,## assigns names of subfunctions within a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## checks cache for matrix inverse. Returns cached solution
## if no cached value, calculates inverse and stores it in cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)) { ##checks cache for inverse
    message("getting cached data")
    return(m) ##returns cache result
  }
  data <- x$get()
  m <- solve(data, ...) ##calculates inverse of x
  x$setinverse(m) ##stores inverse in cache
  m ##returns inverse
}
