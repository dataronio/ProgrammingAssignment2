## These functions store and retrieve a matrix and caches the
## matrix's inverse.  If the inverse has been calculated previously 
## we will retrieve it rather than recalculate it again.
 
## makeCacheMatrix: returns a list of four functions
## set: stores a matrix into the object
## get: retrieves the matrix from the object
## setinverse: calculated and caches the matrix inverse
## getinverse: retrieves the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize by setting null for matrix inverse      
  m <- NULL
  
  # store a matrix in x set NULL for the inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # retreive the stored matrix
  get <- function() x
  
  # store inverse of of x in m
  setinverse <- function(inverse) m <<- inverse
  
  # retrieve inverse of x (m)
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: returns the inverse of the matrix in the 
## makeCacheMatrix object. Input x is a makeCacheMatrix object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

