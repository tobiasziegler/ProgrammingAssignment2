## The following functions allow the inverse of a matrix to be cached
## to improve performance on subsequent retrieval.

## This function creates a list of four functions to store
## and retrieve the value of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialise an object to store the inverse
  set <- function(y) {
    x <<- y # Store the matrix value
    inv <<- NULL # Clear any stored value for the inverse
  }
  get <- function() x # Return the stored matrix
  setinverse <- function(inverse) inv <<- inverse # Store the inverse value
  getinverse <- function() inv # Return the stored inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # Return the list of get/set functions
}


## This function returns the inverse of a cached matrix
## that has been created using the makeCacheMatrix function.
## If the inverse has been calculated previously then the
## cached value will be returned to avoid repeated
## computation. Otherwise it will calculate and cache it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Retrieve any cached value for the inverse
  if(!is.null(inv)) { # If there is a cached inverse
    message("getting cached data") # Indicate the cache is being used
    return(inv) # Return the cached inverse
  }
  data <- x$get() # Retrieve the stored matrix
  inv <- solve(data, ...) # Calculate the inverse of the matrix
  x$setinverse(inv) # Cache the newly-calculated inverse
  inv # Return the inverse
}
