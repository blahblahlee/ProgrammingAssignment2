# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  inv <- NULL
  # Set the matrix
  set <- function(y) {
    x <<- y
    # Reset the inverse property
    inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  # Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  # Get the inverse of the matrix
  getInverse <- function() inv
  # Return a list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Get the inverse property of x
  inv <- x$getInverse()
  # If it is not NULL, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise, calculate the inverse using solve()
  mat <- x$get()
  inv <- solve(mat, ...)
  # Set the inverse property of x
  x$setInverse(inv)
  # Return the inverse
  inv
}


# Create a 2x2 matrix
A <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
# Create a special matrix object using makeCacheMatrix
B <- makeCacheMatrix(A)
# Get the inverse of B using cacheSolve
cacheSolve(B)



