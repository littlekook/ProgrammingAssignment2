

# Save time for Matrix inversion with caching the inverse of a matrix!

# this skript contains two functions, the first creates the matrix and inverse matrix
# the second returns the inverse of the matrix with comparing the cache




# makecacheSolve creates a list containing a function to set and get value of matrix and inverse matrix

makecacheSolve <- function(x = matrix()) {
  inv <- NULL
  set <- function(x2) {
    x <<- x2
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve function returns inverse of the matrix
# if it isn't cached yet it calculates it
# This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}





# Test:
# > x = rbind(c(1, 2), c(3, 4))
# > m = makecacheSolve(x)

# No cache in the first run
# > cacheSolve(m)
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

# Retrieving from the cache in the second run
# > cacheSolve(m)
# getting cached data.
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
