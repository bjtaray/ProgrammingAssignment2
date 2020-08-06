## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function is suppose to create a special object "matrix" that stores a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # setting the inverse to be null
  inv  <- NULL 
  set <- function(y) {
    ## setting the vector, x, to a new vector, y, 
    x <<- y
    ## resets I to null 
    inv <<- NULL
  }
  # return the vector, x 
  get <- function() x
  # sets the inverse, inv, to inverse
  setinverse <- function(inverse) inv <<- inverse
  # will return the inverse, inv
  getinverse <- function() inv
  # return the special matrix containing all the functions we just defined above.
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve should retrieve the inverse from the cache.
##  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## set the inverse of the x matrix 
  inv <- x$getinverse()
  ## checks in the inverse has already been calculated 
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if the inverse has not been calculated than, calculate the inverse
  matrix.data <- x$get()
  inv <- solve(matrix.data, ...)
  ## sets the value of the inverse in the cache via the setinverse function
  x$setinverse(inv)
  inv
}
