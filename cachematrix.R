## Put comments here that give an overall description of what your
## functions do
## The following two functions create an invertible matrix and, given that 
## the inverse was calculated elsewhere in the matrix and the matrix has not 
## changed, retrieves that value from the cache of the previous calculation 
## instead of doing the same calculation again. This is a useful tool that 
## caches a potentially time-consuming computation.

## Write a short comment describing this function
## This function creates an invertible matrix that can store both its content 
## (x) and its inverse (inv) as well. So there are two variables: x and inv. 
## Each variable has two functions, set and get. Set uses the variable y to  
## assign to the matrix, or variable x, and the same for the inverse value, or 
## variable inv, making these variables accessible only by using the functions 
## set and get.

makeCacheMatrix <- function(x = matrix()) {
  ## Sets up the inverse property
  inv <- NULL
  ## Sets up the empty matrix where y is the input into the matrix x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Method to get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ##Method to set the inverse of the matrix
  setinverse <- function(inverse) {
    inv <<- inverse 
  }
  ##Method to get the inverse of the matrix
  getinverse <- function() {
    ## Return the inverse of the matrix
    inv
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Find the inverse of the matrix returned by "makeCacheMatrix" from the above
## code. If the inverse was previously calculated elsewhere in the matrix,
## and no change has been made to the matrix, then "cacheSolve" should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## Return the inverse if it is already set (i.e, not NULL) by retrieving data
  ## from the cache.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get the matrix of the object in question (x) and store as "mat"
  mat <- x$get()
  ## Calculate the inverse using matrix multiplication
  inv <- solve(mat, ...)
  ## Set the calculated inverse value to the object in question (x)
  x$setinverse(inv)
  ## Return the inverse matrix value
  inv
}
## End assignment. Fin