## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates and returns a list of functions
## cacheSolve is to set or get the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  # create the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store it in cache
  setinverse<- function(inverse) inv_x <<-inverse
  # get the inverted matrix from cache
  getinverse <- function() inv_x
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(inv_x)) {
    message("Getting cached data from inverse matrix")
    return(inv_x)
  } 
  
  # create matrix if it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and can be inverted
  tryCatch( {
    
    # set and return inverse of matrix
    inv_x <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in inv_x
    x$setinverse(inv_x)
  } )
  
  # display matrix
  return (inv_x)
  
  
}
