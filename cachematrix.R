## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x: assuming a square invertible matrix
  inv = NULL
  ## Function for setting value
  set = function(y) {
    # assigning value to x
    x <<- y
    # setting inv as null
    inv <<- NULL
  }
  ## Function for fetching value
  get = function() x
  ## Set inverse of x
  setinv = function(inverse) inv <<- inverse 
  ## fetch inverse of x
  getinv = function() inv
  
  ## return: list containing following functions 
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## x: result of function makeCacheMatrix()
 
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    message("getting cached inverse matrix")
    ## return: inverse of matrix 
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data = x$get()
  
  ## check martix is square 
  ## can use is.square.matrix(x) function as well if matrixcalc lib is installed
  if (nrow(data) != ncol(data)) {
    message("not a square matrix")
    return (NULL)
  }
  
  ## install matrixcalc using --- install.package('matrixcalc')
  ## include --- library('matrixcalc') 
  ## above library facilitate is.singular.matrix function
  if (is.function(is.singular.matrix) == TRUE) {
    if (is.singular.matrix(data) == TRUE) {
      message("matrix is singular")
      return (NULL)
    }
  }
  
  inv = solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  ## return: inverse of matrix 
  return(inv)
  
}
