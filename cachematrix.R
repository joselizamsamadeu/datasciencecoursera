
## Once the inversion of a matrix is very costly computing, we cant write two 
## functions that will cache the matrix inverse that will be used showing it whithout 
## the need too calculate it again

## The function makeCacheMatrix creates a cached inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix 

cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("This Matrix is CAHED")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}

my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()

