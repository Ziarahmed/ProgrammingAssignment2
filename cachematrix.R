#These are a couple of functions that reduce computation time for calculating the inverse of a matrix.

# #the first one creates a set of functions that get and set the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) s <<- inverse
  get_inverse <- function() s
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}
# #this one gives the inverse of the matrix but before it checks if it was already calculated,

  #if so it gets it ,and if not it calculates it's inverse and store it in the cache for later reuse.

cacheSolve <- function(x, ...) {
  s <- x$get_inverse()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <-solve(data, ...)
  x$set_inverse(s)
  s
}

