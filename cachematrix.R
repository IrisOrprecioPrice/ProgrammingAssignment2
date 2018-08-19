## The two functions below when used in conjunction
## with each other, uses lexical scoping and the 
## assignment symbol <<- to cache inverses of 
## matrices and retrieve them when needed, or 
## compute inverses of matrices that aren't cached.

## This first function takes a matrix and returns 
## something like a new "data type" where the 
## matrix fed into it is now attached with a list of
## four things, the setter and getter of the said 
## function, and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}

## This second function takes as argument an object
## that has essentially "data type" set by the above
## function. It returns either a cached inverse 
## (if it has been previously computed) or computes
## the inverse and returns that.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  newmat <- x$getmat()
  inv <- solve(newmat, ...)
  x$setinv(inv)
  inv
}