## Cache the value of a matrix, and its inverse, with the ability to return 
## or reset the value of the matrix and its inverse

## makeCacheMatrix: Cache the value of the matrix x (and its inverse)
## and store a list of functions to:
##    1) set the value of x (and reset the value of x's inverse)
##    2) retrieve ('get') the value of x
##    3) set the inverse of x to a given value 
##    4) retrieve the inverse of x (stored in 'Inv')
## Note that this function does not calculate the inverse of a matrix
## Calculation of the inverse must be done in cacheSolve and
## passed to the variable'Inv' via  the 'setInv' function

makeCacheMatrix <- function(x = matrix(numeric(),0,0)) {
      Inv <- NULL
      set <- function(y) {
            x <<- y
            Inv <<- NULL
      }
      get <- function() x
      setInv <- function(Inverse) Inv <<- Inverse
      getInv <- function() Inv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## cacheSolve: Returns the inverse of the matrix stored in fx (an 
## instance of makeCacheMatrix) if it exists, and if it does not yet exist,
## calculate the inverse (using the 'solve' function) and cache it.

cacheSolve <- function(fx, ...) {
      Inv <- fx$getInv()
      if(!is.null(Inv)) {
            message("getting cached data")
            return(Inv)
      }
      data <- fx$get()
      Inv <- solve(data, ...)
      fx$setInv(Inv)
      Inv
}

