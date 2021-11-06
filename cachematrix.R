    ## sets value of matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ## gets value of matrix
    get <- function() x
    ## sets value of inverse
    setinv <- function(inverse) inv <<- inverse
    ## gets value of inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns an inversed matrix by checking
##to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value 
##of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## checks to see if inverse has been calculated
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    ## calculates inverse if it hasn't been calculated
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
}
