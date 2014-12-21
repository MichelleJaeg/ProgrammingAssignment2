## The two functions below cache the inverse of a matrix

# This function takes an invertible matrix and creates a list storing four functions, 
# two to get values (the value of the matrix and the value of it's inverse),
# and two to set those same values.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}

# This function accesses the object created in the above function and calls it's
# get_inverse method. If the inverse of the matrix has been calculated before, 
# that inverse is returned. If not, the inverse is caculated and stored for next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}


        