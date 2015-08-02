## makeCacheMatrix contains four anonymous functions for solving, caching, and 
##      retrieving the inverse of a matrix.
## cacheSolve solves inverse or obtains cached matrix of the solved inverse

## *** Much of these functions have been adapted from the instructions for the
##     problem at hand.

## makeCacheMatrix contains four anonymous functions for solving, caching, and 
##      retrieving the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               # Clear m at the beginning of the run
    set <- function(y) {    # create set anonymous function storing whatever is 
        x <<- y             # passed to set into local variable x
        m <<- NULL          # clear local variable m
    }
    get <- function() x     # get local variable x
    # setsolve is an anonymous function to solve the inverse of the matrix that is passed
    setsolve <- function(solve) m <<- solve 
    getsolve <- function() m #getsolve gets and returns cache of solved matrix
    list(set = set, get = get, # return list of four functions
         setsolve = setsolve,
         getsolve = getsolve)
}


## makeCacheMatrix contains four anonymous functions for solving, caching, and 
##      retrieving the inverse of a matrix.

cacheSolve <- function(x, ...) { # Receives matrix x and any arguments 
    ## Return a matrix that is the inverse of 'x'
    # Run getsolve function on object x and assign value to m
    m <- x$getsolve()  
    ## not null indicates data cached already
    if(!is.null(m)) {       
        message("getting inverse matrix")
        return(m)   # Cached data retrieved with getsolve, so just return it
    }
    data <- x$get() # store calling variable x into local variable data
    m <- solve(data, ...) # solve matrix data and assign into m
    x$setsolve(m)       # cache the solved matrix m into calling object x
    m    # return m
}
