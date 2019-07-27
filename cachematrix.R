## The first function allows us to cache the matrix object and its inverse into the global environment.
## An makeCacheMatrix object is created to cache the inverse and store this information in a list which
## contains all the functions associated.
## The second function is passed the object of the makeCacheMatrix function. It computes the inverse
## either through the cached data or calculates it and stores it for a new matrix.

## This function creates the matrix object. It does the following things :
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the inverse of the given matrix
##      4. get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(solve) m <<- solve
        get_inverse_matrix <- function() m
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)

}


## This function returns the inverse matric either through the cached data or calculates the inverse and caches
## it for a matrix whose inverse is not present.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse_matrix()
        if(!is.null(m)) {
          message("Getting cached data.")
          return(m)
        } else {
          data <- x$get()
          m <- solve(data, ...)
          x$set_inverse_matrix(m)
          return(m)
        }
}
