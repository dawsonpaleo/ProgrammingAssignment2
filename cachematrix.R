## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invertmatrix <- NULL
        set <- function(y) {
        x <<- y
        invertmatrix <<- NULL
  }
        get <- function() x
        setinvert <- function(solve) invertmatrix <<- solve
        getinvert <- function() invertmatrix
        list(set = set, get = get,
        setinvert = setinvert,
        getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertmatrix <- x$getinvert()
        if(!is.null(invertmatrix)) {
                message("getting cached data")
                return(invertmatrix)
        }
        data <- x$get()
        invertmatrix <- solve(data, ...)
        x$setinvert(invertmatrix)
        invertmatrix
}
