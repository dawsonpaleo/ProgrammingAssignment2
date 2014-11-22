## This program has two functions that work together to give the inverse of a square 
## matrix.  It assumes the user gives a matrix that has an inverse, otherwise it will 
## yield an error.


## This function will take a matrix as input and cache (or store) it until it is passed 
## to cacheSolve function.  

makeCacheMatrix <- function(x = matrix()) {
        invertmatrix <- NULL
        set <- function(y) {
          
        ##the following points to data objects that will be called by cacheSolve
        x <<- y 
        invertmatrix <<- NULL
  }
        ##sets up 4 functions that will be called by cacheSolve
        get <- function() x
        setinvert <- function(solve) invertmatrix <<- solve  
        getinvert <- function() invertmatrix
        list(set = set, get = get,
        setinvert = setinvert,
        getinvert = getinvert)
}


## This function will take a matrix that has been passed through makeCacheMatrix
## and will give the inverse as output.  Note that it will not take a matrix directly, so
##it needs to be passed through makeCacheMatrix.

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
