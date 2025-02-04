## Both function in this file creates a special matrix that can cache its inverse
## and then the second function checks for that inverse if there is non, it calculates 
## the inverse for the updated matrix

## This function create a special matrix and then it caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set= set, get = get, setinv = setinv, getinv = getinv)

}


## This function checks for cached inverse of the matrix and calculates the inverse
## if there are non

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
            message('getting cached data')
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}