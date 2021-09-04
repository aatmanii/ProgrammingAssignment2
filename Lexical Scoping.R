makeCacheMatrix <- function(x=matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y 
        inv <<- NULL}
    get <- function() x
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function () inv
    list(set = set , get = get , setinv = setinv , getinv = getinv)
}
cashsolve <- function(x,...) {
    inv <- x$getInverse()
    if (!is.NULL(inv)) { 
        message("getting data from cash")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
} 
