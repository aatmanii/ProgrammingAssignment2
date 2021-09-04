makeCacheMatrix <- function(x=matrix()) {
    inv <- NULL
    set <- function(t) { 
        x <<- t 
        inv <<- NULL}
    get <- function() x
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function () inv
    list(set = set , get = get , setinv = setinv , getinv = getinv)
}
cashsolve <- function(x,...) {
    inv <- x$getinverse()
    if (!is.NULL(inv)) { 
        message("getting data from cash")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat,...)
    x$setinverse(inv)
    inv
} 
