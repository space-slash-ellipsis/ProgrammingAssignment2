## These two functions are a two-step process for 'caching' a matrix's
## inverse.

## This function is the first step in making the inverse of a matrix
## 'cached.'
makeCacheMatrix <- function(x = matrix()) {
        A_inverse <- NULL
        set <- function(q) {
                x <<- q
                A_inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) A_inverse <<- solve
        getinverse <- function() A_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is the second step in making the inverse of a
## matrix 'cached' meaning that it doesn't have to be recalculated
## to be accessed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        A_inverse <- x$getinverse()
        if(!is.null(A_inverse)) {
                message("retrieving")
                return(A_inverse)
        }
        
        p <- x$get()
        A_inverse <- solve(p, ...)
        x$setinverse(A_inverse)
        A_inverse
}
