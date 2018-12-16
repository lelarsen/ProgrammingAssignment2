## Put comments here that give an overall description of what your
## functions do
## These Functions are for the completion of the Peer Graded Assignment in week 3 of R Programming

## Write a short comment describing this function
## This function will create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { x <<- y
                            inv <<- Null}
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if( !is.NULL(inv)) {message( "getting the Cached Data")
                        return(inv) }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
