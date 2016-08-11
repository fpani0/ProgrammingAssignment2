
## makeCacheMatrix - function used to store a cache copy of it's matrix and it's inverse; there are 4 objects
#  1. set        - set the matrix 
#  2. get        - returns the matrix 
#  3. setinverse - set the value of the inverse matrix 
#  4. getinverse - returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mat) m <<- mat
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve - function to solve the inverse of the matrix unless it is already cached at which the cache value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}



