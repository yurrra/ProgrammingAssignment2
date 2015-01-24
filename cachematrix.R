## The first function creates an object of a matrix
## The second function outputs inverse matrix for a given matrix. The argument shall be a matrix of a first function type.
## If the inverse matrix has already been calculated, then cached value is used as an output in order to save calculation time.

## The function creates an object of a matrix. You can assign or reassign matrix value by $set

makeCacheMatrix <- function(x = matrix() ) {
        m <- NULL  ##inverse matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(invrs) m <<- invrs
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## It returns inverse matrix of a matrix created in the function above. In case it was previously calculated, cached value will be taken.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
