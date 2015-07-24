## The functions makeCacheMatrix and cacheSolve work together to cache the inverse
## of a matrix

## The makeCacheMatrix function uses 4 functions to input a matrix and cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y
			i <<- NULL
		}	
	get <- function() x
	setinverse <- function(solve) i <<- solve
    getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function retrieves the cached inverse of the matrix from the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
