## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an object, which is a list of functions
## for getting and setting the matrix itself, as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y) {
		x <<- y
		invx <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invx <<- inverse
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks whether the invx element has already been
## calculated for the matrix x, and if it hasn't, it uses solve to
## calculate it and caches the value

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x', calculating
	## and caching it if it hasn't already been cached
	invx <- x$getinverse()
	if (!is.null(invx)) {
		message("getting cached inverse")
		return(invx)
	}
	matrix <- x$get()
	invx <- solve(matrix, ...)
	x$setinverse(invx)
	invx
}
