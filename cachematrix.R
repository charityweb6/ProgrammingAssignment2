## This code creates an object with four methods (set, get, setinverse,
## and getinverse), which, respectively, set and get a matrix, and get
## and set the matrix's inverse, caching the value of the inverse the
## first time it is calculated with "solve", and then returning the
## cached value each time it is called for until the matrix value is
## changed with the "set" function.

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
