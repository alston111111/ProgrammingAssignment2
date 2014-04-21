## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## To create a special object that can cache its inverse.
## Specifically, such an object contains:
## 1. internal member variables that store the matrix and its possibly cached inverse.
## 2. the public functions by calling which we can implement the caching of the inverse of a given matrix.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(y) {
		m <<- y
	}
	getinv <- function(y) m
	list(set = set, get = get,
		setinv = setinv, 
		getinv = getinv)
}

## Write a short comment describing this function
## To return the inverse of the given "matrix".
## If the inverse has been calculated, then this function should retrieve it from the cache.
## x: the special "matrix" created by function makeCacheMatrix
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	# If the inverse of the given matrix has been cached(and the matrix has not changed),
	if( !is.null(inv) ) {
		# just return it.
		return(inv)
	}
	# Else compute the inverse and cache it into the special object, and then return the inverse.
	inv <- solve( x$get() )
	x$setinv( inv )
	inv
}
