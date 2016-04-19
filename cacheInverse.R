## make a matrix that has list of functions (methods)
## return its cached inverse if it is not null
## or solve, cache and return its inverse if its cached inverse is null

makeCacheMatrix <- function(matrix = matrix()) {
	inverse <- NULL
	
	## set matrix as m and inverse as null
	set <- function(m) {
		matrix <<- m
		inverse <<- NULL
	}
	
	## get matrix
	get <- function() {
		matrix
	}
	
	## set inverse, will corrupt cached inverse if there is one
	setInverse <- function(i) {
		inverse <<- i
	}
	
	## get cached inverse, will reset to null for new matrix
	getInverse <- function() {
		inverse
	}
	
	## list of functions with these names
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

getCacheInverse <- function(m, ...) {	
	## return cached inverse if it is not null, it may be corrupted by setInverse
	inverse <- m$getInverse()
	if (!is.null(inverse)) {
		message("from cached data")
		return(inverse)
	}
	
	## compute, set and return inverse if cached inverse is null
	inverse <- solve(m$get(),...)
	m$setInverse(inverse)
	inverse
}