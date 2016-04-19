## make a vector that has list of functions (methods)
## return its cached mean if it is not null
## or compute, cache and return its mean if its cached mean is null

makeCacheVector <- function(vector = numeric()) {
	mean <- NULL
        
	## set vector as v and mean as null
	set <- function(v) {
		vector <<- v
		mean <<- NULL
	}
        
	## get vector
	get <- function() {
		vector
	}
	
	## set mean, will corrupt cached mean if there is one
	setMean <- function(m) {
		mean <<- m
	}
        
	## get cached mean, will reset to null for new vector
	getMean <- function() {
		mean
	}
        
	## list of functions with these names
	list(set = set, get = get, setMean = setMean, getMean = getMean)
}

getCacheMean <- function(v, ...) {
	## return cached mean if it is not null, it may be corrupted by setMean
	mean <- v$getMean()
	if(!is.null(mean)) {
		message("from cached data")
		return(mean)
	}
	
	## compute, cache and return mean if cached mean is null
	mean <- mean(v$get(), ...)
	v$setMean(mean)
	mean
}