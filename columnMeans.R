## return the means of all columns in matrix m, excluding NAs

columnMeans <- function(m) {
	nc <- ncol(m)
	means <- numeric(nc)
	for(i in 1:nc) {
		means[i] <- mean(m[,i], na.rm = TRUE)
	}
	means
}