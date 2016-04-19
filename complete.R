## return the number of complete observations in each file in id in directory

complete <- function(directory = "Week 2 data", id = 1:332) {
	ID <- c()
	nobs <- c()
	for (i in id) {
		filenameI = paste(directory, "/", sprintf("%03d", i), ".csv", sep = "")
		frameI = read.csv(filenameI)
		ID <- c(ID, i)
		nobs <- c(nobs, sum(complete.cases(frameI)))
		}
	data.frame(ID, nobs)
}