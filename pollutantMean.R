## return the mean of column pollutant in all files in id in directory

pollutantMean <- function(directory = "Week 2 data", pollutant = "sulfate", id = 1:332) {
	column <- c()
	for (i in id) {
		filenameI = paste(directory, "/", sprintf("%03d", i), ".csv", sep = "")
		frameI = read.csv(filenameI)
		columnI <- frameI[ , pollutant]
		column <- c(column, columnI)
	}
	mean(column, na.rm = TRUE)
}