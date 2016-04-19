## return the correlation between column sulfate and column nitrate
## in each file with at least threshold complete cases in directory

corr <- function(directory = "Week 2 data", threshold = 0) {
	files <- list.files(path = directory, pattern=".csv", all.files = T, full.names = T)
	cr <- numeric(0)
	for (file in files) {
		frame <- read.csv(file)
		f <- na.omit(frame)
		if (sum(complete.cases(f)) > threshold) {
			cr <- c(cr, cor(f$sulfate, f$nitrate))
		}
	}
	cr
}