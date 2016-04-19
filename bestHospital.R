## return hospital in state with lowest 30-day fatality rate in outcome
## the outcomes are heart attack, heart failure, pneumonia

bestHospital <- function(state, outcome) {
	data <- read.csv("Week 4 data/outcome-of-care-measures.csv", colClasses = "character")
	## check for valid state
	if (! state %in% data[,7]) {
		stop("invalid state")
	}
	## check for valid outcome
	if (! outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}
	
	## keep rows state and available, keep columns Hospital.Name and outcome
	if (outcome == "heart attack") {
		subdata <- subset(data, State == state & data[,11] != "Not Available", select = c(2,11))
	}
	
	else if (outcome == "heart failure") {
		subdata <- subset(data, State == state & data[,17] != "Not Available", select = c(2,17))
	}
	
	else if (outcome == "pneumonia") {
		subdata <- subset(data, State == state & data[,23] != "Not Available", select = c(2,23))
	}
	
	## sort by outcome subdata[,2], tie break by Hospital.Name subdata[,1]
	subdatasorted <- subdata[order(as.numeric(subdata[,2]), subdata[,1], na.last = T), ]
	name <- subdatasorted[1,1]
}