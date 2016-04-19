## return hospital in each state with n^th lowest 30-day fatality rate in outcome
rankAll <- function(outcome, num = "best") {
	data <- read.csv("Week 4 data/outcome-of-care-measures.csv", colClasses = "character")
	## check for valid outcome
	if (! outcome %in% c("heart attack", "heart failure", "pneumonia")) {
		stop("invalid outcome")
	}
	
	## keep rows available, keep columns Hospital.Name, State and outcome
	if (outcome == "heart attack") {
		subdata <- subset(data, data[,11] != "Not Available", select = c(2,7,11))
	}
	else if (outcome == "heart failure") {
		subdata <- subset(data, data[,17] != "Not Available", select = c(2,7,17))
	}
	else if (outcome == "pneumonia") {
		subdata <- subset(data, data[,23] != "Not Available", select = c(2,7,23))
	}
	
	## split subdata into list of subframes by states subdata[,2] and apply rank
	## return hospital in each state with n^th lowest 30-day fatality rate in outcome 
	lapply(split(subdata, subdata[,2]), rank, num = num)
}

## sort by outcome subdata[,3], tie break by Hospital.Name subdata[,1]
rank <- function(subdata, num) {
	subdatasorted <- subdata[order(as.numeric(subdata[,3]), subdata[,1], na.last = T), ]
	if (num == "best") {
        name <- subdatasorted[1,1]
    }   
    else if (num == "worst") {
        name <- subdatasorted[nrow(subdatasorted), 1]
    } 
    else {
        name <- subdatasorted[num,1]
    }
}