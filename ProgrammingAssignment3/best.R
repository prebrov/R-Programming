best <- function(state, outcome) {
    ## read the data file
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check that state and outcome data are valid
    if(!(state %in% data[, 7])) stop("invalid state") ## check if state is present in the data
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome") ## check if outcome is one of the listed
    
    if(outcome == "heart attack") {
        rates <- subset(data, data$State == state, select = c(2, 11)) ## subset heart rate cases for specified state
    } else if(outcome == "heart failure") {
        rates <- subset(data, data$State == state, select = c(2, 17)) ## subset heart failure cases for specified state
    } else {
        rates <- subset(data, data$State == state, select = c(2, 23)) ## subset pneumonia cases (not one of the above, but still on the list) for specified state
    }

    ## return hospital name in that state with lowest 30-day death rate
    rates[, 2] <- as.numeric(rates[, 2]) ## convert rates to numeric
    print(rates[which.min(rates[, 2]), 1]) ## find where minimum is, print Hospital Name for that
}
