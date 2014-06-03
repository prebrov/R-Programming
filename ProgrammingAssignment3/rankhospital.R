rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!(state %in% data[, 7])) stop("invalid state") ## check if state is present in the data
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome") ## check if outcome is one of the listed
    
    if(outcome == "heart attack") {
        rates <- subset(data, data$State == state, select = c(2, 11)) ## subset heart rate cases for specified state
    } else if(outcome == "heart failure") {
        rates <- subset(data, data$State == state, select = c(2, 17)) ## subset heart failure cases for specified state
    } else {
        rates <- subset(data, data$State == state, select = c(2, 23)) ## subset pneumonia cases (not one of the above, but still on the list) for specified state
    }
    
    names(rates)[2] <- "Rate" ## Set name for rate
    rates$Rate <- as.numeric(rates$Rate) ## convert rates to numeric
    rates <- na.omit(rates) ## omit NA values
    rates <- rates[order(rates$Rate, rates$Hospital.Name, na.last=TRUE), ] ## sort data frame by Rate column, then by Hospital.Name
    rates <- cbind(rates, 1:dim(rates)[1]) ## add columns with rankings
    names(rates)[3] <- "Rank" ## Set name for Ranks
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    if(num == "best") num <- 1
    if(num == "worst") num <- length(rates$Rank)
    if(num > length(rates$Rank)) return(NA)
    
    return(rates[rates$Rank == num, 1])
    
}
