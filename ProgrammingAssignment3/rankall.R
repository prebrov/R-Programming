rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome") ## check if outcome is one of the listed

    if(outcome == "heart attack") {
        data <- subset(data, select = c(2, 7, 11)) ## subset only data we'll be working with
    } else if(outcome == "heart failure") {
        data <- subset(data, select = c(2, 7, 17)) ## subset only data we'll be working with
    } else {
        data <- subset(data, select = c(2, 7, 23)) ## subset only data we'll be working with
    }

    names(data) <- c("hospital", "state", "rate") ## name columns
    data$rate <- as.numeric(data$rate) ## convert observations to numeric
    data <- na.omit(data) ## drop NA values
    
    ## For each state, find the hospital of the given rank
    bystate <- split(data, data$state) ## split dataset by state

    if(num == "best") num <- 1
    
    ranked_states <- lapply(bystate, function(x, y) {
        x <- x[order(x$rate, x$hospital, na.last=TRUE), ] ## sort data frame by Rate column, then by Hospital
        if(y == "worst") y <- length(x$rate) ## if "worst" is given, then assume it the last item in this particular state sorted list
        x[y, 2] <- x$state[1] ## put state name into requested row, even if it's empty
        results <- rbind(results, x[y, c(1, 2)]) ## add to results data frame
    }, y = num)

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    do.call(rbind, ranked_states)
}
