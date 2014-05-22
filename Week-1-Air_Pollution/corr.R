corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    files <- list.files(path=directory, full.names=TRUE) # read a list of files in the directory
    correlations <- vector() # create empty vector to store correlations
    
    for(file_name in files) {
        temp_data <- read.csv(file_name, header=TRUE) # read a file
        case_count <- nrow(subset(temp_data[complete.cases(temp_data), ])) # calculate number of complete observations in a file

        if(case_count > threshold) { # if number of complete cases is above threshold,
            clean_data <- subset(temp_data[complete.cases(temp_data), ]) # create a clean data set
            correlations <- c(correlations, cor(clean_data[, "sulfate"], clean_data[, "nitrate"])) # calculate correlations between sulfate and nitrate measurements, append them to the vector
        }
    }
    
    return(as.numeric(correlations)) # return correlations as numeric vector
}

