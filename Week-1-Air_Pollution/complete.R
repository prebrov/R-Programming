complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    files <- list.files(path=directory, full.names=TRUE) # read a list of files in the directory
    dataset <- data.frame() # create empty data frame for dataset
    case_count <- data.frame() # create empty data frame to fill in with complete case counts

    for(file_name in files) {
        dataset <- rbind(dataset, read.csv(file_name, header=TRUE)) # read a file and merge its content into a data frame
    }
    
    for(i in id) {
        count <- c(i, nrow(subset(dataset[complete.cases(dataset), ], ID==i))) # create vector containing ID and number of rows matching ID in dataset, being a complete case
        case_count <- rbind(case_count, count) # add vector to the data frame as row
    }
    
    colnames(case_count) <- c("id", "nobs") # name columns
    return(case_count)
    
}