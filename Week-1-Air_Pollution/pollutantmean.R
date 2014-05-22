pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    dataset <- data.frame() # create empty data frame
    
    for(i in id) {
        file_number <- as.character(i) # convert iterator to characters
        if(nchar(file_number) < 3) { # add leading zeroes to filenames
            if(nchar(file_number) < 2) {
                file_number = paste0("00", file_number)
            } else {
                file_number = paste0("0", file_number)
            }
        }
        file_name <- paste0(directory, "/", file_number, ".csv") # create a full path for a file
        
        # read a file and merge its content into a data frame
        dataset <- rbind(dataset, read.csv(file_name, header=TRUE))
        
    }
    
    mean_pollution <- mean(dataset[, pollutant], na.rm=TRUE) # calculate mean, omit NA values
    return(mean_pollution) # return calculation
}
