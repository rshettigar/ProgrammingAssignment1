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
    ## NOTE: Do not round the result!

    #get the directory name to append /
    if(grep("specdata", directory) == 1) {
        directory <- paste(directory, "/", collapse = '', sep = '')
    }  
    
    # Get  the files from the directory and covert to character
    files <- as.character(list.files(directory))
    
    # Generate the path for these files to read in loop as per the id values
    files_path <- paste(directory, files, sep="")
    
    #Initialize the vector to store mean of all the files
    mean_pollutant <- c()
    
    #Loop to read all the files with the id  
    #Calculate the mean of column from each file removing NA
    for (file in id){
        curr_file <- read.csv(files_path[file], header = TRUE)
        na_removed <- curr_file[!is.na(curr_file[,pollutant]),pollutant]
        mean_pollutant <- c(mean_pollutant, na_removed)
    }
    
    #Calculate the final mean
    final_mean <- mean(mean_pollutant)
    return(round(final_mean,3))
    
}
