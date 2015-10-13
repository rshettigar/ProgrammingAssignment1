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
    
    #get the directory name to append /
    if(grep("specdata", directory) == 1) {
        directory <- paste(directory, "/", collapse = '', sep = '')
    }  
    
    # Get the files from the directory and covert to character
    files <- as.character(list.files(directory))
    
    # Generate the path for these files to read in loop as per the id values
    files_path <- paste(directory, files, sep="")
    id_len <- length(id)
    complete_info <- rep(0, id_len)
    ctr <- 1
    
    #Loop using id to read all the files
    #create complete cases by removing the row that has NA in any column
    for (file in id){
        curr_file <- read.csv(files_path[file], header = TRUE)
        na_removed <- na.omit(curr_file)
        complete_info[ctr] <- nrow(na_removed)
        ctr <- ctr + 1
    }
    result <- data.frame(id = id, nobs = complete_info)
    return(result)

}
