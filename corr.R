corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    #get the directory name to append /
    if(grep("specdata", directory) == 1) {
        directory <- paste(directory, "/", collapse = '', sep = '')
    }
    
    #call complete script to get complete cases/nobs
    complete_tb <- complete("specdata", 1:332)
    nobs <- complete_tb$nobs
    
    
    #Get all the ids where the complete cases are greater than threshold
    ids <- complete_tb$id[nobs > threshold]
    
    #get the length of ids
    id_len <- length(ids)
    
    #initialize the correlation vector
    correlation <- rep(0, id_len)
    
    # Get the files from the directory and covert to character
    files <- as.character(list.files(directory))
    
    # Generate the path for these files to read in loop as per the id values
    files_path <- paste(directory, files, sep="")
    
    ctr <- 1
    for(id in ids){
        curr_file <- read.csv(files_path[id], header = TRUE)
        correlation[ctr] <- cor(curr_file$sulfate, curr_file$nitrate, use="complete.obs")
        ctr <- ctr+1
    }
    result_cor <- correlation
    return(result_cor)
    
}