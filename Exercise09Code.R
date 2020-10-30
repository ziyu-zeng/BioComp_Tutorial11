# R Code for Exercise 9

coeff <- function(dir, colnum, override = F){
  # Set working directory to dir
  setwd(dir)
  # Create a list of files
  filelist <- list.files()
  # Initialize the vector of coefficients
  coeffvec <- numeric(length(filelist))
  # Initialize an error marker
  no_error = TRUE
  
  for (i in 1:length(filelist)){
    temp <- read.csv(filelist[i], header = TRUE, stringsAsFactors = FALSE)
    typeof(temp)
    # Check if the specified column exists
    if(colnum > ncol(temp)){
      print(paste(filelist[i], " Error: Specified column does not exist"))
      no_error = FALSE
    }
    # Check if there are more than 50 non-NA observations
    if(length(!is.na(temp[colnum]))<50){
      print(paste(filelist[i], " Warning: Sample size is less than 50."))
      if(override == FALSE){
        print(paste(filelist[i], " Error: No override permission."))
        no_error = FALSE
      }
    }
    if(no_error == TRUE){
      datavec <- as.double(temp[,colnum])
      # Calculate variation coefficient
      coeffvec[i] = sd(datavec, na.rm = TRUE)/mean(datavec)
    }else{
      coeffvec[i] = NA
    }
  }
  return(coeffvec)
}