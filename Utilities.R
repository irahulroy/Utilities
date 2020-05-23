
## --------------------------------------------------------------------------------------------------

# function to calculate NA values column-wise in a dataframe
# function returns count and proportion of NA values in the data for each column 
na_values <- function(data){
  # create empty matrix to store NA values
  na_vals <- matrix(0, nrow =1, ncol = ncol(data))
  
  # populate each column of empty matrix with count of NA values
  for(i in 1:ncol(data)){
    na_vals[, i] = sum(is.na(data[, i]))
  }
  
  # convert the matrix to data frame and assign column names to the data frame 
  na_vals <- as.data.frame(na_vals)
  colnames(na_vals) <- colnames(data)
  
  # create data frame containing variable names, NA value count and proportion
  # save the data frame to global environment
  na_vals <<- data.frame(variable = colnames(na_vals), 
                         na_val = t(na_vals), 
                         prop_na = t(100*na_vals/nrow(data)), 
                         row.names = NULL)
}

## --------------------------------------------------------------------------------------------------

# function to scale data
# data should not contain missing values
# method = 'standardize' for standardization of data using mean and standard deviation
# method = 'normalize' for normalization of data using minimum and maximum values
# param_data = data whose parameters will be used for scaling
# scale_data = data to be scaled
scaling <- function(param_data, scale_data, method){
  # check for method
  if(method == "standardize"){
    # standardization using mean and standard deviation
    scaled_data <- data.frame(scale(scale_data,
                                    center = apply(param_data, 2, mean),
                                    scale = apply(param_data, 2, sd)))
  }else if(method == "normalize"){
    # normalization using min-max
    scaled_data <- data.frame(scale(scale_data,
                                    center = apply(param_data, 2, min),
                                    scale = apply(param_data, 2, function(x){max(x) - min(x)})))
  }
  
  # save scaled data to global environment
  assign(paste0("data_", method), scaled_data, inherits = T)
}

