# mep-simulation.R
# Utilities for simulating data


#' @title Simulate Correlated Data
#' 
#' @description Create data.frame of simulated data
#' with correlations among independent variables and the
#' dependent variable.
#' 
#' @param num_var integer number of variables/cols
#' @param num_obs integer number of observations/rows
#' @param col_names character vector of column names
#' @return data.frame of correlated simulated data
#' 
simCorrelatedData <- function(num_var, num_obs, col_names = NULL) {
  DEF_IV_NAME <- "Predictor"
  DEF_DV_NAME <- "Response"
  
  # Check column names and auto-generate if needed
  if (!is.null(col_names)) {
    if (length(col_names) != num_var) {
      warning("Unexpected # of column names: Using default column names")
      col_names <- NULL
    }
  } else {
    col_names <- paste0(DEF_IV_NAME, 1:(num_var-1))
    col_names <- c(col_names, DEF_DV_NAME)
  }
  
  # Create matrix with positive values
  random_matrix <- Matrix::Matrix(rnorm(num_var^2, mean = .5, sd = .1), 
                          num_var)
  
  # Set diagonal to identity and make symmetric
  for (i in seq_len(ncol(random_matrix))) {
    random_matrix[i,i] <- 1
  }
  correlation_matrix <- Matrix::forceSymmetric(random_matrix)  

  # Decompose via Cholesky
  cholesky_matrix <- Matrix::chol(correlation_matrix)
  out_matrix <- t(cholesky_matrix) %*% matrix(rnorm(num_var*num_obs), 
                                nrow=num_var, ncol=num_obs)
  out_matrix <- t(out_matrix)
  out_matrix <- as.matrix(out_matrix)
  out_df = as.data.frame(out_matrix)

  names(out_df) <- col_names
  return(out_df)
}

