library(roxygen2)

# original code
DEF_VAR_NAMES <- c("age", "gender", "income", "kids", "ownHome", "subscribe")
DEF_VAR_DISTS <- c("norm", "binom", "norm", "pois", "binom", "binom")

# replace strings in DEF_VAR_DISTS with functions in DEF_VAR_DISTS_FUNS
DEF_VAR_DISTS_FUNS <- c(rnorm, rbinom, rnorm, rpois, rbinom, rbinom)

DEF_SEG_NAMES <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")
DEF_SEG_SIZES <- c(100, 50, 80, 70)

# means and probabilities (cols = variables, rows = segments)
DEF_SEG_MEANS <- matrix( c(
  40, 0.5, 55000, 2, 0.5, 0.1,
  24, 0.7, 21000, 1, 0.2, 0.2,
  58, 0.5, 64000, 0, 0.7, 0.05,
  36, 0.3, 52000, 2, 0.3, 0.2 ), ncol=length(DEF_VAR_NAMES), byrow=TRUE)

DEF_SEG_SDS <- matrix( c(
  5, NA, 12000, NA, NA, NA,
  2, NA, 5000, NA, NA, NA,
  8, NA, 21000, NA, NA, NA,
  4, NA, 10000, NA, NA, NA ), ncol=length(DEF_VAR_NAMES), byrow=TRUE)


#' @title Simulate dummy marketing analtics data
#' 
#' @description Return data.frame of simulated, segmented marketing data
#' 
#' @details Given the sample segmentation, out_df has columns for each
#' of the vars in variable_names plus a single column for 'Segment'. out_df 
#' has rows equal to the sum of the segment sample counts in segment_sizes. 
#' 
#' @param variable_names character vector of demographic variable names
#'        (e.g., c("age","gender"))
#' @param variable_distributions character vector of distribution names
#'        (e.g., c("norm", "binom"))
#' @param segment_names character vector of segment names
#'        (e.g., c("Suburb mix", "Urban hip"))
#' @param segment_sizes integer vector of segment sample sizes
#'        (e.g., c(100, 50))
#' @param segment_means matrix of variable means and probabilities
#' @param segment_sds matrix of variable SDs
#' 
#' @return seg_df data.frame of simulated data
#'        
generateData <- function(variable_names = DEF_VAR_NAMES, 
                         variable_distributions = DEF_VAR_DISTS,
                         segment_names = DEF_SEG_NAMES, 
                         segment_sizes = DEF_SEG_SIZES, 
                         segment_means = DEF_SEG_MEANS,
                         segment_sds = DEF_SEG_SDS) {
  
  
  
  seg_df <- NULL
  set.seed(02554)
  # iterate over segments and create data for each
  for (i in seq_along(segment_names)) {
    
    # write out segment names
    cat(i, segment_names[i], "\n")
    
    # empty matrix to hold this particular segment’s data
    df <- data.frame(matrix(NA, 
                            nrow=segment_sizes[i], 
                            ncol=length(variable_names)))
    
    # within segment, iterate over variables and draw appropriate random data
    for (j in seq_along(variable_names)) { # and iterate over each variable
      df[, j] <- getDistribution(variable_distributions = variable_distributions,
                                 segment_sizes = segment_sizes,
                                 segment_means = segment_means,
                                 segment_sds = segment_sds, 
                                 i, 
                                 j)
    }
    seg_df <- rbind(seg_df, df)
  }
  
  # make the data frame names match what we defined
  names(seg_df) <- variable_names
  
  # TODO: THIS IS HARD-WIRED BELOW e.g., seg_df$ownHome
  # WHAT HAPPEMS WHEN DEFAULT VARIABLES CHANGE?
  
  # add segment membership for each row
  seg_df$Segment <- factor(rep(segment_names, times=segment_sizes))
  
  # convert the binomial variables to nicely labeled factors
  seg_df$ownHome <- factor(seg_df$ownHome, labels=c("ownNo", "ownYes"))
  seg_df$gender <- factor(seg_df$gender, labels=c("Female", "Male"))
  seg_df$subscribe <- factor(seg_df$subscribe, labels=c("subNo", "subYes"))
  
  return(seg_df)
  
}

#' @title Generate a random distribution given parameters
#' 
#' @param variable_distributions character vector of distribution 'names'
#' @param segment_sizes integer vector of sample sizes
#' @param segment_means numeric vector of means and probabilities
#' @param segment_sds numeric vector of SDs
#' @param i integer segment index
#' @param j integer variable index
#' 
#' @return vector of simulated variable values
getDistribution <- function(variable_distributions, 
                            segment_sizes, 
                            segment_means, 
                            segment_sds, 
                            i, 
                            j) {
  
  if (variable_distributions[j] == "norm") { # draw random normals
    
    out_dist <- rnorm(segment_sizes[i], 
                     mean=segment_means[i,j], 
                     sd=segment_sds[i,j])
    
  } else if (variable_distributions[j] == "pois") { # draw counts
    
    out_dist <- rpois(segment_sizes[i], 
                     lambda=segment_means[i, j])
    
  } else if (variable_distributions[j] == "binom") { # draw binomials
    
    out_dist <- rbinom(segment_sizes[i], 
                      size=1, 
                      prob=segment_means[i, j])
  } else {
    
    stop("Bad segment data type: ", variable_distributions[j])
  }
  return(out_dist)
}