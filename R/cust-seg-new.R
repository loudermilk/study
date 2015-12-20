
generateData <- function() {
  # original code
  variable_names <- c("age", "gender", "income", "kids", "ownHome", "subscribe")
  variable_distributions <- c("norm", "binom", "norm", "pois", "binom", "binom")
  
  segment_names <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")
  segment_sizes <- c(100, 50, 80, 70)
  
  # means and probabilities (cols = variables, rows = segments)
  segment_means <- matrix( c(
    40, 0.5, 55000, 2, 0.5, 0.1,
    24, 0.7, 21000, 1, 0.2, 0.2,
    58, 0.5, 64000, 0, 0.7, 0.05,
    36, 0.3, 52000, 2, 0.3, 0.2 ), ncol=length(variable_names), byrow=TRUE)
  
  segment_sds <- matrix( c(
    5, NA, 12000, NA, NA, NA,
    2, NA, 5000, NA, NA, NA,
    8, NA, 21000, NA, NA, NA,
    4, NA, 10000, NA, NA, NA ), ncol=length(variable_names), byrow=TRUE)
  
  
  seg_df <- NULL
  set.seed(02554)
  # iterate over segments and create data for each
  for (i in seq_along(segment_names)) {
    cat(i, segment_names[i], "\n")
    
    # empty matrix to hold this particular segment’s data
    this.seg <- data.frame(matrix(NA, nrow=segment_sizes[i], ncol=length(variable_names)))
    
    # within segment, iterate over variables and draw appropriate random data
    for (j in seq_along(variable_names)) { # and iterate over each variable
      if (variable_distributions[j] == "norm") { # draw random normals
        this.seg[,j] <- rnorm(segment_sizes[i], mean=segment_means[i,j], sd=segment_sds[i,j])
      } else if (variable_distributions[j] == "pois") { # draw counts
        this.seg[, j] <- rpois(segment_sizes[i], lambda=segment_means[i, j])
      } else if (variable_distributions[j] == "binom") { # draw binomials
        this.seg[, j] <- rbinom(segment_sizes[i], size=1, prob=segment_means[i, j])
      } else {
        stop("Bad segment data type: ", variable_distributions[j])
      }
    }
    seg_df <- rbind(seg_df, this.seg)
  }
  
  # make the data frame names match what we defined
  names(seg_df) <- variable_names
  # add segment membership for each row
  seg_df$Segment <- factor(rep(segment_names, times=segment_sizes))
  # convert the binomial variables to nicely labeled factors
  seg_df$ownHome <- factor(seg_df$ownHome, labels=c("ownNo", "ownYes"))
  seg_df$gender <- factor(seg_df$gender, labels=c("Female", "Male"))
  seg_df$subscribe <- factor(seg_df$subscribe, labels=c("subNo", "subYes"))
  return(seg_df)
  
}