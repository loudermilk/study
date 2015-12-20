
generateData <- function() {
  # original code
  segVars <- c("age", "gender", "income", "kids", "ownHome", "subscribe")
  segVarType <- c("norm", "binom", "norm", "pois", "binom", "binom")
  segNames <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")
  segSize <- c(100, 50, 80, 70)
  
  segMeans <- matrix( c(
    40, 0.5, 55000, 2, 0.5, 0.1,
    24, 0.7, 21000, 1, 0.2, 0.2,
    58, 0.5, 64000, 0, 0.7, 0.05,
    36, 0.3, 52000, 2, 0.3, 0.2 ), ncol=length(segVars), byrow=TRUE)
  
  segSDs <- matrix( c(
    5, NA, 12000, NA, NA, NA,
    2, NA, 5000, NA, NA, NA,
    8, NA, 21000, NA, NA, NA,
    4, NA, 10000, NA, NA, NA ), ncol=length(segVars), byrow=TRUE)
  
  
  seg.df <- NULL
  set.seed(02554)
  # iterate over segments and create data for each
  for (i in seq_along(segNames)) {
    cat(i, segNames[i], "\n")
    
    # empty matrix to hold this particular segment’s data
    this.seg <- data.frame(matrix(NA, nrow=segSize[i], ncol=length(segVars)))
    
    # within segment, iterate over variables and draw appropriate random data
    for (j in seq_along(segVars)) { # and iterate over each variable
      if (segVarType[j] == "norm") { # draw random normals
        this.seg[,j] <- rnorm(segSize[i], mean=segMeans[i,j], sd=segSDs[i,j])
      } else if (segVarType[j] == "pois") { # draw counts
        this.seg[, j] <- rpois(segSize[i], lambda=segMeans[i, j])
      } else if (segVarType[j] == "binom") { # draw binomials
        this.seg[, j] <- rbinom(segSize[i], size=1, prob=segMeans[i, j])
      } else {
        stop("Bad segment data type: ", segVarType[j])
      }
    }
    seg.df <- rbind(seg.df, this.seg)
  }
  
  # make the data frame names match what we defined
  names(seg.df) <- segVars
  # add segment membership for each row
  seg.df$Segment <- factor(rep(segNames, times=segSize))
  # convert the binomial variables to nicely labeled factors
  seg.df$ownHome <- factor(seg.df$ownHome, labels=c("ownNo", "ownYes"))
  seg.df$gender <- factor(seg.df$gender, labels=c("Female", "Male"))
  seg.df$subscribe <- factor(seg.df$subscribe, labels=c("subNo", "subYes"))
  return(seg.df)
  
}