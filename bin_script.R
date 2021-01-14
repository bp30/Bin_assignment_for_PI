# Load packages (NOTE:if you haven't installed tidyverse previously, just uncomment the line of code immediately following this line)
# install.packages (tidyverse)
library (tidyverse)

# Set directory modified when necessary 
setwd("C:/Users/dpen466/Google Drive/Phd (1)/Share_with_Chris/Bin_assignment_for_PI/")

# Functions to assign trials to bins, number of trials in each bin is depended on the remainder value. 
bin_assign <- function (df, bin_n){
  if (sum (df$CueFixation_mean < 0) != 0) {
    start_trialn <- end(which (df$CueFixation_mean < 0))[1] + 1
  } else {
    start_trialn <- 1
  } 
  # Obtain total number of trials, number of trials per bin
  trial_n <- sum (df$CueFixation_mean >= 0)
  if (trial_n < bin_n) {
    bin_n <- trial_n
  } 
  bin_size <- trial_n %/% bin_n
  remainder <- trial_n %% bin_n 
  if (remainder == 0 & trial_n > 0) {
    for (bin in 1:bin_n){
      end_trialn <- start_trialn + bin_size
      df$bins[start_trialn:(end_trialn - 1)] <- bin
      start_trialn <- end_trialn 
    }
  } else if (remainder != 0 & trial_n > 0) {
    rbin_size <- bin_size + 1
    for (bin in 1:remainder) {
      end_trialn <- start_trialn + rbin_size
      df$bins[start_trialn:(end_trialn - 1)] <- bin
      start_trialn <- end_trialn 
    }
    for (bin in (remainder + 1):bin_n){
      end_trialn <- start_trialn + bin_size
      df$bins[start_trialn:(end_trialn - 1)] <- bin
      start_trialn <- end_trialn 
    }
  } else {
    df$bins <- 0
  }
  table_n <- c(n, df$Trial.N.Congruency_mean[1], trial_n, bin_size, remainder)
  return (list(df, table_n))
}

# Load data
FET.df <- read.csv("FET - Manual and Gaze - Data - Trial Level - ACC and PT.ACC - 2021.1.11.csv", header = T)
# FET.df$Sequence <- seq(1, dim(FET.df)[1])

# Set all NA values to -99 in CueFixation_mean
FET.df$CueFixation_mean[is.na(FET.df$CueFixation_mean)] <- -99
FET.df$bins[FET.df$CueFixation_mean < 0] = 0

# Specify bin size, modify if necessary
bin_n = 5

# Identify each participant
subject_n <- unique (FET.df$SubNum)

# Define empty dataframes and vectors for the loop
FET_new.df <- data.frame()
table_n <- data.frame (Subject = 0,Congruency = 0, trial_n = 0, bin_size=0, remainder = 0)

# Assign appropriate bins according to CueFixation_mean for each participant 
for  (n in subject_n) {
  print (paste("Processing subject: ", n))
  # Obtain subseted data set for each subject
  cong.df <- FET.df %>% filter (SubNum == n, Trial.N.Congruency_mean == 1) %>% arrange (CueFixation_mean)
  incong.df <- FET.df %>% filter (SubNum == n, Trial.N.Congruency_mean == 0) %>% arrange (CueFixation_mean)

  cong_bins <- bin_assign(cong.df, bin_n)[[1]]
  incong_bins <- bin_assign(incong.df, bin_n)[[1]]
  FET_new.df <- rbind.data.frame (FET_new.df, cong_bins, incong_bins)
  
  cong_file <- bin_assign (cong.df, bin_n)[[2]]
  incong_file <- bin_assign (incong.df, bin_n)[[2]]
  table_n <- rbind.data.frame(table_n, incong_file, cong_file)
}

#FET_new.df <- FET_new.df [order(FET_new.df$Sequence),]

varify_file <- FET_new.df %>% group_by (SubNum, Trial.N.Congruency_mean) %>% count (bins)

# Save files
write.csv (table_n[2:dim(table_n)[1], ], "FET_summary.csv", row.names = F)
write.csv (varify_file, "FET_varify.csv", row.names = F)
write.csv(FET_new.df, "FET_TrialLevelDataACC.PTACC_bins.csv", row.names = F)
