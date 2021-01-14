# Set directory modified when necessary 
setwd("C:/Users/cerb915/Dropbox (Uni of Auckland)/CMND-Lab/CMND Lab Members/Current Projects/FET/Current Versions/Quintile Analyses/")

# Load data
FET.df <- read.csv("FET - Manual and Gaze - Data - Trial Level - ACC and PT.ACC - 2021.1.11.csv", header = T)
#FET.df$Sequence <- seq(1, dim(FET.df)[1])

# Set all NA values to -99 in CueFixation_mean
FET.df$CueFixation_mean[is.na(FET.df$CueFixation_mean)] <- -99

# Specify bin size, modify if necessary
bin_n = 5

# Identify each participant
subject_n <- unique (FET.df$SubNum)

# Define empty dataframes and vectors for the loop
FET_new.df <- data.frame()
table_n <- data.frame (Subject = 0, trial_n = 0, bin_size=0, remainder = 0)
rbin_subject <- c(); bin_subject <- c(); no_value_subject <- c()

# Assign appropriate bins according to CueFixation_mean for each participant 
for  (n in subject_n) {
  print (paste("Processing subject: ", n))
  # Obtain subseted data set for each subject
  df <- FET.df[FET.df[1] == n, ] 
  df <- df [order(df$CueFixation_mean),]
  df$bins[df$CueFixation_mean < 0] = 0
  start_trialn <- 1
  # start_trialn <- end(which (df$CueFixation_mean < 0))[1] + 1
  
  # Identify the number of trials where there is values in CueFixation_mean, trial number in each bin and remainder
  trial_n <- sum (df$CueFixation_mean >= 0)
  bin_size <- trial_n %/% bin_n
  remainder <- trial_n %% bin_n 
  # table that includes total number of trials, number of trials in each bin and number of remainder for each subject
  table_n <- rbind.data.frame(table_n, c(n, trial_n, bin_size, remainder))
  
  # Functions to assign trials to bins, number of trials in each bin is depended on the remainder value. 
  if (remainder == 0 & trial_n > 0) {
    for (bin in 1:bin_n){
      end_trialn <- start_trialn + bin_size
      df$bins[start_trialn:(end_trialn - 1)] <- bin
      start_trialn <- end_trialn 
    }
    bin_subject <- c(bin_subject, n)
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
    rbin_subject <- c(rbin_subject, n)
  } else {
    df$bins <- 0
    no_value_subject <- c(no_value_subject, n)
  }
  FET_new.df <- rbind.data.frame (FET_new.df, df)
}

# The following codes display subjects with remainders, without remainders or have no value in CueFixation_mean
print ("Following subjects have no remainders: "); print (bin_subject)
print ("")
print ("Following subjects have remainders: "); print (rbin_subject)
print ("")
print ("Following subjects have no value in CueFixation_mean: "); print (no_value_subject)

# Use to display the number of trials in each bin for each subject
for (n in subject_n){
  check <- FET_new.df [FET_new.df$SubNum == n, ]
  print (table (check$bins))
  print (paste("Summary table of subject: ", n))
}

#FET_new.df <- FET_new.df [order(FET_new.df$Sequence),]

# Save files
write.csv (table_n[2:dim(table_n)[1], ], "FET_TrialLevelDataACC.PTACC_bins.csv", row.names = F)
write.csv(FET_new.df, "FET_TrialLevelDataACC.PTACC_bins.csv", row.names = F)
