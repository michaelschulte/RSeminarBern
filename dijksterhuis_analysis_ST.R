#A script to analyze an idividual lab's replication results.
#This script binds all individual subject data into a single
#file and saves it out in raw form, and also saves out a results
#file that includes effects, confidence intervals, and summary statistics.

#Vers. 1.01
#Created by Katherine Wood 
#Modified by Stefan Thoma for the Replication Seminar at the UniBe

#November 2018

labname = 'LABNAME'

#Install packages, if needed, and load them in.
if(!require('data.table',quietly=TRUE)) {
    install.packages('data.table', quiet=TRUE)
  }
library('data.table')
library("BayesFactor")
library("ggplot2")
#First read in all data files and bind into a single data frame.
setwd(paste(getwd(), "RawData", sep = "/"))

all_subjects <- as.data.frame(rbindlist(lapply(list.files(
                        pattern = '[[:digit:]]_[[:alpha:]]*.*.csv'), read.csv)))

#change values of sex_r and age
placeholder <- all_subjects$sex_r
all_subjects$sex_r <- all_subjects$age
all_subjects$age <- placeholder
placeholder <- NULL

#now fix values for hupf_items
hupf_corr_cols <- paste(rep('triv', 3), c(8, 13, 23), rep('_correct', 3), sep='')
hupf_raw_cols <- paste(rep('triv', 3), c(8, 13, 23), rep('_raw', 3), sep='')
hupf_answers <- c(4, 20, 29)

#shitty loop bc I need True as string and too tired to write it nicely... 
for(i in 1:length(hupf_corr_cols)){
  for(j in 1:length(dat$Subject.ID)){
  if(dat[j,hupf_raw_cols[i]] ==hupf_answers[i]){
    dat[j,hupf_corr_cols[i]] <- "True"
  } else {dat[j,hupf_corr_cols[i]] <- "False"}
  
    #if i would need true as boolean then this following line would be sufficient and no double loop were needed...
  #dat[,hupf_corr_cols[i]] <- dat[,hupf_raw_cols[i]] ==hupf_answers[i]
  }
}

#dat[,hupf_corr_cols]

#Process the exclusions we can detect automatically from the data
excluded_subjs <- all_subjects$subjID[all_subjects$age < 18 | all_subjects$age > 24 |
                                      as.character(all_subjects$year) == 'not a student' |
                                      is.na(all_subjects$prime_response)]

#Determine number of trivia questions correct, number of trivia questions skipped,
#percent correct, and percent correct calculated relative only to questions answered.
#set chose only not-excluded subjects.
if(is.null(excluded_subjs)){
  dat <- all_subjects
} else{
dat <- all_subjects[!(all_subjects$subjID %in% excluded_subjs),]
} 
corr_cols <- paste(rep('triv', 30), seq(1:30), rep('_correct', 30), sep='')
raw_cols <- paste(rep('triv', 30), seq(1:30), rep('_raw', 30), sep='')
dat$correct_raw <- rowSums(dat[,corr_cols] == 'True')

dat$correct_pct <- (dat$correct_raw/30) * 100

dat$hupf_corr_pct <- (rowSums(dat[,hupf_corr_cols] == "True")/3)*100


#Save out this data frame as a .csv
#now first set the wd back to outside the folder
setwd(qdap::beg2char(getwd(), char = "/", noc = 6, include = FALSE))

write.csv(dat, paste(labname, '_data_complete.csv', sep=''), row.names = FALSE)






####ANALYSIS
t.test(dat$correct_pct[dat$prime_condition=="Universitätsprofessor"], dat$correct_pct[dat$prime_condition=="Fußballhooligan"])
library(ggplot2)
ggplot2::ggplot(data = dat, ggplot2::aes(x = prime_condition)) +
  geom_bar(stat = "count")
ggplot2::ggplot(data = dat, ggplot2::aes(x = prime_condition, y = correct_pct)) +
  geom_boxplot()


#well alright, let us look at the questions suggested J.H.H.
#these were qNR: 8, 13, 23



hupf_boxplot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = prime_condition, y = hupf_corr_pct)) +
  geom_boxplot()
hupf_ttest <- t.test(dat$hupf_corr_pct[dat$prime_condition=="Universitätsprofessor"], dat$hupf_corr_pct[dat$prime_condition=="Fußballhooligan"])



?ggplot()


