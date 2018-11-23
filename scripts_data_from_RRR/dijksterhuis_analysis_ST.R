#This script binds all individual subject data into a single
#file and saves it out in raw form.

#Vers. 1.02

#November 2018

labname = 'LABNAME'

#Install packages, if needed, and load them in.
if(!require('data.table',quietly=TRUE)) {
    install.packages('data.table', quiet=TRUE)
  }
library('data.table')
library("BayesFactor")
library("ggplot2")
library("stringr")
library("readxl")

#First read in all data files and bind into a single data frame.
setwd(paste(getwd(), "RawData", sep = "/"))

all_subjects <- as.data.frame(rbindlist(lapply(list.files(
                        pattern = '[[:digit:]]_[[:alpha:]]*.*.csv'), read.csv)))

#import backup data and merge it together
backup <- readxl::read_xls("backup_data.xls")
all_subjects <- dplyr::bind_rows(all_subjects, backup)
#change values of sex_r and age
placeholder <- all_subjects$sex_r
all_subjects$sex_r <- all_subjects$age
all_subjects$age <- placeholder
placeholder <- NULL



#get all column names and hupf column names and all correct answers
hupf_corr_cols <- paste(rep('triv', 3), c(8, 13, 23), rep('_correct', 3), sep='')
hupf_raw_cols <- paste(rep('triv', 3), c(8, 13, 23), rep('_raw', 3), sep='')
corr_cols <- paste(rep('triv', 30), seq(1:30), rep('_correct', 30), sep='')
raw_cols <- paste(rep('triv', 30), seq(1:30), rep('_raw', 30), sep='')


all_answers <- c("Botswana", "ASCII", "Wasserschwein","Erde","12", "Indien","Arm", "4", 
                  "Bronze","Welle","Isobare",  "21",  "20",  "180",  "Venus","Michail Gorbatschow", 
                  "Strahlung", "Deep Blue", "Kontinentalsockel","Faschismus","Penicillin", "Pablo Picasso",
                  "29", "Im Rachen", "Sibirien", "Jupiter", "Tschaikowski",
                  "Luftdruck", "Keratin", "Frankreich")
#exclude subjects based on their answer in "purpose" variable.
#print(all_subjects$purpose) #39, 40 and 41 have to be excluded. 27 maybe.
#(potentially) problematic answers
ppa <- c("Eignungstest als Professor", "Erfahren, inwiefern die innere Haltung als \"Professor\" die Performance im Wissenstest beeinflusst.",
         "wenn man sich vorher als typischer Professor vorstellt, ist man besser im Wissenstest\n", "Priming von Stereotypen", 
         "Ich glaube es ging um die Verbindung vom ersten Teil (Professor) mit dem zweiten Teil")
dat <- all_subjects %>% dplyr::filter(!(purpose %in% ppa))
#print(dat$prime_response)

#cool loop to verify all answers. 
for(i in 1:length(corr_cols)){

    dat[,corr_cols[i]] <- as.character(dat[,raw_cols[i]]) ==all_answers[i]
  
}

dat$correct_raw <- rowSums(dat[,corr_cols] == TRUE)

dat$correct_pct <- (dat$correct_raw/30) * 100

dat$hupf_corr_pct <- (rowSums(dat[,hupf_corr_cols] == TRUE)/3)*100


#Save out this data frame as a .csv
#now first set the wd back to outside the folder
#setwd(qdap::beg2char(getwd(), char = "GitHub/RSeminarBern", include = TRUE))

write.csv(dat, paste(labname, '_data_complete_withBackup.csv', sep=''), row.names = FALSE)



