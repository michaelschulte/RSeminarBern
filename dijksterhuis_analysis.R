#A script to analyze an idividual lab's replication results.
#This script binds all individual subject data into a single
#file and saves it out in raw form, and also saves out a results
#file that includes effects, confidence intervals, and summary statistics.

#Vers. 1.0
#Created by Katherine Wood 
#June 2016

labname = 'LABNAME'

setwd(getSrcDirectory(function(x) {x}))
#Install packages, if needed, and load them in.
if(!require('data.table',quietly=TRUE)) {
    install.packages('data.table', quiet=TRUE)
  }
library('data.table')

#First read in all data files and bind into a single data frame.
all_subjects <- as.data.frame(rbindlist(lapply(list.files(path=getwd(), 
                        pattern = '[[:digit:]]_[[:alpha:]]*.*.csv'), read.csv)))

#Process the exclusions we can detect automatically from the data
excluded_subjs <- all_subjects$subjID[all_subjects$age < 18 | all_subjects$age > 24 |
                                      as.character(all_subjects$year) == 'not a student' |
                                      is.na(all_subjects$prime_response)]

#Determine number of trivia questions correct, number of trivia questions skipped,
#percent correct, and percent correct calculated relative only to questions answered.
dat <- all_subjects[!(all_subjects$subjID %in% excluded_subjs),]
corr_cols <- paste(rep('triv', 30), seq(1:30), rep('_correct', 30), sep='')
raw_cols <- paste(rep('triv', 30), seq(1:30), rep('_raw', 30), sep='')
dat$correct_raw <- rowSums(dat[,corr_cols] == 'True')
dat$skipped_raw <- rowSums(dat[,raw_cols] == 0)
dat$correct_pct <- (dat$correct_raw/30) * 100
dat$correct_skip_pct <- (dat$correct_raw/rowSums(dat[,raw_cols] != 0)) * 100

skip_prof <- rowSums(dat[dat$prime_condition == 'professor',raw_cols] == 0)
skip_hool <- rowSums(dat[dat$prime_condition == 'hooligan',raw_cols] == 0)

#Save out this data frame as a .csv
write.csv(dat, paste(labname, '_data_complete.csv', sep=''), row.names = FALSE)

aov3 <- function(model){ # model is an lm object
  tab <- drop1(model,~.,test='F')[-1,-3] # Type III SS including F-tests 
  tab[,3] <- tab[,2]/tab[,1] # replace AIC column with mean squares
  colnames(tab)[2:3] <- c('Sum Sq','Mean Sq') # rename SS and MS columns
  resid <- anova(model)[nrow(anova(model)),] # Df, SS, and MS for residuals (error)
  rbind(tab,resid) # add row for residuals
}

dat$prime_condition <- relevel(dat$prime_condition, 'professor')
prime.mod <- lm(correct_pct ~ prime_condition * sex_r, data=dat)
skip.mod <- lm(correct_skip_pct ~ prime_condition * sex_r, data=dat)
prime.aov <- aov3(prime.mod)
skip.aov <- aov3(skip.mod)

#Extract raw difference of means and SE of means
sediff <- function(x, y) sqrt((var(x)/NROW(x)) + (var(y)/NROW(y)))
prof_scores <- dat$correct_pct[dat$prime_condition == 'professor']
prof_skip <-dat$correct_skip_pct[dat$prime_condition == 'professor']
hool_scores <- dat$correct_pct[dat$prime_condition == 'hooligan']
hool_skip <- dat$correct_skip_pct[dat$prime_condition == 'hooligan']
main_diff <- mean(prof_scores) - mean(hool_scores)
main_se <- sediff(prof_scores, hool_scores)
skip_diff <- mean(prof_skip) - mean(hool_skip)
skip_se <- sediff(prof_skip, hool_skip)
f_prof_scores <- dat$correct_pct[dat$prime_condition=='professor' & dat$sex_r=='female']
f_prof_skip <- dat$correct_skip_pct[dat$prime_condition=='professor' & dat$sex_r=='female']
f_hool_scores <- dat$correct_pct[dat$prime_condition=='hooligan' & dat$sex_r=='female']
f_hool_skip <- dat$correct_skip_pct[dat$prime_condition=='hooligan' & dat$sex_r=='female']
f_diff <- mean(f_prof_scores) - mean(f_hool_scores)
f_skip_diff <- mean(f_prof_skip) - mean(f_hool_skip)
f_se <- sediff(f_prof_scores, f_hool_scores)
f_skip_se <- sediff(f_prof_skip, f_hool_skip)

m_prof_scores <- dat$correct_pct[dat$prime_condition=='professor' & dat$sex_r=='male']
m_prof_skip <- dat$correct_skip_pct[dat$prime_condition=='professor' & dat$sex_r=='male']
m_hool_scores <- dat$correct_pct[dat$prime_condition=='hooligan' & dat$sex_r=='male']
m_hool_skip <- dat$correct_skip_pct[dat$prime_condition=='hooligan' & dat$sex_r=='male']
m_diff <- mean(m_prof_scores) - mean(m_hool_scores)
m_skip_diff <- mean(m_prof_skip) - mean(m_hool_skip)
m_se <- sediff(m_prof_scores, m_hool_scores)
m_skip_se <- sediff(m_prof_skip, m_hool_skip)

diff_diff <- summary(prime.mod)$coefficients[4, 2]
diff_diff_skip <- summary(skip.mod)$coefficients[4, 2]
diff_se <- summary(prime.mod)$coefficients[4, 2]
diff_skip_se <- summary(skip.mod)$coefficients[4, 2]

stats <- data.frame(mean(prof_scores), sd(prof_scores), mean(hool_scores), sd(hool_scores),
                    mean(prof_skip), sd(prof_skip), mean(hool_skip), sd(hool_skip))
colnames(stats) <- c('prof_mean', 'prof_sd', 'hool_mean', 'hool_sd',
                     'prof_mean_skip', 'prof_sd_skip', 'hool_mean_skip', 'hool_sd_skip')

skip_stats <- data.frame(mean(skip_prof), sd(skip_prof), mean(skip_hool), sd(skip_hool))
colnames(skip_stats) <- c('prof_skip_mean', 'prof_skip_sd', 'hool_skip_mean', 'hool_skip_sd')

stats_gen <- data.frame(mean(f_prof_scores), sd(f_prof_scores), 
                        mean(f_hool_scores), sd(f_hool_scores),
                        mean(m_prof_scores), sd(m_prof_scores),
                        mean(m_hool_scores), sd(m_hool_scores),
                        mean(f_prof_skip), sd(f_prof_skip), 
                        mean(f_hool_skip), sd(f_hool_skip),
                        mean(m_prof_skip), sd(m_prof_skip),
                        mean(m_hool_skip), sd(m_hool_skip))
colnames(stats_gen) <- c('f_prof_mean', 'f_prof_sd', 'f_hool_mean', 'f_hool_sd',
                         'm_prof_mean', 'm_prof_sd', 'm_hool_mean', 'm_hool_sd',
                         'f_prof_mean_skip', 'f_prof_sd_skip', 
                         'f_hool_mean_skip', 'f_hool_sd_skip',
                         'm_prof_mean_skip', 'm_prof_sd_skip',
                         'm_hool_mean_skip', 'm_hool_sd_skip')

#Assemble and save out results file; this file is passed to the meta-analysis script
effects <- data.frame(main_diff, main_se, skip_diff, skip_se, sum(dat$skipped_raw)/nrow(dat), 
                      f_diff, f_se, f_skip_diff, f_skip_se,
                      m_diff, m_se, m_skip_diff, m_skip_se,
                      diff_diff, diff_se, diff_diff_skip, diff_skip_se)
colnames(effects) <- c('main_effect', 'main_se', 'skip_effect', 'skip_se', 'pct_skipped',
                       'f_effect', 'f_se', 'f_skip_effect', 'f_skip_se', 
                       'm_effect', 'm_se', 'm_skip_effect', 'm_skip_se',
                       'diff_effect', 'diff_se', 'diff_skip_effect', 'diff_skip_se')

write.csv(effects, paste(labname, '_data_meta.csv', sep=''), row.names=FALSE)
write.csv(prime.aov, paste(labname, '_results.csv', sep=''), row.names=FALSE)
write.csv(skip.aov, paste(labname, '_skip_results.csv', sep=''), row.names=FALSE)
write.csv(skip_stats, paste(labname, '_skip_stats.csv', sep=''), row.names=FALSE)
write.csv(stats, paste(labname, '_stats_main.csv', sep=''), row.names=FALSE)
write.csv(stats_gen, paste(labname, '_stats_gender.csv', sep=''), row.names=FALSE)