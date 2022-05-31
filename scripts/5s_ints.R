# created axl 18/03/22
# last edited axl 29/05/22

# libraries required
library(tidyverse)
library(data.table)
library(here)

# find the files

files <- list.files(path = here("data_tidy", "5s_ints"), recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

# Read all the files and create a FileName column to store filenames
scrdat <- rbindlist(sapply(files, fread, simplify = FALSE),
                use.names = TRUE, idcol = "FileName")
colnames(scrdat) <- c("id","time", "byte", "label", "mean") # rename columns

scrdat <- scrdat %>%
  dplyr::filter(time != "" & time != "Cmt Time") # throw out the nonsense rows

scrdat$id <- scrdat$id %>%
  substring(75,) %>% 
  str_remove('.txt') # then remove the part that says ".txt" leaving us with the number only


scrdat$id <- as.numeric(scrdat$id)
scrdat$mean <- as.numeric(scrdat$mean)
scrdat <- scrdat %>%
  arrange(id, time)

unique(scrdat$id)

# Data processing ####
## EXCLUSIONS ####
scrdat <- scrdat %>%
  filter(id != 32 & id != 46) %>% # issues with shock; fell asleep; respectively
  filter(id != 11 & id != 18 & id != 25 & id != 27 & id != 38 & id != 43) # had no trials of at least 1/4 trial categories
# we can't use data from before the 1st trial starts, so discard those
scrdat <- scrdat %>% filter(time != "0:00:00.0")

scrdat <- scrdat %>% 
  group_by(id) %>%
  mutate(trial = rep(1:40, each = 7)) %>% # add trial numbers. here each trial number is repeated 7 times, since there are 7 data points from each trial
  ungroup()

scrdat <- scrdat %>%
  group_by(id, trial) %>%
  mutate(datapt = rep(1:7)) %>% # 7 data points from each trial: each of 4 5s intervals in the 20s delay, 5s postoutcome interval, 2 intervals comprising baseline for the NEXT trial 
  ungroup()

# we can't use data from trial 1 since it has not been baselined, so discard these
# scrdat <- scrdat %>% filter(!(trial == 1 & label != "proceed")) # discard everything from trial 1 except the 2 baseline data points which we reserve for baselining the next trial

scrdat <- scrdat %>%
  group_by(id) %>%
  mutate(subtrial = lead(lead(datapt)), # we now change the file structure such that baseline info from the prev trial falls under the curret trial
         trial = lead(lead(trial))) %>% # so that 7 data pts from each trial is now in this order: 2 intervals comprising baseline, each of 4 5s intervals in 20s delay, 5s postoutcome interval
  dplyr::select(-c(datapt)) %>%
  dplyr::filter(!is.na(trial)) %>% # note this results in the last 2 "proceed" datapts (from the very last trial) to be NA - we don't need these as there is no next trial to be baselined
  ungroup()


# give trial category labels based on choice + what happens
scrdat <- scrdat %>%
  group_by(id, trial) %>%
  mutate(cat =  # add category where KIS trials are in a single category
           ifelse("FON" %in% label, 
                  ifelse("nothing" %in% label, "FONnothing",
                         "FONshock"),
                  ifelse("KIS" %in% label, "KIS", NA)),
         
         cat2 = #add category where KIS also separated into shock and nothing
           ifelse("FON" %in% label, 
                  ifelse("nothing" %in% label, "FONnothing",
                         "FONshock"),
                  ifelse("KIS" %in% label, 
                         ifelse("nothing" %in% label, "KISnothing",
                                "KISshock"),
                         NA)),
  ) %>%
  ungroup()

# give better trial labels

scrdat <- scrdat %>%
  mutate(label2 = case_when(
    subtrial == 1 | subtrial == 2 ~ "baseline",
    subtrial == 3 ~ "int1", #5s
    subtrial == 4 ~ "int2", #10s - to be recalculated 
    subtrial == 5 ~ "int3", #15s -
    subtrial == 6 ~ "int4", #20s
    subtrial == 7 ~ "outcome"
  ))

# have an outcome column
outcome <- scrdat %>%
  dplyr::filter(label2=="outcome") %>%
  rename(outcome = label) %>%
  dplyr::select(c(id, trial, outcome))

scrdat<- left_join(scrdat, outcome)

## get means for 2nd, 3rd, and 4th 5s interval (+ 5s for outcome) following choice ####
  
postchoice <- scrdat %>%
  group_by(id, trial) %>%
  filter(label2 != "baseline") %>% # only the rows which have interval related means
  ungroup()

# do some algebra so that we convert each value to means over a 5s interval
postchoice <- postchoice %>%
  mutate(mean2 = lag(mean)) %>%
  mutate(meanInt = case_when(
    label2 == "int1" ~ mean,
    label2 == "int2" ~ 2*mean - mean2, # to obtain mean for 2nd 5s interval, 2*(10s average) - (avg for 1st 5s interval)
    label2 == "int3" ~ 3*mean - 2*mean2,
    label2 == "int4" ~ 4*mean - 3*mean2,
    label2 == "outcome" ~ mean
  ))

postchoice <- postchoice %>%
  dplyr::select(-c(mean, mean2)) %>% 
  rename(mean = meanInt) # rename column to match with rest of data; this df now contains raw SCL for each 5s interval during the 20s delay


## get means for baseline ####
baseline <- scrdat %>%
  group_by(id, trial) %>%
  filter(label2 == "baseline") %>% # first 2 subtrials for each trial gives the baseline for that trial
  ungroup() # note that trial 1 does not have baseline

baseline <- baseline %>%
  group_by(id,trial) %>%
  mutate(mean2 = lag(mean)) %>%
  mutate(baseMean = (4/5)*mean2 + (1/5)*mean) %>%
  ungroup() %>%
  dplyr::filter(!is.na(baseMean))

baseline <- baseline %>%
  dplyr::select(c(id, trial, baseMean, outcome, cat2)) # df with baseline per trial per participant

# add baseline data as another column
tmp <- left_join(postchoice, baseline) # join by id and trial

# now subtract baseMean from each mean for baselined data on each trial
baselined <- tmp %>%
  mutate(mean_baselined = mean - baseMean)

cleaned <- baselined %>% 
  dplyr::select(id, trial, cat, cat2, outcome, subtrial, label2, mean, baseMean, mean_baselined) %>%
  rename(label = label2)

cleaned <- cleaned %>% mutate(
  mean_log = log(
    cleaned$mean_baselined+(-(min(cleaned$mean_baselined))+0.001) #so that min(Y+a) = 0.001
  )
) # NAs !!!!!

max(cleaned$mean_baselined)

# plots ####

## grouped by category -- not log transformed ####

catSummary <- cleaned %>% # chosen method
  group_by(id, cat, subtrial) %>%
  summarise(mean = mean(mean_baselined, na.rm=TRUE)) %>% # 
  ungroup() %>%
  group_by(cat, subtrial) %>%
  summarise(mean_sd = sd(mean, na.rm=TRUE),
            mean = mean(mean, na.rm=TRUE)) %>%
  ungroup()
  

indSummary <- cleaned %>%
  group_by(id, cat, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined, na.rm=TRUE),
            mean = mean(mean_baselined, na.rm=TRUE)) %>%
  ungroup()

int_plot <- ggplot(catSummary,
                   aes(as.factor(subtrial), mean, group = cat, col = cat)) +
  geom_point(size = 3) +
  geom_line() +# +
  #geom_errorbar(aes(ymin=mean_log - mean_log_sd, ymax = mean_log + mean_log_sd), width = .1) 
  ylab("change in SCL") +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) 

int_plot

# ggsave("plots/intPlot_cat.pdf", width = 6.5, height = 4.8)

# grouped by category, but with individual spaghetti lines
ggplot(indSummary, aes(x=as.factor(subtrial), y=mean, group=interaction(cat, id), col=cat)) +
  geom_line(aes(alpha = 1)) +
  geom_line(data = catSummary, aes(alpha = 1, group=cat, col = cat), size = 1.4)+
  geom_point(data = catSummary, aes(alpha = 1, group=cat, col = cat), size = 3) +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) +
  ylab("log change in SCL") 

#ggsave("plots/intPlot_ind.pdf", width = 7.5, height = 6)

## grouped by category2 (ie choice + outcome) -- not log transformed ####

catSummary2 <- cleaned %>% # case 1: group by conditions
  group_by(cat2, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined, na.rm=TRUE),
            mean = mean(mean_baselined, na.rm=TRUE)) %>%
  ungroup()

catSummary2 <- cleaned %>%
  group_by(id, cat2, subtrial) %>%  #case 2: group by participants then conditions
  summarise(mean = mean(mean_baselined, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(cat2, subtrial) %>%
  summarise(mean_sd = sd(mean, na.rm=TRUE),
            mean = mean(mean, na.rm=TRUE)) %>%
  ungroup() 
  

indSummary2 <- cleaned %>%
  group_by(id, cat2, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined, na.rm=TRUE),
            mean = mean(mean_baselined, na.rm=TRUE)) %>%
  ungroup()

int_plot2 <- ggplot(catSummary2,
                   aes(as.factor(subtrial), mean, group = cat2, col = cat2)) +
  geom_point(size = 3) +
  geom_line() +# +
  #geom_errorbar(aes(ymin=mean_log - mean_log_sd, ymax = mean_log + mean_log_sd), width = .1) 
  ylab("change in SCL") +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) 

int_plot2

# ggsave("plots/intervals_cat2.pdf", width = 6.5, height = 4.8)

# grouped by category, but with individual spaghetti lines (overlaid)
ggplot(indSummary2, aes(x=as.factor(subtrial), y=mean, group=interaction(cat2, id), col=cat2)) +
  geom_line(aes(alpha = 1)) +
  geom_line(data = catSummary2, aes(alpha = 1, group=cat2, col = cat2), size = 1.4)+
  geom_point(data = catSummary2, aes(alpha = 1, group=cat2, col = cat2), size = 3) +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) +
  ylab("change in SCL") 
# split individual lines into facets
ggplot(indSummary2, aes(x=as.factor(subtrial), y=mean, group=interaction(cat2, id), col=cat2)) +
  geom_line() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) +
  ylab("change in SCL") +
  facet_wrap(~id, ncol=6) +
  theme_bw() 

ggsave("plots/intervals_cat2_ind.jpg", width = 15, height = 20)

# why is there a difference beteween KISshock and KISnothing? ####

#### a) check: baseline following SHOCK vs NOTHING ####

tmp <- scrdat %>%
  dplyr::select(c(id, mean, trial, subtrial, outcome, cat2)) %>%
  group_by(id, trial) %>%
  slice(-c(1:2)) %>%
  ungroup()

tmp2 <- baseline %>%
  rename(mean = baseMean) %>%
  mutate(subtrial = 0)

tmp <- rbind(tmp2, tmp)

tmp <- tmp %>% arrange(id, trial, subtrial)

baseAfterOut <- tmp %>% 
  group_by(id) %>%
  mutate(prevOutcome = lag(outcome)) %>%
  dplyr::filter(subtrial == 0 & !is.na(prevOutcome)) %>%
  ungroup() %>%
  dplyr::select(-c(outcome)) 

tmp <- baseAfterOut %>%
  group_by(id, prevOutcome) %>%
  summarise(mean = mean(mean))

ggplot(tmp, aes(x=as.factor(prevOutcome), y=mean, group=prevOutcome, col=prevOutcome)) +
  geom_boxplot() +
  theme_bw() +
  ylab("mean of current baseline (microsiemens)") +
  xlab("outcome on previous trial")

#ggsave("plots/baseline_prevOutcome.pdf", width = 6.5, height = 4.8)
# plot shows baseline elevated in trial following shock compared to nothing outcome

#### b) check: baseline following FONnothing vs FONshock vs KISnothing vs KISshock ####

tmp <- scrdat %>%
  dplyr::select(c(id, mean, trial, subtrial, outcome, cat2)) %>%
  group_by(id, trial) %>%
  slice(-c(1:2)) %>%
  ungroup()

tmp2 <- baseline %>%
  rename(mean = baseMean) %>%
  mutate(subtrial = 0)

tmp <- rbind(tmp2, tmp)

tmp <- tmp %>% arrange(id, trial, subtrial)

baseAfterCat2 <- tmp %>% 
  group_by(id) %>%
  mutate(prevCat2 = lag(cat2)) %>%
  dplyr::filter(subtrial == 0 & !is.na(prevCat2)) %>%
  ungroup() %>%
  dplyr::select(-c(cat2)) 

tmp <- baseAfterCat2 %>%
  group_by(id, prevCat2) %>%
  summarise(mean = mean(mean))

ggplot(tmp, aes(x=as.factor(prevCat2), y=mean, group=prevCat2, col=prevCat2)) +
  geom_boxplot() +
  theme_bw() +
  ylab("baseline (microsiemens)") +
  xlab("previous trial type") 

#ggsave("plots/basline_prevCat.pdf", width = 6.5, height = 4.8) #note this is plot of subject means for each condition
# plot shows that on aaverage, baseline highest after KISshock trial, followed by FONshock

#### c) check: baseline for FONnothing vs FONshock vs KISnothing vs KISshock trials ####

tmp <- baseline %>%
  group_by(id, cat2) %>%
  summarise(mean = mean(baseMean)) %>%
  ungroup()

means <- aggregate(mean ~  cat2, tmp, mean)

ggplot(tmp, aes(x=as.factor(cat2), y=mean, group=cat2, col=cat2)) +
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_text(data = means, aes(label = mean, y = mean+1.5))+ # y argument moves text up
  geom_jitter()+
  theme_bw() +
  ylab("baseline (microsiemens)") +
  xlab("current trial type") 

#ggsave("plots/baseline_currCat.pdf", width = 7.8, height = 4.8) #note this is plot of subject means for each condition
# baseline is highest for KISshock trials, followed by KISnothing, followed by FONshock then FONnothing

## here we see that KISshock trials have the highest mean baseline - this possibly will lower the
## raw SCL measure during intervals 1-5

#### d) % shock vs nothing on PREVIOUS trial for each trial cat ####

# we check this to see if KISshock trials have higher % previous shock trials vs nothing

data_prevOutcome <- cleaned %>%
  group_by(id) %>%
  mutate(prevOutcome = lag(outcome),
         prevCat2 = lag(cat2)) %>%
  filter(subtrial == 3) %>%
  select(c(id, trial, cat, cat2, outcome, prevOutcome, prevCat2))


### just as fyi see # of trials of each trial cat each subject experienced ####
mean_trialcat <- data_prevOutcome %>%
  group_by(id, cat2) %>%
  summarise(count = n()) %>%
  ungroup()

means <- aggregate(count ~  cat2, mean_trialcat, mean) # find means 

# cat2     count
# 1 FONnothing 12.902439
# 2   FONshock 13.785714
# 3 KISnothing  7.763158
# 4   KISshock  7.486486

## on average people choose FON on  ~26/40 trials; and KIS on ~15/40 trials

ggplot(mean_trialcat, aes(x=as.factor(cat2), y=count, group=cat2, col=cat2)) +
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_text(data = means, aes(label = count, y = count+1.5))+ # y argument moves text up
  geom_jitter()+
  theme_bw() +
  ylab("mean # trials") +
  xlab("trial type")

# now remove trial 1 because there is no prev trial data
tmp <- data_prevOutcome %>% filter(!is.na(prevOutcome))

# find prop shock vs nothing for each trial cat
# ... first count the number of trials belonging to each trial cat/prevOutcome
# combination each subject had experienced

tmp <- tmp %>%
  group_by(id, cat2, prevOutcome) %>%
  summarise(count = n()) %>%
  arrange(id, cat2, prevOutcome) %>%
  ungroup() # this gives no. of trials for each cat2/prevoutcome combination per subject

# add column with counts of number of trials of certain category, for calculating proportions
tmp <- tmp %>%
  group_by(id, cat2) %>%
  mutate(total = sum(count)) %>% # total gives the total no. of trials for sub in each cat2 category 
  ungroup()

tmp <- tmp %>%
  mutate(prop = count/total) # gives prop of shock / total # trials for cat2
                             # if KISshock has high baselines bc of mostly preceded by shock trials, we should be able to see this
# in the above, props for each cat2 adds up to 1

ggplot(tmp, aes(x=cat2, y=prop, group=interaction(cat2, prevOutcome), col=interaction(cat2, prevOutcome))) +
  geom_boxplot() +
  # stat_summary(fun=mean, position=position_nudge(x = 0.1, y = 0), colour="darkred", geom="point", 
  #              shape=18, size=3, show.legend=FALSE) + 
  geom_point(position = position_jitterdodge()) +
  theme_bw() +
  ylab("mean # trials") +
  xlab("trial type")

tmp %>%
  group_by(cat2, prevOutcome) %>%
  summarise(meanProp = mean(prop)) %>%
  ungroup()
# essentially, the takeaway here is that trials of the type KISshock isn't more likely to be preceded by shock trials
# in fact shows ~57.5% of KISshock trials preceded by nothing, ~51.1% preceded by shock

#### e) % shock vs nothing on CURRENT trial for FON vs KIS ####

# in hindsight I'm not sure how much sense this question makes

#### f) mean number of successive shocks preceding.... ####

# further data exploration ####

## behavioral KIS/FON preferences across blocks ####

choiceData <- cleaned %>%
  select(id, trial, cat2, outcome) %>%
  unique() %>%
  mutate(choice = substr(cat2, 1, 3)) %>% # keep FON or KIS only
  select(-c(cat2)) %>%
  arrange(id, trial) %>%
  group_by(id) %>%
  mutate(block = rep(1:4, each = 10)) %>% # add block numbers
  ungroup()

tmp <- choiceData %>%
  group_by(id, block, choice, .drop=FALSE) %>% # not working
  tally() %>%
  ungroup()

tmp <- tmp %>%
  filter(choice=="FON") %>%
  select(-c(choice)) %>%
  mutate(propFON = n/10)

ggplot(tmp, aes(x=block, y=propFON, group=as.factor(id), col=as.factor(id))) +
  geom_line() +
  facet_wrap(~id, ncol=3)

## does SCR diff between KIS and FON predict prop of KIS/FON choices?? ####

# for each subject, we need mean baselined scr across 20s interval for KIS - mean baselined SCR across 20s interval for FON 
tmp <- scrdat %>%
  dplyr::filter(label2 == "int4" | label2 == "outcome") #20s averaged period only

tmp <- tmp %>% 
  mutate(label2 = case_when(
    label2 == "outcome" ~ "outcome",
    label2 == "int4" ~ "20s"
  ))

tmp <- left_join(tmp, baseline)

# now subtract baseMean from each mean for baselined data on each trial
baselined_20s <- tmp %>%
  mutate(mean_baselined = mean - baseMean)

cleaned_20s <- baselined_20s %>% 
  dplyr::select(id, trial, cat, outcome, subtrial, label2, mean, baseMean, mean_baselined) %>%
  mutate(choice = substr(cat, 1, 3)) %>% # keep FON or KIS only
  rename(label = label2)

cleaned_20s <- cleaned_20s %>% dplyr::filter(label == "20s") %>% filter(!is.na(mean_baselined))

choice_scr <- cleaned_20s %>%
  group_by(id, choice) %>%
  summarise(sd = sd(mean_baselined),
            m = mean(mean_baselined),
            n = n()) %>%
  ungroup()

choice_scr_diff <- choice_scr %>%
  group_by(id) %>%
  mutate(lagm = lag(m)) %>%
  ungroup() %>%
  mutate(KIS_FON = m - lagm) %>% # difference between KIS mean baselined and FON
  filter(!is.na(KIS_FON)) %>%
  rename(nKIS = n) %>%
  select(-c(choice, sd))

# now get prop of FON choices and add as a column 
choice_scr_diff <- choice_scr_diff %>%
  mutate(propFON = (39-nKIS)/39) #out of 39 as only 39 trials had baselined means

# does KIS-FON predict prop FON?
mod1 <- lm(propFON ~ 1, choice_scr_diff)
mod2 <- lm(propFON ~ KIS_FON, choice_scr_diff)
summary(mod2) #ns
anova(mod1, mod2)

## does trial category on previous trial predict choosing FON on current trial? ####
tmp <- data_prevOutcome %>%
  mutate(choice = substr(cat, 1, 3)) %>%
  filter(!is.na(prevOutcome))

tmp$choice <- as.factor(tmp$choice)
tmp$prevOutcome <- as.factor(tmp$prevOutcome)
tmp$prevChoice <- substring(tmp$prevCat2, 1, 3)

mod1<-glm(choice ~ 1, family = binomial, data = tmp)
mod2<-glm(choice ~ prevOutcome, family = binomial, data = tmp)
mod3<-glm(choice ~ prevOutcome + prevChoice, family = binomial, data = tmp)

anova(mod1, mod2, mod3, test="Chisq") # ns for prevoutcome, sig for prevCat2

## predictor of baselined SCR on current trial: FON/KIS choice vs outcome vs prevOutcome ####

# add prevOutcome column to cleaned_20s which has the 20s mean scrs

tmp <- data_prevOutcome %>% filter(!is.na(prevOutcome))
cleaned_20s

cleaned_20s <- right_join(cleaned_20s, tmp)
# change to factors
cleaned_20s$prevOutcome <- as.factor(cleaned_20s$prevOutcome)
cleaned_20s$outcome <- as.factor(cleaned_20s$outcome)
cleaned_20s$choice <- as.factor(cleaned_20s$choice)

cleaned_20s$prevChoice <- substring(cleaned_20s$prevCat2, 1,3 )
# make models with scr as dv

library(lme4)
mod1 <- lmer(mean_baselined ~ 1 + (1|id), data = cleaned_20s)
mod2 <- lmer(mean_baselined ~ choice + (1|id), data = cleaned_20s)
mod3 <- lmer(mean_baselined ~ choice + prevOutcome + (1|id), data = cleaned_20s)
mod4 <- lmer(mean_baselined ~ choice + prevOutcome + prevChoice + (1|id), data = cleaned_20s)

anova(mod1, mod2, mod3, mod4)

## see if outcome is good predictor

### for FON trials
mod1 <- lmer(mean_baselined ~ 1 + (1|id), data = subset(cleaned_20s, choice == 'FON'))
mod2 <- lmer(mean_baselined ~ outcome + (1|id), data = subset(cleaned_20s, choice == 'FON'))
anova(mod1, mod2)

### for KIS trials
mod1 <- lmer(mean_baselined ~ 1 + (1|id), data = subset(cleaned_20s, choice == 'KIS'))
mod2 <- lmer(mean_baselined ~ outcome + (1|id), data = subset(cleaned_20s, choice == 'KIS'))
anova(mod1, mod2)

# re-baselined outcome t-test ####

tmp <- cleaned %>%
  select(c(id, trial, cat, cat2, outcome, label, mean)) %>%
  group_by(id, trial) %>%
  mutate(mean_lagged = lag(mean)) %>%
  ungroup() %>%
  filter(label == 'outcome')%>%
  rename(mean_int4 = mean_lagged)

tmp <- tmp %>% mutate(
  outcome_rebased = mean - mean_int4
)

#### For each KIS/FON, compare nothing vs shock

outcome_KIS <- subset(tmp, cat2 == 'KISshock' | cat2 == 'KISnothing')
outcome_FON <- subset(tmp, cat2 == 'FONshock' | cat2 == 'FONnothing')

outcome_KIS <- outcome_KIS %>%
  group_by(id, cat2) %>%
  summarise(outcome_rebased = mean(outcome_rebased)) %>%
  ungroup()


outcome_FON <- outcome_FON %>%
  group_by(id, cat2) %>%
  summarise(outcome_rebased = mean(outcome_rebased)) %>%
  ungroup()

library(rstatix)

outcome_KIS %>% wilcox_effsize(outcome_rebased ~ cat2, mu = 0) # moderate
outcome_FON %>% wilcox_effsize(outcome_rebased ~ cat2, mu = 0) # large

outcome_KIS <- pivot_wider(outcome_KIS, names_from = cat2, values_from = outcome_rebased)
outcome_FON <- pivot_wider(outcome_FON, names_from = cat2, values_from = outcome_rebased)

## use wilcoxin since we can't get data to be normal even after log transform

wilcox.test(outcome_KIS$KISnothing, outcome_KIS$KISshock, paired=TRUE) 
wilcox.test(outcome_FON$FONnothing, outcome_FON$FONshock, paired=TRUE) 

# replicating helen's analysis with baselined data ####
catSummary_20s <- cleaned_20s %>%
  group_by(cat, subtrial) %>%
  summarise(sd = sd(mean_baselined),
            m = mean(mean_baselined),
            n = n()) %>%
  ungroup()

ggplot(catSummary_20s, aes(cat, m)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=m-sd/sqrt(n), ymax=m+sd/sqrt(n)), width=.05) + 
  labs(x="Choice", y="Mean SCR", title="SCR by Choice (averaged across trials)")

#ggsave("plots/helen_baselined.pdf", width = 7.5, height = 6)

