# created axl 18/03/22

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

scrdat$id <- substring(scrdat$id, 71) # turn the file name into subject numbers; first leave [NUMBER].txt
scrdat$id<- str_replace(scrdat$id, ".txt", "") # then remove the part that says ".txt" leaving us with the number only

scrdat$id <- as.numeric(scrdat$id)
scrdat$mean <- as.numeric(scrdat$mean)
scrdat <- scrdat %>%
  arrange(id, time)

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
scrdat <- scrdat %>% filter(!(trial == 1 & label != "proceed")) # discard everything from trial 1 except the 2 baseline data points which we reserve for baselining the next trial

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

# get means for 2nd, 3rd, and 4th 5s interval (+ 5s for outcome) following choice ####
  
postchoice <- scrdat %>%
  group_by(id, trial) %>%
  slice(-(c(1:2))) %>% # only the rows which have interval related means
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


# get means for baseline ####
baseline <- scrdat %>%
  group_by(id, trial) %>%
  slice(c(1:2)) %>% # first 2 subtrials for each trial gives the baseline for that trial
  ungroup()

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
)

max(cleaned$mean_baselined)

# check: baseline following SHOCK vs NOTHING ####

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
  ylab("mean SCL (microsiemens)")

# check: baseline following FONnothing vs FONshock vs KISnothing vs KISshock

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
  ylab("mean SCL (microsiemens)")

# plots ####

# grouped by category -- not log transformed ####

catSummary_case1 <- cleaned %>%
  group_by(cat, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined),
            mean = mean(mean_baselined)) %>%
  ungroup()

catSummary_case2 <- cleaned %>%
  group_by(id, cat, subtrial) %>%
  summarise(mean = mean(mean_baselined)) %>%
  ungroup() %>%
  group_by(cat, subtrial) %>%
  summarise(mean_sd = sd(mean),
            mean = mean(mean)) %>%
  ungroup()
  

indSummary <- cleaned %>%
  group_by(id, cat, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined),
            mean = mean(mean_baselined)) %>%
  ungroup()

int_plot <- ggplot(catSummary_case2,
                   aes(as.factor(subtrial), mean, group = cat, col = cat)) +
  geom_point(size = 3) +
  geom_line() +# +
  #geom_errorbar(aes(ymin=mean_log - mean_log_sd, ymax = mean_log + mean_log_sd), width = .1) 
  ylab("change in SCL") +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) 

int_plot

ggsave("plots/intPlot_cat_case1.pdf", width = 6.5, height = 4.8)

# grouped by category, but with individual spaghetti lines
ggplot(indSummary, aes(x=as.factor(subtrial), y=mean, group=interaction(cat, id), col=cat)) +
  geom_line(aes(alpha = 1)) +
  geom_line(data = catSummary_case1, aes(alpha = 1, group=cat, col = cat), size = 1.4)+
  geom_point(data = catSummary_case1, aes(alpha = 1, group=cat, col = cat), size = 3) +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) +
  ylab("log change in SCL") 

#ggsave("plots/intPlot_ind_case1.pdf", width = 7.5, height = 6)

# grouped by category2 (ie choice + outcome) -- not log transformed ####
# 
catSummary_case1 <- cleaned %>% # case 1: group by conditions
  group_by(cat2, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined),
            mean = mean(mean_baselined)) %>%
  ungroup()

catSummary_case2 <- cleaned %>%
  group_by(id, cat2, subtrial) %>%  #case 2: group by participants then conditions
  summarise(mean = mean(mean_baselined)) %>%
  ungroup() %>%
  group_by(cat2, subtrial) %>%
  summarise(mean_sd = sd(mean),
            mean = mean(mean)) %>%
  ungroup() 
  

indSummary <- cleaned %>%
  group_by(id, cat2, subtrial) %>%
  summarise(mean_sd = sd(mean_baselined),
            mean = mean(mean_baselined)) %>%
  ungroup()

int_plot <- ggplot(catSummary_case2,
                   aes(as.factor(subtrial), mean, group = cat2, col = cat2)) +
  geom_point(size = 3) +
  geom_line() +# +
  #geom_errorbar(aes(ymin=mean_log - mean_log_sd, ymax = mean_log + mean_log_sd), width = .1) 
  ylab("change in SCL") +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) 

int_plot

# ggsave("plots/intPlo_cat_allsep_case1.pdf", width = 6.5, height = 4.8)

# grouped by category, but with individual spaghetti lines
ggplot(indSummary, aes(x=as.factor(subtrial), y=mean, group=interaction(cat2, id), col=cat2)) +
  geom_line(aes(alpha = 1)) +
  geom_line(data = catSummary_case2, aes(alpha = 1, group=cat2, col = cat2), size = 1.4)+
  geom_point(data = catSummary_case2, aes(alpha = 1, group=cat2, col = cat2), size = 3) +
  theme_bw() +
  scale_x_discrete("interval", labels = c("1", "2", "3", "4", "outcome")) +
  ylab("log change in SCL") 

# ggsave("plots/intPlot_ind.pdf", width = 7.5, height = 6)

# why is there a difference beteween KISshock and KISnothing?

# a) check people with highest prop KISnothing/KIS shock
tmp <- cleaned %>%
  mutate(prevOutcome = lag(outcome)) %>%
  filter(subtrial == 3) %>%
  group_by(id, cat2) %>%
  summarise(mean = mean(mean_baselined), propShock = count(prevOutcome == "shock"))

tmp2 <- cleaned %>% filter(subtrial == 3) %>% group_by(id) %>% count(cat2)

tmp <- left_join(tmp, tmp2)
# replicating helen's analysis with baselined data ####

tmp <- scrdat %>%
  dplyr::filter(label2 == "int4" | label2 == "outcome") #20s averaged period only

tmp <- tmp %>% 
  mutate(label2 = case_when(
    label2 == "outcome" ~ "outcome",
    label2 == "int4" ~ "20s"
))

tmp <- left_join(tmp, baseline)

# now subtract baseMean from each mean for baselined data on each trial
baselined <- tmp %>%
  mutate(mean_baselined = mean - baseMean)

cleaned <- baselined %>% 
  dplyr::select(id, trial, cat, outcome, subtrial, label2, mean, baseMean, mean_baselined) %>%
  rename(label = label2)

tmp <- cleaned %>% dplyr::filter(label == "20s")

catSummary <- tmp %>%
  group_by(cat, subtrial) %>%
  summarise(sd = sd(mean_baselined),
            m = mean(mean_baselined), #mean in brackets reproduces helen's plots successfully
            n = n()) %>%
  ungroup()

ggplot(catSummary, aes(cat, m)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=m-sd/sqrt(n), ymax=m+sd/sqrt(n)), width=.05) + 
  labs(x="Choice", y="Mean SCR", title="SCR by Choice (averaged across trials)")

ggsave("plots/helen_baselined.pdf", width = 7.5, height = 6)

