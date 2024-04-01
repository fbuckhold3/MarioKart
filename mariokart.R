library(gsheet)
library(dplyr)
library(lubridate)
library(tidyverse)
library(mice)
library(VIM)
library(lattice)
#### Import 3 data sets
setwd('~/Documents/GitHub/MarioKart')
#### data.21 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1sX4Ffnf_EnYb3XsfQJx1_Y-iwJCCOYlU5TAbh7q4DFM/edit#gid=61841258')
#### data.20 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1KG1isuMrWhowBzsUHbP1BXB6vx0nnSffmJsleXmUSK8/edit?usp=sharing')
### data.19 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1WDLhYTpkoakLlQF1yklGzIxJXY7Dqsw0Htrh1fPl2Mo/edit?usp=sharing')

data.21 <- read_csv('FY21 daily tally sheet - ACADEMIC TEAMS.csv')
data.20 <- read_csv('FY20 daily tally sheet - ACADEMIC TEAMS.csv')
data.19 <- read_csv('FY19 daily tally sheet - ACADEMIC TEAMS.csv')

### Erase empty/extra cells, rename columns

colnames <- c('day', 'date', 'tot_red', 'new_red', 'cons_red', 'tot_green', 'new_green', 'cons_green', 'tot_white', 'new_white', 'cons_white', 'tot_brown', 'new_brown', 'cons_brown', 'tot_yell', 'new_yell', 'cons_yell', 'cens_tot', 'cens_new', 'cens_cons')

data.21 <- data.21[-c(1, 366:369),]
data.20 <- data.20[-c(1, 367:467),]

data.19[, c('X14', "X15", "X16", "X17", "X18", "Consult Reason", "X20", "X21", "X22", "X23", "X24")] <- list(NULL)
colnames2 <- c('day', 'date', 'tot_red', 'new_red', 'tot_green', 'new_green', 'tot_white', 'new_white', 'tot_brown', 'new_brown', 'tot_yell', 'new_yell', 'cens_cons', 'cens_tot', 'cens_new')
colnames(data.19) <- colnames2
data.19 <- data.19[-c(1, 368:399),]

### Data processing
### Convert text dates to date format

data.19$date <- mdy(data.19$date)
data.19$month <- month(ymd(data.19$date), label = TRUE, abbr = FALSE)
data.20$date <- mdy(data.20$date)
data.20$month <- month(ymd(data.20$date), label = TRUE, abbr = FALSE)
data.21$date <- mdy(data.21$date)
data.21$month <- month(ymd(data.21$date), label = TRUE, abbr = FALSE)

data.19$AY <- 2019
data.20$AY <- 2020
data.21$AY <- 2021

### Remove totals so missing data can be imputed, then create datasets for mice package to impute missing values

data.19.f <- within(data.19, rm('cens_cons', 'cens_tot', 'cens_new'))
data.20.f <- within(data.20, rm('cens_cons', 'cens_tot', 'cens_new'))
data.21.f <- within(data.21, rm('cens_cons', 'cens_tot', 'cens_new'))
colnames(data.19.f)

data.19.t <- data.19.f[, c("date", "tot_red", "tot_green", "tot_white","tot_brown", "tot_yell", "month", "AY" )]
data.20.t <- data.20.f[, c("date", "tot_red", "tot_green", "tot_white","tot_brown", "tot_yell", "month", "AY" )]
data.21.t <- data.21.f[, c("date", "tot_red", "tot_green", "tot_white","tot_brown", "tot_yell", "month", "AY" )]

### Define datasets pre and post intervention:
# Preintervention
data.20.t %>%
  subset(date <= '2020-03-31') -> df.pre
# Postintervention
data.20.t %>%
  subset(date >= '2020-04-01') -> df.post

### Create finalized datasets, one for the days before the start of the intervention of the model and one for post-intervention. Will remove excess columns and data that was part of previous datasets so as not to confound imputation via mice package. 

pre <- rbind(data.19.t, df.pre)
post <- rbind(df.post, data.21.t)

post[,c("tot_brown")] <- list(NULL)

pre %>%
  mutate(
    red = as.numeric(tot_red),
    green = as.numeric(tot_green),
    white = as.numeric(tot_white),
    brown = as.numeric(tot_brown),
    yellow = as.numeric(tot_yell)) -> pre

post %>%
  mutate(
    red = as.numeric(tot_red),
    green = as.numeric(tot_green),
    white = as.numeric(tot_white),
    yellow = as.numeric(tot_yell)) -> post

pre[,c("tot_red", "tot_green", "tot_white","tot_brown", "tot_yell")] <-list(NULL)
post[,c("tot_red", "tot_green", "tot_white", "tot_yell")] <- list(NULL)
post <- post[-c(455),]

### Impute missing data via Monte Carlo approach using the MICE package
  
imp.pre <- mice(pre, m=5, maxit=40)
pre.fin <- complete(imp.pre)


imp.post <- mice(post, m=5, maxit=40)
post.fin <- complete(imp.post)

### Sum up totals per row:
pre.fin$total <- rowSums(pre.fin[,4:8])
post.fin$total <- rowSums(post.fin[,4:7])

### Calculate patients per team, mean and median. Data named pre.fin is before implementation of new schedule, post.fin is after. 
pre.fin$ptperteam <- pre.fin$total/5
post.fin$ptperteam <- post.fin$total/4

## Descriptive statistics
summary(pre.fin)
summary(post.fin)

### Simple calculations comparing median patient census 

### Pre (24 days working) patients 
median(pre.fin$ptperteam)*24
quantile(pre.fin$ptperteam, 0.25)*24
quantile(pre.fin$ptperteam, 0.75)*24

### Post (22.5 days working) patients
median(post.fin$ptperteam)*22.5
quantile(post.fin$ptperteam, 0.25)*22.5
quantile(post.fin$ptperteam, 0.75)*22.5

### Examine difference in patients per team for statistical significance
t.test(pre.fin$ptperteam, post.fin$ptperteam, alternative = "two.sided", var.equal = FALSE)
t.test(pre.fin$ptperteam*24, post.fin$ptperteam*22.5, alternative = "two.sided", var.equal = FALSE)

### Graphing results:
## box plots of average daily census
pre.fin %>%
  select(red, green, white, brown, yellow) %>%
  pivot_longer(., cols = c(red, green, white, brown, yellow), names_to = 'Team', values_to = 'Avg Daily Census') %>%
  ggplot(aes(x = Team, y= `Avg Daily Census`)) + geom_boxplot(fill = c('chocolate4', 'green','red', 'white', 'yellow'), alpha=0.3) + labs(title= "Box-plot of Average Daily Census before implementating the MarioKart System", subtitle = "(Five inpatient teams)") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
post.fin %>%
  select(red, green, white, yellow) %>%
  pivot_longer(., cols = c(red, green, white, yellow), names_to = 'Team', values_to = 'Avg Daily Census') %>%
  ggplot(aes(x = Team, y= `Avg Daily Census`)) + geom_boxplot(fill = c('green','red', 'white', 'yellow'), alpha=0.3) + labs(title= "Box-plot of Average Daily Census after implementating the MarioKart System", subtitle = "(Four inpatient teams)") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


# Rolling census

pre.fin$Schedule <- 'Traditional'
post.fin$Schedule <- 'MarioKart'
plot.data <- bind_rows(pre.fin, post.fin)

boxplot <- ggplot(plot.data, aes(x=Schedule, y=total, fill=Schedule))+
  geom_boxplot() +
  labs(title = "Comparison of Total Daily Census", y="Number of patients") +
  theme(plot.title = element_text(hjust = 0.5), ) +
  scale_fill_discrete(name="")
boxplot

boxplot2 <- ggplot(plot.data, aes(x=Schedule, y=ptperteam, fill=Schedule))+
  geom_boxplot() +
  labs(title = "Comparison of Resident Daily Census", y="Number of patients") +
  theme(plot.title = element_text(hjust = 0.5), ) +
  scale_fill_discrete(name="")

boxplot2


### Duty hours
library(lubridate)
duty <- read.csv('/Users/fredbuckhold/Documents/GitHub/MarioKart/logs ver 2.csv')

### Convert dates
duty$date <- mdy_hm(duty$Log.Date)
duty$date <- date(duty$date)





### create list of sequential dates over study period to merge duty hour data and create dataframe for analysis.
date <- as.Date('2018-07-01')
len <- 1096

as.data.frame(seq(date, by = 'day', length.out = len)) -> dates
colnames(dates) <- c("date")

### Investigation of 80-hour violations over 4 weeks rather than weekly reporting
duty %>%
  dplyr::filter(Rule == 'ACGME 80 Hour') %>%
  filter(str_detect(Description, '320'))

duty %>% 
  pivot_wider(names_from = Rule,
              values_from = Log.Date) %>%
  mutate(short = if_else(is.na(`ACGME Short Break`), 0, 1),
         `80_hour` = if_else(is.na(`ACGME 80 Hour`), 0, 1)) %>%
  select (date, Rotation, short, `80_hour`) %>%
  right_join(dates) %>%
  arrange(date)-> duty.hour 

duty.hour[c('short', '80_hour')][is.na(duty.hour[c('short', '80_hour')])] <- 0

### Now need to sort in GIM v. total; combine same dates into frequency...
duty.hour %>% 
  subset(date <= '2020-03-31') -> pre.duty

duty.hour %>% 
  subset(date >= '2020-04-01') -> post.duty

### Create dataframe for general inpatient service, as compared to the entire program
duty.hour %>%
  filter(Rotation == 'MED:IM:SLUH:YELLOW' | Rotation == 'MED:IM:SLUH:RED' | Rotation == "MED:IM:SLUH:WHITE" | Rotation == "MED:IM:SLUH:GREEN" | Rotation == "MED:IM:SLUH:BROWN" | Rotation == "MED:IM:SLUH:ACE" | Rotation == "MED:IM:SLUH:SLU FLOOR (TBA)") -> gim.duty

gim.duty %>%
  subset(date <= '2020-03-31') -> pre.gim.duty

gim.duty %>%
  subset(date >= '2020-04-01') -> post.gim.duty
  
### looking at short breaks:
#### total duty hours
pre.duty %>%
  select(date, short) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(short)) -> pre.duty.short

sum(pre.duty.short$total) / 20

post.duty %>%
  select(date, short) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(short)) -> post.duty.short 

sum(post.duty.short$total) / 15

t.test(pre.duty.short$total, post.duty.short$total, alternative = "two.sided", var.equal = FALSE)

#### just for GIM:
pre.gim.duty %>% 
  select(date, short) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(short)) -> pre.gim.short

post.gim.duty %>%
  select(date, short) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(short)) -> post.gim.short

sum(pre.gim.short$total) / 20
sum(post.gim.short$total) / 15

t.test(pre.gim.short$total, post.gim.short$total, alternative = "two.sided", var.equal = FALSE)


### 80 hour

pre.duty %>%
  select(date, `80_hour`) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(`80_hour`)) -> pre.duty.eight

post.duty %>%
  select(date, `80_hour`) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(`80_hour`)) -> post.duty.eight

sum(pre.duty.eight$total)/20
sum(post.duty.eight$total)/15

t.test(pre.duty.eight$total, post.duty.eight$total, alternative = "two.sided", var.equal = FALSE)

#### just for GIM:
pre.gim.duty %>% 
  select(date, `80_hour`) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(`80_hour`)) -> pre.gim.eight

post.gim.duty %>%
  select(date, `80_hour`) %>%
  group_by(date) %>%
  dplyr::summarise(total = sum(`80_hour`)) -> post.gim.eight

sum(pre.gim.eight$total) / 20
sum(post.gim.eight$total) / 15

t.test(pre.gim.eight$total, post.gim.eight$total, alternative = "two.sided", var.equal = FALSE)










