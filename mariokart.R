library(gsheet)
library(dplyr)
library(lubridate)
library(tidyverse)
library(mice)
library(VIM)
library(lattice)
library(plyr)
#### Import 3 data sets
data.21 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1sX4Ffnf_EnYb3XsfQJx1_Y-iwJCCOYlU5TAbh7q4DFM/edit#gid=61841258')
data.20 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1KG1isuMrWhowBzsUHbP1BXB6vx0nnSffmJsleXmUSK8/edit?usp=sharing')
data.19 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1WDLhYTpkoakLlQF1yklGzIxJXY7Dqsw0Htrh1fPl2Mo/edit?usp=sharing')


### Erase empty/extra cells, rename columns

colnames <- c('day', 'date', 'tot_red', 'new_red', 'cons_red', 'tot_green', 'new_green', 'cons_green', 'tot_white', 'new_white', 'cons_white', 'tot_brown', 'new_brown', 'cons_brown', 'tot_yell', 'new_yell', 'cons_yell', 'cens_tot', 'cens_new', 'cens_cons')
colnames(data.21) <- colnames
colnames(data.20) <- colnames
data.21 <- data.21[-c(1, 366:369),]
data.20 <- data.20[-c(1, 367:467),]

data.19[, c('X14', "X15", "X16", "X17", "X18", "Consult Reason", "X20", "X21", "X22", "X23", "X24")] <- list(NULL)
colnames2 <- c('day', 'date', 'tot_red', 'new_red', 'tot_green', 'new_green', 'tot_white', 'new_white', 'tot_brown', 'new_brown', 'tot_yell', 'new_yell', 'cens_cons', 'cens_tot', 'cens_new')
colnames(data.19) <- colnames2
data.19 <- data.19[-c(1, 368:399),]

sapply(data.19, class)
sapply(data.20, class)

data.19$date <- mdy(data.19$date)
data.19$month <- month(ymd(data.19$date), label = TRUE, abbr = FALSE)
data.20$date <- mdy(data.20$date)
data.20$month <- month(ymd(data.20$date), label = TRUE, abbr = FALSE)
data.21$date <- mdy(data.21$date)
data.21$month <- month(ymd(data.21$date), label = TRUE, abbr = FALSE)

data.19$AY <- 2019
data.20$AY <- 2020
data.21$AY <- 2021

data.19.f <- within(data.19, rm('cens_cons', 'cens_tot', 'cens_new'))
data.20.f <- within(data.20, rm('cens_cons', 'cens_tot', 'cens_new'))
data.21.f <- within(data.21, rm('cens_cons', 'cens_tot', 'cens_new'))
colnames(data.19.f)

data.19.t <- data.19.f[, c("date", "tot_red", "tot_green", "tot_white","tot_brown", "tot_yell", "month", "AY" )]
data.20.t <- data.20.f[, c("date", "tot_red", "tot_green", "tot_white","tot_brown", "tot_yell", "month", "AY" )]
data.21.t <- data.21.f[, c("date", "tot_red", "tot_green", "tot_white","tot_brown", "tot_yell", "month", "AY" )]

data.20.t %>%
  subset(date <= '2020-03-31') -> df.pre

data.20.t %>%
  subset(date >= '2020-04-01') -> df.post

pre <- rbind(data.19.t, df.pre)
post <- rbind(df.post, data.21.t)
sapply(pre, class)


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
  
imp.pre <- mice(pre, m=5, maxit=40)
pre.fin <- complete(imp.pre)


imp.post <- mice(post, m=5, maxit=40)
post.fin <- complete(imp.post)

pre.fin$total <- rowSums(pre.fin[,4:8])
post.fin$total <- rowSums(post.fin[,4:7])
pre.fin$ptperteam <- pre.fin$total/5
post.fin$ptperteam <- post.fin$total/4
pre.fin$ptdays <- pre.fin$ptperteam * 24
post.fin$ptdays <- post.fin$ptperteam *22.5

median(pre.fin$total)/5
median(post.fin$total)/4



ggplot(data=pre.fin, aes(x=date, y = total)) + 
  geom_bar(stat = 'identity')

plot2 <- ggplot(NULL, aes(x=date, y=ptperteam)) + 
    geom_line(data = pre.fin) +
    geom_line(data = post.fin) +
    scale_color_discrete(labels=c('Pre', 'Post'), values = c("blue", "red")) +
    labs(title = "Patients per Team", x='Date', y="Patients per Team")
plot2

boxplot <- ggplot(NULL, aes(x=date, y=ptperteam)) + 
  geom_boxplot(data = pre.fin) +
  geom_boxplot(data = post.fin) +
  labs(title = "Patients per Team", x='Date', y="Patients per Team")

boxplot

ggplot( aes(x=, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")


don <- bind_rows(pre.fin, post.fin)
colnames(don)
don <- don[,c("date", "ptperteam")]

library(dygraphs)
library(xts)

neat <- xts(x = don$ptperteam, order.by = don$date)           
p <- dygraph(neat) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)



library(htmlwidgets)
saveWidget(p, file=paste0(getwd(), "/dygraphs318.html"))
