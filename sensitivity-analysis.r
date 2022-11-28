library(data.table)
library(here)
library(tidyverse)

## COVID-19 (U07.1) as an underlying cause of death for the study period
## see .txt files for date on which the data was accessed

covid = NULL
year = 2020
for(month in 4:12) {
  covid = rbind(covid,
                fread(here(sprintf("data/monthly/%d-%02d.txt",year,month)),nrows=1) %>% mutate(month=month,year=year))
}
sum(covid$Deaths)
year = 2021
for(month in 1:12) {
  covid = rbind(covid,
                fread(here(sprintf("data/monthly/%d-%02d.txt",year,month)),nrows=1) %>% mutate(month=month,year=year))
}
sum(covid$Deaths)

year = 2022
for(month in 1:8) {
  covid = rbind(covid,
                fread(here(sprintf("data/monthly/%d-%02d.txt",year,month)),nrows=1) %>% mutate(month=month,year=year))
}
covid[covid$year == 2022,"Deaths"]

plot(covid$year + covid$month/12,covid$Deaths,ty="l")
covid$date = lubridate::my(paste(covid$month,covid$year))
g = ggplot(covid,aes(x=date,y=Deaths))
g = g + geom_bar(stat="identity")

g = g + theme_bw() + ylab("")
g = g + 
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-07-01"), by="3 months"), 
               date_labels = "%b\n%Y",expand=c(.5/12,0)) + scale_y_continuous(limits=c(0,180),
                                                                              expand=c(0,0))
g = g + ylab("Deaths")
g = g + xlab("")
# g = g + geom_title("")
g = g +  geom_text(aes(label=Deaths),vjust=-.25)

g
ggsave(here("figures/timeseries.png"),g,width=10*.8,height=8*.6)

annual1 = NULL
dd = covid$Deaths
for(i in 1:(30-12)) {
  # print(length(dd[i:(i+12-1)]))
  # print(paste(i,i+12-1))
  annual1 = c(annual1,sum(dd[i:(i+12-1)]))
}
ii = (28-17+1):29
df=data.frame(x=annual1,date=paste(covid$month[ii],year=covid$year[ii]))
df = df[c(1,8,12),]
df = rbind(df,data.frame(x=c(472,297),date=c("Influenza/pneumonia","Cerebrovascular diseases")))

date_periods = covid$date[ii]
date_periods = 
  sprintf("Covid-19: %d/%d to %d/%d",month(date_periods-330),year(date_periods-330),month(date_periods),year(date_periods))
df=data.frame(x=annual1,date=date_periods,
              order=order(covid$date[ii]),type="COVID-19 deaths in period")
df$rank = "Rank 8"
df$rank[df$x < 472] = "Rank 9"
df$rank[df$x < 297] = "Rank 10"
df = rbind(df,data.frame(x=c(867,472,297),date=c("Heart disease in 2019",
                                                 "Influenza/pneumonia in 2019",
                                                 "Cerebrovascular diseases in 2019"),
                         type="Deaths in 2019",order=c(20,6.5,.5),
                         rank=c("Rank 7 in 2019","Rank 8 in 2019","Rank 9 in 2019")))

df = df[order(df$order),]
g = ggplot(df,aes(x=factor(date,levels=date),y=x,fill=type))
g = g + geom_bar(stat="identity") + ylab("\nDeaths") + coord_flip() + xlab("")
g = g + scale_y_continuous(expand=c(0,0),limits=c(0,875),breaks=seq(0,875,125))
g = g + geom_text(aes(label = rank), hjust=1.2, colour = "white")
g = g + theme_bw()
g = g + theme(legend.position="none",plot.margin=margin(5,20,20,5))
g
ggsave(here("figures/ranks.png"),g,width=10*.8,height=8*.6)
