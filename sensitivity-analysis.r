library(data.table)
library(here)
library(tidyverse)

## COVID-19 (U07.1) as an underlying cause of death for the study period
## see .txt files for date on which the data was accessed
## as more data becomes available, change the end date -- recent months are incomplete
END_DATE = as.Date("2022-09-01")

covid = fread(here("data/monthly.txt")) # fine to ignore the warning
dates = as.Date(strptime(paste0(covid$`Month Code`,"/01"),format = "%Y/%m/%d"))

covid$month = month(dates)
covid$year = year(dates)
covid = covid[complete.cases(covid),]
plot(covid$year + covid$month/12,covid$Deaths,ty="l",col="red")
covid$date = lubridate::my(paste(covid$month,covid$year))


g = ggplot(covid %>% filter(date <= END_DATE),aes(x=date,y=Deaths))
g = g + geom_bar(stat="identity")

g = g + theme_bw() + ylab("")
g = g + 
  scale_x_date(breaks = seq(as.Date("2020-04-01"), as.Date("2022-07-01"), by="3 months"), 
               date_labels = "%b\n%Y",expand=c(.5/12,0)) + scale_y_continuous(limits=c(0,180),
                                                                              expand=c(0,0))
g = g + ylab("Deaths")
g = g + xlab("")
g = g +  geom_text(aes(label=Deaths),vjust=-.25)

g
ggsave(here("figures/timeseries.png"),g,width=10*.8,height=8*.6)
# ggsave(here("figures/timeseries.svg"),g,width=10*.8,height=8*.6)

# annual1 = NULL
# dd = covid$Deaths
# for(i in 1:(30-12)) {
#   annual1 = c(annual1,sum(dd[i:(i+12-1)]))
# }

# df=data.frame(x=annual1,date=paste(covid$month[ii],year=covid$year[ii]))
# df = df[c(1,8,12),]
# 
# 
# 
# ## note the hard-coded numbers here from 2019
# df = rbind(df,data.frame(x=c(472,297),date=c("Influenza/pneumonia","Cerebrovascular diseases")))

annual1 = zoo::rollsum(covid$Deaths,12,na.pad=TRUE,align = "right") 
ii = which(!is.na(annual1) & covid$date <= as.Date(END_DATE)) #(28-17+1):29
date_periods = covid$date[ii]
date_periods = 
  sprintf("Covid-19: %d/%d to %d/%d",month(date_periods-330),year(date_periods-330),month(date_periods),year(date_periods))
df=data.frame(x=annual1[ii],date=date_periods,
              order=order(covid$date[ii]),type="COVID-19 deaths in period")
# note hard-coding here! the code only works as deaths are between 297-471 and 472-867
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
g = g + scale_y_continuous(expand=c(0,0),limits=c(0,925),breaks=seq(0,900,200))
g = g + geom_text(aes(label = rank), hjust=1.2, colour = "white")
g = g + theme_bw()
g = g + theme(legend.position="none",plot.margin=margin(5,20,20,5))
g = g +  geom_text(aes(label=x),hjust=-.15)
g
ggsave(here("figures/ranks.png"),g,width=10*.8,height=8*.6)
# ggsave(here("figures/ranks.svg"),g,width=10*.8,height=8*.6)
