library(data.table)
library(tidyverse)
library(here)
library(readxl)

# US population data for 2021 comes from 
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/national/asrh/nc-est2021-agesex-res.csv
# 

pop_us = read.csv(here("data/nc-est2021-agesex-res.csv")) %>% filter(SEX == 0) %>% 
  mutate(Age=AGE,pop_age = POPESTIMATE2021) %>%
  select(Age,pop_age) %>% filter(Age <= 100)
pop_us$pop_age[1]

sum(pop_us$pop_age[pop_us$Age <= 19])

## COVID-19 (U07.1) as an underlying cause of death for the study period
## see README.md for source of data
## see .txt files for date on which the data was accessed

covid = fread(here("data/age-cats.txt"),nrows=5) %>% select(cause=`Underlying Cause of death`,deaths=Deaths,agegroup=`Five-Year Age Groups`)
 
covid$agegroup = c("[0,1)","[1,5)","[5,10)","[10,15)","[15,20)") #,"[0-20)")
covid = left_join(covid, pop_us %>% filter(Age < 20) %>% group_by(agegroup=cut(Age,c(0,1,5,10,15,20,100),right=F)) %>%   
                    summarise(pop = sum(pop_age)),by="agegroup")
covid$rate = covid$deaths / covid$pop * 100000  
round(covid$rate,3)
sum(covid$pop)


## 2019 data on top 10 leading causes of death
wonder = rbind(
  fread(here("data/2019/0.txt"),nrows=15) %>% select(cause=`UCD - 15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[0,1)"),
  fread(here("data/2019/1to4.txt"),nrows=15) %>% select(cause=`UCD - 15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[1,5)"),
  fread(here("data/2019/5to9.txt"),nrows=14) %>% select(cause=`UCD - 15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[5,10)"),
  fread(here("data/2019/10to14.txt"),nrows=15) %>% select(cause=`UCD - 15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[10,15)"),
  fread(here("data/2019/15to19.txt"),nrows=15) %>% select(cause=`UCD - 15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[15,20)"))
wonder[1]
# population in 2019:
sum((wonder %>% filter(cause == "#Influenza and pneumonia (J09-J18)"))$pop) ## 2019

## 2019 data on all causes of death
wonder.allcause = rbind(
  fread(here("data/2019/0-allcause.txt"),nrows=1) %>% select(total.deaths=Deaths) %>% mutate(agegroup="[0,1)"),
  fread(here("data/2019/1to4-allcause.txt"),nrows=1) %>% select(total.deaths=Deaths) %>% mutate(agegroup="[1,5)"),
  fread(here("data/2019/5to9-allcause.txt"),nrows=1) %>% select(total.deaths=Deaths) %>% mutate(agegroup="[5,10)"),
  fread(here("data/2019/10to14-allcause.txt"),nrows=1) %>% select(total.deaths=Deaths) %>% mutate(agegroup="[10,15)"),
  fread(here("data/2019/15to19-allcause.txt"),nrows=1) %>% select(total.deaths=Deaths) %>% mutate(agegroup="[15,20)"))

wonder$rate = as.numeric(wonder$rate)
wonder = wonder[complete.cases(wonder),]
wonder$rate = round(wonder$deaths / wonder$pop * 100000,2)

#all_deaths = as.numeric(fread(here("data/2019/all-ages.txt"),nrows=1) %>% select(Deaths))

##


agegroups = levels((pop_us %>% filter(Age < 20) %>% group_by(agegroup=cut(Age,c(0,1,5,10,15,20,100),right=F)))$agegroup)

covid.plot = covid
covid.plot$agegroup = factor(covid.plot$agegroup,levels=agegroups[1:5],
                             labels = c("< 1 year olds","1-4 year olds","5-9 year olds","10-14 year olds","15-19 year olds"))

g = ggplot(covid.plot, aes(x=agegroup,y=rate))
g = g +   geom_bar(stat="identity",
           position = position_dodge2(preserve='single'),fill="maroon")  
g = g + scale_y_continuous(expand=c(0,0),limits=c(0,4.6))
g = g + labs(y = "Covid-19 deaths per 100,000\n", x="\nAge group") 
g = g + ggtitle("Covid-19 death rate in the US: August 1, 2021-July 31st, 2022")
g = g + theme_bw()
g = g +  geom_text(aes(label=round(rate,1)),vjust=-.25) #, position=position_dodge(width=0.9*86400), vjust=-0.25)

g
ggsave(g,filename = here("figures/US-death-rate-age-groups-21-22.png"),width=7,height=4)
# ggsave(g,filename = here("figures/US-death-rate-age-groups-21-22.svg"),width=7,height=4)

d = rbind(covid,wonder) %>% left_join(wonder.allcause,by="agegroup")

## for text in paper: 
## Covid-19 accounted for 0.7% (<1 year old),
## 2.5% (1-4 year olds), 3.8% (5-9 year olds), 3.5% (10-14 year olds), and 3.7% (15-19 year olds) of all causes of death by age group
pct = d %>%
  group_by(agegroup) %>% mutate(percent = round(100*deaths/total.deaths,1),
                                old.percent=deaths/sum(deaths)) %>% arrange(-percent)

pct$agegroup = factor(pct$agegroup,levels=agegroups[1:5],
                             labels = c("< 1 year olds","1-4 year olds","5-9 year olds","10-14 year olds","15-19 year olds"))

pct %>% filter(cause ==  "COVID-19") %>% arrange(agegroup)

for(ag in unique(d$agegroup)) {
  print(sprintf("Age group: %s",as.character(ag)))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(-rate) %>% select(cause,rate,deaths))
  print(d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),4)) %>% arrange(-deaths) %>% select(cause,rate,deaths))
  
  print(max(diff((d %>% filter(agegroup == ag)  %>% mutate(rate=round(as.numeric(rate),1)) %>% arrange(-deaths) %>% select(cause,rate,deaths))$rate),na.rm=T))
}

d$agegroup = factor(d$agegroup,levels=agegroups)
d$cause[d$cause == "COVID-19"] = "#COVID-19 (U07.1)"


write.csv(d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
#            mutate(rank =  rank(-rate,ties.method="min"))  %>% filter(rank <= 10) %>% 
            mutate(rank =  rank(-deaths,ties.method="min"))  %>% filter(rank <= 10) %>% 
            mutate(percent=round(deaths/total.deaths * 100,1)) %>% 
            arrange(agegroup,-deaths) %>% select(cause,rate,deaths,rank,percent,
                                                 agegroup),
          file=here("results/WONDER-agegroups-2021-22.csv"),row.names=F)

## text in manuscript:
## ranks are: #7 (<1 year old), #7 (1-4 year olds), #6 (5-9 year olds), #6 (10-14 year olds), and #5 (15-19 year olds). 
d %>% group_by(agegroup) %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
  # mutate(rank =  rank(-rate,ties.method="min")) %>% arrange(agegroup,-deaths) %>% filter(grepl("COVID",cause)) %>% arrange(cause)
  mutate(rank =  rank(-deaths,ties.method="min")) %>% arrange(agegroup,-deaths) %>% filter(grepl("COVID",cause)) %>% arrange(cause)

### todo: refactor this code so that it is part of the code above!
### compare to other leading causes of death: ages 0-19 together

covid0_19 = fread(here("data/age-cats.txt"),nrows=6)[6] %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>%
  mutate(agegroup="[0,20)")

covid0_19$pop = as.numeric(pop_us %>% filter(Age < 20) %>% summarise(pop = sum(pop_age)))
covid0_19$rate = covid0_19$deaths / covid0_19$pop * 100000
sum(covid0_19$pop)


allcause0to19 = fread(here("data/2019/0to19-allcause.txt"),nrows=1) %>% 
 select(total.deaths=Deaths) %>% mutate(agegroup="[0,20)")
d = rbind(
  fread(here("data/2019/0to19.txt"),nrows=15) %>% select(cause=`UCD - 15 Leading Causes of Death`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% 
    mutate(agegroup="[0,20)"),covid0_19) %>% left_join(allcause0to19,by="agegroup")

which(round(d$rate,1) != round(d$deaths/d$pop*100000,1))
d

pct = d %>%
  group_by(agegroup) %>% mutate(percent = deaths/total.deaths) %>% arrange(-percent)
pct %>%  arrange(agegroup)

d$cause[d$cause == "COVID-19"] = "#COVID-19 (U07.1)"

print(d %>% mutate(rate=round(as.numeric(rate),2),
                   rank =  rank(-deaths,ties.method="min")) %>% arrange(-deaths))

write.csv(d %>% filter(agegroup == "[0,20)") %>% mutate(rate=round(as.numeric(rate),1)) %>% drop_na() %>%
            mutate(rank =  rank(-deaths,ties.method="min")) %>% 
            filter(rank <= 10) %>% mutate(
              percent=round(deaths/total.deaths*100,1)
              ) %>% 
            arrange(-deaths) %>% select(cause,rate,deaths,rank,percent), file = here("results/WONDER-0-19.csv"),row.names=F)


d = rbind(
  fread(here("data/2019/infectious.txt"),nrows=15) %>% 
    select(cause=`UCD - ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths,
           pop=Population) %>% mutate(agegroup="[0,20)"),covid0_19)
d %>% filter(cause == "#Septicemia (A40-A41)")
d$rate = round(d$deaths / d$pop * 100000  ,2)

write.csv(d %>% filter(agegroup == "[0,20)") %>% group_by(agegroup)  %>% drop_na() %>% select(-pop) %>%
            mutate(rank =  rank(-deaths,ties.method="min")) %>% arrange(agegroup,-deaths),file = here("results/WONDER-0-19-infectious.csv"),row.names=F)

