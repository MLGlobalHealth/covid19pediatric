## not putting this into GitHub, it doesn't seem like it's mentioned in the paper

## todo: download this data and rerun.

library(here)
library(data.table)
library(tidyverse)
## source: https://wonder.cdc.gov/controller/saved/D76/D313F348
wonder1 = 
  fread(here("data/2019/1to19.txt")) %>% 
  select(cause=`ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[1,19)")

## source: https://wonder.cdc.gov/controller/saved/D76/D313F347
wonder2 = 
  fread(here("data/2019/0-130CauseList.txt")) %>% 
  select(cause=`ICD-10 130 Cause List (Infants)`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[0,1)")


## source: https://wonder.cdc.gov/controller/saved/D76/D313F345
wonder3 = 
  fread(here("data/2019/0to19-113CauseList.txt")) %>% 
  select(cause=`ICD-10 113 Cause List`,rate=`Crude Rate`,deaths=Deaths,pop=Population) %>% mutate(agegroup="[0,19)")

q=wonder2 %>% mutate(rate=round(100000*deaths/combined.pop,2)) %>% arrange(-rate)
(q %>% filter(grepl("#",cause)))[1:15,]

wonder = rbind(wonder2,wonder3) %>% filter(grepl("#",cause))
combined.pop = unique(wonder3$pop) # 81,625,416 0-19 year olds

covid$agegroup = "[0,19)"
q=rbind(wonder,covid) %>% mutate(rate=round(100000*deaths/combined.pop,2))  %>%  arrange(agegroup) #arrange(-rate)
q=q[!duplicated(cause,fromLast=T),] %>% arrange(-rate)
q = q %>% mutate(rank=1:nrow(q))
q %>% arrange(cause)
q[1:15,]

# merge categories
wonder2$cause[wonder2$cause == "#Accidents (unintentional injuries) (V01-X59)"] = "#Accidents (unintentional injuries) (V01-X59,Y85-Y86)"
wonder2$cause[wonder2$cause == "#Assault (homicide) (*U01,X85-Y09)"] = "#Assault (homicide) (*U01-*U02,X85-Y09,Y87.1)"
wonder$cause[grepl("heart",wonder$cause) & grepl("#",wonder$cause)]
wonder=rbind(wonder1,wonder2)
wonder$rankable = grepl("#",wonder$cause)
combined.pop = sum(unique(wonder$pop)) # 81,625,416 0-19 year olds

q=wonder %>% filter(rankable == TRUE) %>% group_by(cause) %>% 
  summarize(deaths=sum(deaths),pop=sum(pop)) %>% mutate(rate=deaths/combined.pop)  %>% 
  ungroup() %>% arrange(-rate)
q[1:15,]

covid = fread(here("data/2021-22/0to19.txt"),nrow=1) %>% select(cause=`Underlying Cause of death`,deaths=Deaths) %>%
  mutate(rate=deaths/combined.pop,pop=combined.pop)
q = rbind(q,covid) %>% arrange(-rate) %>% mutate(rate=round(rate*100000,2))
q[1:10,]
