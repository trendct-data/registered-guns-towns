library(readxl)
library(dplyr)
library(ggplot2)
library(ctnamecleaner)
guns <- read_excel("data/16-591.xlsx", sheet=1)
guns <- ctnamecleaner(CITY_OR_TOWN, guns)
guns <- guns %>% group_by(real.town.name, Year, GUN_TYPE) %>%
  summarise(Count=sum(Count, na.rm=T)) %>%
  filter(!is.na(real.town.name)) %>%
  filter(real.town.name!="M")
## Overall by year

overall <- guns %>%
  group_by(Year) %>%
  summarise(Count=sum(Count)) %>%
  filter(Year<=2016)

gg <- ggplot(overall, aes(x=Year, y=Count))
gg <- gg + geom_line()

gg

## Overall by year by gun type

overall_type <- guns %>%
  group_by(Year, GUN_TYPE) %>%
  summarise(Count=sum(Count)) %>%
  filter(Year<=2016)

gg <- ggplot(overall_type, aes(x=Year, y=Count, group=GUN_TYPE, color=GUN_TYPE))
gg <- gg + geom_line()

gg

## Overall by year by town

overall_town<- guns %>%
  group_by(real.town.name, Year) %>%
  summarise(Count=sum(Count)) %>%
  filter(Year<=2016)

gg <- ggplot(overall_town, aes(x=Year, y=Count))
gg <- gg + geom_line()
gg <- gg + facet_wrap(~real.town.name, ncol=4)

gg

