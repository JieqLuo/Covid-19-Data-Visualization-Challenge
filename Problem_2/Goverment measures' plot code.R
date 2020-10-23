

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
Goverment_measures<-read.csv("Cleaned_Countries_measures.csv",stringsAsFactors = FALSE)


Goverment_measures$COUNTRY<-as.factor(Goverment_measures$COUNTRY)
Goverment_measures$CATEGORY<-as.factor(Goverment_measures$CATEGORY)


#Change date column's data to create a dataframe which record govermenrt's measure number at different dates for different countries. 
Goverment_measure_DF<-Government_measures_filter %>% group_by(COUNTRY,CATEGORY) %>%
  summarise(measures_number=length(CATEGORY))
Goverment_measure_DF<-as.data.frame(Count_measure_DF)
Label<-c("India","Sri Lanka","Bangladesh","Pakistan","Brazil","Egypt","Indonesia")


ggplot(Goverment_measure_DF,aes(x=COUNTRY,y=measures_number,fill=CATEGORY))+  geom_bar(stat="identity",position = 'dodge')+
  geom_text(aes(label=measures_number), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(xlab="Countries",ylab="Number of Measures",title="Government's Measures for Seven Countries")

       