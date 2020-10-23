#Code is designed to clean Covid-19 data and do data preparation for data visulization. 
#After finishing code, dataset will consists of date, different countries with added words 
#which show a column is confirmed data, Recovery data, Death data or goverment measures count

library(readxl)
library(readr)
library(xlsx)
library(lubridate)
library(dplyr)
library(tidyr)

#input government measures dataset
Government_measures<-read_excel("acaps_covid19_government_measures_dataset.xlsx",sheet=2,col_names = TRUE)

#Rename column names and filter to get wanted columns , then remove na data for governments'measure column and date column
Government_measures_filter<- Government_measures%>%select(COUNTRY,DATE_IMPLEMENTED,CATEGORY,MEASURE,COMMENTS)
Government_measures_filter<-Government_measures_filter[Government_measures_filter$COUNTRY %in% c("India","Sri Lanka","Bangladesh","Pakistan","Brazil","Egypt","Indonesia"),]
Government_measures_filter<-Government_measures_filter[!is.na(Government_measures_filter$DATE_IMPLEMENTED),]
Government_measures_filter<-Government_measures_filter[!is.na(Government_measures_filter$MEASURE),]

#Change date column's data to create a dataframe which record govermenrt's measure number at different dates for different countries. 
Government_measures_filter$DATE_IMPLEMENTED<-as.factor(Government_measures_filter$DATE_IMPLEMENTED)
Count_measure_DF<-Government_measures_filter %>% group_by(COUNTRY,DATE_IMPLEMENTED) %>%
  summarise(measures_number=length(DATE_IMPLEMENTED))
Count_measure_DF<-as.data.frame(Count_measure_DF)

#set date data as date type and order the dataframe according to countries and dates
Count_measure_DF$DATE_IMPLEMENTED<-as.Date(Count_measure_DF$DATE_IMPLEMENTED)
Count_measure_DF<-Count_measure_DF[order(Count_measure_DF$COUNTRY,Count_measure_DF$DATE_IMPLEMENTED),]

#save countires' goverment measures as a csv file after cleaning data
Government_measures_filter<-Government_measures_filter[order(Government_measures_filter$COUNTRY,Government_measures_filter$DATE_IMPLEMENTED),]
write_csv(Government_measures_filter,file="Cleaned_Countries_measures.csv",  col_names=TRUE)

#import covid-19 confirm dataset, recovery dataset and death dataset
Confirmed_data<-read.csv("time_series_covid19_confirmed_global.csv",stringsAsFactors = FALSE)
Confirmed_data<-Confirmed_data %>%select(Country.Region,India,Sri.Lanka,Bangladesh,Pakistan,Brazil,Egypt,Indonesia)
names(Confirmed_data)[1]<-"Date"
Confirmed_data<- Confirmed_data[-c(1,2,3),]

Recovery_data<-read.csv("time_series_covid19_recovered_global.csv",stringsAsFactors = FALSE)
Recovery_data<-Recovery_data %>%select(Country.Region,India,Sri.Lanka,Bangladesh,Pakistan,Brazil,Egypt,Indonesia)
names(Recovery_data)[1]<-"Date"
Recovery_data<- Recovery_data[-c(1,2,3),]


Death_data<-read.csv("time_series_covid19_deaths_global.csv",stringsAsFactors = FALSE)
Death_data<-Death_data %>%select(Country.Region,India,Sri.Lanka,Bangladesh,Pakistan,Brazil,Egypt,Indonesia)
names(Death_data)[1]<-"Date"
Death_data<- Death_data[-c(1,2,3),]

#rename countries'name
names(Confirmed_data)[3]<-"Sri Lanka"
names(Recovery_data)[3]<-"Sri Lanka"
names(Death_data)[3]<-"Sri Lanka"

#rename columns' name for different datasets with unique words
names_confirm_column<-names(Confirmed_data)
names_recovery_column<-names(Recovery_data)
names_death_column<-names(Death_data)

renameColumns<-function(names_of_column,add_word){renames_column<-c()
  for (i in names_of_column){
  renames_column<-c(renames_column,paste(i,add_word))}
  return(renames_column)
}

colnames(Confirmed_data)<-renameColumns(names_confirm_column,"Confirm")
colnames(Recovery_data)<-renameColumns(names_confirm_column,"Recovery")
colnames(Death_data)<-renameColumns(names_confirm_column,"Death")

#rename date column's name
names(Confirmed_data)[1]<-"Date"
names(Recovery_data)[1]<-"Date"
names(Death_data)[1]<-"Date"

#clean date column with appropriate dates
Date_update<-seq(as.Date("2020/01/22"), as.Date("2020/10/17"),by = "day")

Confirmed_data$Date<-Date_update
Recovery_data$Date<-Date_update
Death_data$Date<-Date_update

#combine datasets for confirm dataset, recover dataset and death datasets where countries as columns with corresponded word to differentiate them
Confirmed_Recovery_Data<-merge(Confirmed_data,Recovery_data, by = "Date", all = TRUE)
Covid_case_DF<-merge(Confirmed_Recovery_Data,Death_data, by = "Date", all = TRUE)

#change the layout of data in count governments'measures dataframe
Count_measure_DF<- Count_measure_DF %>% spread(key=COUNTRY,value=measures_number,fill=0)
colnames(Count_measure_DF)<-renameColumns(names(Count_measure_DF),"Measure_count")
names(Count_measure_DF)[1]<-"Date"


#combine Covid-19 cases' datasets with governments'measures number dataframe
Covid_case_DF_with_measure_count<-merge(Covid_case_DF,Count_measure_DF,by = "Date",all = TRUE)

#for combined dataframe, refill NA value as 0
Covid_case_DF_with_measure_count[is.na(Covid_case_DF_with_measure_count)] = 0

#save the new dataframe as a csv dataset for data visualizations
write_csv(Covid_case_DF_with_measure_count,file="Cleaned_Covid_19_cases_and_measures_count.csv", col_names=TRUE)