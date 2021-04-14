#install needed packages
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("tidyverse")
library("readxl")
#Data visualization
library(ggplot2)
library(tidyverse)

#First visualize each drug and its lifetime prevalence in each country

#function that cleans up the dataframe and returns it
clean_lp = function(filename){
  #reading excel file
  dataframe = filename
  dataframe <- read_excel(paste("data/",paste(filename,".xlsx",sep=""),sep=""))

  #Data cleaning
  library(dplyr)
  #removing rows 1 to 2 and 34 to 42 as they contain no info
  dataframe = anti_join(dataframe,dataframe[1:2,])
  dataframe = anti_join(dataframe,dataframe[32:42,])
  #removing last column 
  dataframe = dataframe %>% select(-(7))
  #renaming colums using first row
  colnames(dataframe) = dataframe[1,]
  #removing first row
  dataframe = anti_join(dataframe,dataframe[1,])
  #removing rows with NA
  #dataframe = na.omit((dataframe))
  return(dataframe)
}

# import every data set
cannabis_lp = clean_lp("cannabis_lp")
alc_lp = clean_lp("alc_lp")
amph_lp = clean_lp("amph_lp")
coc_lp = clean_lp("coc_lp")
ecstasy_lp = clean_lp("ecstasy_lp")
LSD_lp = clean_lp("LSD_lp")
tobacco_lp = clean_lp("tobacco_lp")

# Selecting only country and total drug use per drug
cannabis_total = cannabis_lp %>% select(Country,Total)
alc_total = alc_lp %>% select(Country,Total)
amph_total = amph_lp %>% select(Country,Total)
coc_total = coc_lp %>% select(Country,Total)
ecstasy_total = ecstasy_lp %>% select(Country,Total)
LSD_total = LSD_lp %>% select(Country,Total)
tobacco_total = tobacco_lp %>% select(Country,Total)

#Add a column indicating the drug type
cannabis_total$drug = "cannabis"
alc_total$drug = "alcohol"
amph_total$drug = "amphetamines"
coc_total$drug = "cocaine"
ecstasy_total$drug = "ecstasy"
LSD_total$drug = "LSD"
tobacco_total$drug = "tobacco"

#combine all the dataframes
combined_drugs = combine(cannabis_total,alc_total,amph_total,coc_total,ecstasy_total,LSD_total,tobacco_total)
combined_drugs[is.na(combined_drugs)] <- "0"
combined_drugs$Total = as.numeric(combined_drugs$Total)
#side by side bar plot
ggplot(combined_drugs) + 
  geom_bar(aes(fill=drug, y=Total, x=Country),position="dodge", stat="identity")+ggtitle("side by side barplot of lifetime prevalence of drug use")

#stacked barchart
ggplot(combined_drugs, aes(fill=drug, y=Total, x=Country)) + 
  geom_bar(position="stack", stat="identity")+ggtitle("side by side stacked barchart of lifetime prevalence of drug use")

#stacked barchart
ggplot(combined_drugs, aes(fill=drug, y=Total, x=Country)) + 
  geom_bar(position="fill", stat="identity")+ggtitle("percent stacked barchart of lifetime prevalence of drug use")

#side by side frequency plot for a few countries
ggplot(combined_drugs%>% filter(Country == "France"| Country == "Germany" | Country == "Italy" | Country == "Ireland"), aes(Country,Total,fill=drug)) + 
  geom_bar(position="fill", stat="identity")+ggtitle("comparison of lifetime prevalence of drug use in a few countries")+ scale_y_continuous()

#side by side bar plot for a few countries
ggplot(combined_drugs%>% filter(Country == "France"| Country == "Germany" | Country == "Italy" | Country == "Ireland"), aes(Country,Total,fill=drug)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("comparison of lifetime prevalence of drug use in a few countries")+ scale_y_continuous()
