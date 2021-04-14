#install needed packages
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("tidyverse")
library("readxl")
#Data visualization
library(ggplot2)
library(tidyverse)

## First visualize each drug and its lifetime prevalence in each country

#function that cleans up the dataframe and returns it
clean_lp = function(filename){
  #reading excel file
  dataframe = filename
  dataframe <- read_excel(paste("data/lp/",paste(filename,".xlsx",sep=""),sep=""))

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

# import and clean every data set
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
legal_drugs = combine(alc_total,tobacco_total)
illegal_drugs = combine(cannabis_total,amph_total,coc_total,ecstasy_total,LSD_total)

#setting the NA values to zero for graph visualization purposes
PrepareforGraph = function(dataframe){
  dataframe[is.na(dataframe)] <- "0"
  dataframe$Total = as.numeric(dataframe$Total)
  return(dataframe)
}

#Creating dataframes for all, legal, illegal drugs
combined_drugs_plot = PrepareforGraph(combined_drugs)
legal_drugs_plot = PrepareforGraph(legal_drugs)
illegal_drugs_plot = PrepareforGraph(illegal_drugs)

#side by side bar plot
ggplot(combined_drugs_plot) + 
  geom_bar(aes(fill=drug, y=Total, x=Country),position="dodge", stat="identity")+ggtitle("side by side barplot of lifetime prevalence of drug use")

## For every country
#legal drugs and cannabis
legal_can_lp = combined_drugs_plot %>%
  filter(drug == "alcohol" | drug == "tobacco" | drug == "cannabis" 
         & Country != "Denmark" & Country != "Luxembourg" & Country != "Slovenia" & Country != "Sweden" & Country != "United Kingdom" )

#%>%  summarize(mean_total = mean(Total, na.rm = TRUE))

#illegal drugs except cannabis
illegal_lp = combined_drugs_plot %>%
  filter(drug != "alcohol" & drug != "tobacco" & drug != "cannabis") %>%
  arrange(desc(Total))
 
#stacked barchart for legal and illegal

ggplot(legal_can_lp, aes(fill=drug, y=Total, x=Country)) + 
  geom_bar(position="stack", stat="identity")+ggtitle("stacked barchart of lifetime prevalence \nof legal and cannabis drug use")

ggplot(illegal_lp, aes(fill=drug, y=Total, x=Country)) + 
  geom_bar(position="stack", stat="identity")+ggtitle("stacked barchart of lifetime prevalence \n of illegal except cannabis drug use")


## For a limited number of countries for clarity
#all drugs
ggplot(combined_drugs_plot%>% filter(Country == "France"| Country == "Germany" | Country == "Italy" | Country == "Ireland"), aes(Country,Total,fill=drug)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("comparison of lifetime prevalence of drug use in a few countries")+ scale_y_continuous()

#legal drugs
ggplot(legal_drugs_plot%>% filter(Country == "France"| Country == "Germany" | Country == "Italy" | Country == "Ireland"), aes(Country,Total,fill=drug)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("comparison of lifetime prevalence of legal drug use in a few countries")+ scale_y_continuous()

#illegal drugs
ggplot(illegal_drugs_plot%>% filter(Country == "France"| Country == "Germany" | Country == "Italy" | Country == "Ireland"), aes(Country,Total,fill=drug)) + 
  geom_bar(position="dodge", stat="identity")+ggtitle("comparison of lifetime prevalence of illegal drug use in a few countries")+ scale_y_continuous()


#### Visualizing last year prevalence of drug use by age group for each drug 

# function for cleaning last year prevalance excel data
clean_lyp = function(filename){
  #reading excel file
  dataframe = filename
  dataframe <- read_excel(paste("data/lyp/",paste(filename,".xlsx",sep=""),sep=""))
  
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
  # select country and total percentage
  dataframe = dataframe %>% select(Country,Total)
  
  return(dataframe)
}

# import and clean every data set

age_group = c(20,30,40,50,60)
drug_group = c("alc","amph","can","coc","ecs","lsd","tob")

# read all datasets and give them a name corresponding to age and drug type
for (age in age_group){
  for (drug in drug_group){
    file_name <- paste(drug, toString(age), sep = "_")
    var_name <- paste(file_name, "lyp", sep = "_")
    assign(var_name, clean_lyp(file_name))
  }
}

# add an age column to each dataframe with corresponding age
# didn't find how to do it iteratively, sorry if you read it

#Cannabis
can_20_lyp$age = 20
can_30_lyp$age = 30
can_40_lyp$age = 40
can_50_lyp$age = 50
can_60_lyp$age = 60
can_lyp = combine(can_20_lyp,can_30_lyp,can_40_lyp,can_50_lyp,can_60_lyp)
#remove from workspace
rm(can_20_lyp)
rm(can_30_lyp)
rm(can_40_lyp)
rm(can_50_lyp)
rm(can_60_lyp)
can_lyp$drug = "Cannabis"

#Alcohol
alc_20_lyp$age = 20
alc_30_lyp$age = 30
alc_40_lyp$age = 40
alc_50_lyp$age = 50
alc_60_lyp$age = 60
alc_lyp = combine(alc_20_lyp,alc_30_lyp,alc_40_lyp,alc_50_lyp,alc_60_lyp)
#remove from workspace
rm(alc_20_lyp)
rm(alc_30_lyp)
rm(alc_40_lyp)
rm(alc_50_lyp)
rm(alc_60_lyp)
alc_lyp$drug = "Alcohol"

#Amphetamines
amph_20_lyp$age = 20
amph_30_lyp$age = 30
amph_40_lyp$age = 40
amph_50_lyp$age = 50
amph_60_lyp$age = 60
amph_lyp = combine(amph_20_lyp,amph_30_lyp,amph_40_lyp,amph_50_lyp,amph_60_lyp)
#remove from workspace
rm(amph_20_lyp)
rm(amph_30_lyp)
rm(amph_40_lyp)
rm(amph_50_lyp)
rm(amph_60_lyp)
amph_lyp$drug = "Amphetamines"

#Cocaine
coc_20_lyp$age = 20
coc_30_lyp$age = 30
coc_40_lyp$age = 40
coc_50_lyp$age = 50
coc_60_lyp$age = 60
coc_lyp = combine(coc_20_lyp,coc_30_lyp,coc_40_lyp,coc_50_lyp,coc_60_lyp)
#remove from workspace
rm(coc_20_lyp)
rm(coc_30_lyp)
rm(coc_40_lyp)
rm(coc_50_lyp)
rm(coc_60_lyp)
coc_lyp$drug = "Cocaine"

#Ecstasy
ecs_20_lyp$age = 20
ecs_30_lyp$age = 30
ecs_40_lyp$age = 40
ecs_50_lyp$age = 50
ecs_60_lyp$age = 60
ecs_lyp = combine(ecs_20_lyp,ecs_30_lyp,ecs_40_lyp,ecs_50_lyp,ecs_60_lyp)
#remove from workspace
rm(ecs_20_lyp)
rm(ecs_30_lyp)
rm(ecs_40_lyp)
rm(ecs_50_lyp)
rm(ecs_60_lyp)
ecs_lyp$drug = "Ecstasy"

#LSD
lsd_20_lyp$age = 20
lsd_30_lyp$age = 30
lsd_40_lyp$age = 40
lsd_50_lyp$age = 50
lsd_60_lyp$age = 60
lsd_lyp = combine(lsd_20_lyp,lsd_30_lyp,lsd_40_lyp,lsd_50_lyp,lsd_60_lyp)
#remove from workspace
rm(lsd_20_lyp)
rm(lsd_30_lyp)
rm(lsd_40_lyp)
rm(lsd_50_lyp)
rm(lsd_60_lyp)
lsd_lyp$drug = "LSD"

#Tobacco
tob_20_lyp$age = 20
tob_30_lyp$age = 30
tob_40_lyp$age = 40
tob_50_lyp$age = 50
tob_60_lyp$age = 60
tob_lyp = combine(tob_20_lyp,tob_30_lyp,tob_40_lyp,tob_50_lyp,tob_60_lyp)
#remove from workspace
rm(tob_20_lyp)
rm(tob_30_lyp)
rm(tob_40_lyp)
rm(tob_50_lyp)
rm(tob_60_lyp)
tob_lyp$drug = "Tobacco"

## now this annoying part is over, let's plot the last year prevalence of drug use
## by age 

#Compute mean value of Total for all the countries


#combine all the drugs data in a single dataframe
alldrugs_lyp = combine(can_lyp,alc_lyp,amph_lyp,coc_lyp,ecs_lyp,lsd_lyp,tob_lyp)

#setting the NA values to zero for graph visualization purposes
PrepareforGraph = function(dataframe){
  dataframe[is.na(dataframe)] <- "0"
  dataframe$Total = as.numeric(dataframe$Total)
  return(dataframe)
}
alldrugs_lyp_plot = PrepareforGraph(alldrugs_lyp)

#plotting the graph for different countries
ggplot(alldrugs_lyp_plot %>%filter(Country == "France"), aes(x=age, y=Total, colour=drug)) + geom_line()
ggplot(alldrugs_lyp_plot %>%filter(Country == "Germany"), aes(x=age, y=Total, colour=drug)) + geom_line()
ggplot(alldrugs_lyp_plot %>%filter(Country == "Italy"), aes(x=age, y=Total, colour=drug)) + geom_line()
ggplot(alldrugs_lyp_plot %>%filter(Country == "Ireland"), aes(x=age, y=Total, colour=drug)) + geom_line()

# for all countries

#Mean value for legal and illegal drugs

#legal drugs and cannabis
all_legal_drugs_lyp_mean = alldrugs_lyp_plot %>%
 filter(drug == "Alcohol" | drug == "Tobacco" | drug == "Cannabis") %>%
  group_by(drug,age) %>%
  summarize(mean_total = mean(Total, na.rm = TRUE))

ggplot(data=all_legal_drugs_lyp_mean, aes(x=age, y=mean_total, group=drug, colour=factor(drug))) + geom_line(size=1) + geom_point() + ggtitle("mean Drug use by age over all EU countries ")+ ylab("Mean drug use")

# illegal drugs except cannabis
all_ilegal_drugs_lyp_mean = alldrugs_lyp_plot %>%
  filter(drug != "Alcohol" & drug != "Tobacco" & drug != "Cannabis") %>%
  group_by(drug,age) %>%
  summarize(mean_total = mean(Total, na.rm = TRUE))

ggplot(data=all_ilegal_drugs_lyp_mean, aes(x=age, y=mean_total, group=drug, colour=factor(drug))) + geom_line(size=1) + geom_point() + ggtitle("mean Drug use by age over all EU countries ")+ ylab("Mean drug use")

