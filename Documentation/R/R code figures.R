#####################################################################################
######################### CHECK MODEL OUTPUT MAIN FIGURES
####################
### libraries needed
library(dplyr)
library(xlsx)
### the working directory to where your files are
setwd("C:/Users/ivanpg/OneDrive - NORCE/Models BEAST/Cohort_Component_Model_George_Changes 2020 09 18")
### load the file with observed UN values
DF_Obs<-read.xlsx("Observed_UN_values.xlsx",sheetIndex = 1,header = T)
### Omit first row, starting conditions...
DF_Obs<-DF_Obs[2:31,]

### not used anymore, this would have plot the counts of in/out migration per period
DF_Obs$Net_In_Mig_Counts<-round(DF_Obs$Net_In_Mig_Counts,0)
DF_Obs$Net_Out_Mig_Counts<-round(DF_Obs$Net_Out_Mig_Counts,0)
DF_Obs$Net_In_Mig_Mal_Counts<-round(DF_Obs$Net_In_Mig_Mal_Counts,0)
DF_Obs$Net_Out_Mig_Mal_Counts<-round(DF_Obs$Net_Out_Mig_Mal_Counts,0)
DF_Obs$Net_In_Mig_Fem_Counts<-round(DF_Obs$Net_In_Mig_Fem_Counts,0)
DF_Obs$Net_Out_Mig_Fem_Counts<-round(DF_Obs$Net_Out_Mig_Fem_Counts,0)


### READ THE OUTPUT FROM THE MODEL

# in case you wanna use a different path
##path<-c("C:/Users/ivanpg/OneDrive - Universitetet i Agder/Norway Models/Cohort_Component_Method_No_Migration/")
#DF<-read.csv(file = paste(path,"CRED_Annual_Stats.csv",sep = ""),header = T)

# otherwise file must be in same working directory
# CRED_Annual_Stats is not an informative name
# int is actually from a previous model so you may consider changing it
DF<-read.csv(file = "CRED_Annual_Stats.csv",header = T)


## you split the main dataframe, DF, into several ones to create different plots
## data frame with model outputs
DF_Plot1<-DF[,c("Period","Birth_Rate","Death_Rate","Growth_Rate")]
## data frame with observed UN values
DF_Obs_Plot1<-DF_Obs[,c("Period","Birth.rate","Death.rate","Growth.Rate")]
## names in both dataframes should be equal
names(DF_Obs_Plot1)<-names(DF_Plot1)

## data frame with model outputs
DF_Plot2<-DF[,c("Period","Pop_Age_Mean","Mal_Age_Mean","Fem_Age_Mean")]
## data frame with observed UN values
DF_Obs_Plot2<-DF_Obs[,c("Period","Mean_Pop","Mean_Mal","Mean_Fem")]
## names in both dataframes should be equal
names(DF_Obs_Plot2)<-names(DF_Plot2)

#### not used anymore, 
### These plots compared whether the input/output of immigrants in the model were the same as observed by UN
# DF_Plot3<-DF[,c("Period","Current_Period_Pop","Immigration_Counts","Emigration_Counts")]
# DF_Obs_Plot3<-DF_Obs[,c("Period","Population","Net_In_Mig_Counts","Net_Out_Mig_Counts" )]
# names(DF_Plot3)<-names(DF_Obs_Plot3)

## data frame with model outputs
DF_Plot3<-DF[,c("Period","Current_Period_Pop","Num_Births","Num_Deaths")]
## data frame with observed UN values
DF_Obs_Plot3<-DF_Obs[,c("Period","Population","Births","Deaths" )]
## names in both dataframes should be equal
names(DF_Plot3)<-names(DF_Obs_Plot3)

## data frame with model outputs
DF_Plot4<-DF[,c("Period","Pop_Age_SD", "Mal_Age_SD","Fem_Age_SD")]
## data frame with observed UN values
DF_Obs_Plot4<-DF_Obs[,c("Period","SD_Pop","SD_Mal","SD_Fem")]
## names in both dataframes should be equal
names(DF_Obs_Plot4)<-names(DF_Plot4)


### In order to plot the data in ggplot, the dataframes have to be reshaped
### we used reshape library
library(reshape)
library(ggplot2)

### we use the melt function to reshape all previou dataframes
DF_Plot1<-melt(DF_Plot1,id.vars = c("Period"),variable_name = "Measurement")
DF_Obs_Plot1<-melt(DF_Obs_Plot1,id.vars = c("Period"),variable_name = "Measurement")

DF_Plot2<-melt(DF_Plot2,id.vars = c("Period"),variable_name = "Measurement")
DF_Obs_Plot2<-melt(DF_Obs_Plot2,id.vars = c("Period"),variable_name = "Measurement")

DF_Plot3<-melt(DF_Plot3,id.vars = c("Period"),variable_name = "Measurement")
DF_Obs_Plot3<-melt(DF_Obs_Plot3,id.vars = c("Period"),variable_name = "Measurement")

DF_Plot4<-melt(DF_Plot4,id.vars = c("Period"),variable_name = "Measurement")
DF_Obs_Plot4<-melt(DF_Obs_Plot4,id.vars = c("Period"),variable_name = "Measurement")

### Once reshaped, we plot them, we create one plot per dataframe
### note that the geom_boxplot will take the data with the model outputs and geom_point the UN data

p1<-ggplot(DF_Plot1, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Measurement),scales = "free") +  geom_point(data = DF_Obs_Plot1, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
p2<-ggplot(DF_Plot2, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Measurement),scales = "free") +  geom_point(data = DF_Obs_Plot2, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
p3<-ggplot(DF_Plot3, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Measurement),scales = "free") +  geom_point(data = DF_Obs_Plot3, colour = "red", size = 2) +
    theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
p4<-ggplot(DF_Plot4, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Measurement),scales = "free") +  geom_point(data = DF_Obs_Plot4, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))

### save the plots in pdf format
### if not specify, the files will be saved in the working directory
ggsave("BR_DR_GR.pdf",p1,width = 19, height = 11, units = "in",dpi = 300)
ggsave("Age_Averages.pdf",p2,width = 19, height = 11, units = "in",dpi = 300)
ggsave("Pop_Births_Deaths.pdf",p3,width = 19, height = 11, units = "in",dpi = 300)
ggsave("Age_SD.pdf",p4,width = 19, height = 11, units = "in",dpi = 300)


### Extra plots to look at death data in detail, it creates a pdf with a plot per period. 
### The plot presents a boxplot of number of deaths (model output) and a red point for UN Data
DF_Deaths<-DF_Plot3[DF_Plot3$Measurement == "Deaths",]
DF_Obs_Deaths<-DF_Obs_Plot3[DF_Obs_Plot3$Measurement == "Deaths",]

p5<-ggplot(DF_Deaths, aes(x=" ",y=value)) + geom_boxplot() + facet_wrap(vars(Period),scales = "free") +  geom_point(data = DF_Obs_Deaths, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Deaths_By_Period.pdf",p5,width = 19, height = 11, units = "in",dpi = 300)

### Extra plots to look at birth data in detail, it creates a pdf with a plot per period. 
### The plot presents a boxplot of number of births (model output) and a red point for UN Data
DF_Births<-DF_Plot3[DF_Plot3$Measurement == "Births",]
DF_Obs_Births<-DF_Obs_Plot3[DF_Obs_Plot3$Measurement == "Births",]

p6<-ggplot(DF_Births, aes(x=" ",y=value)) + geom_boxplot() + facet_wrap(vars(Period),scales = "free") +  geom_point(data = DF_Obs_Births, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Births_By_Period.pdf",p6,width = 19, height = 11, units = "in",dpi = 300)

##########################################
######################################################################################################
### CHECK MODEL OUTPUTS IN DETAIL, FIGURES ###

### neeeded libraries
library(dplyr)
library(xlsx)

#### I used path here cause I kept these files in a different directory
path<-"C:/Users/ivanpg/OneDrive - Universitetet i Agder/Norway/PROJECTS/MRC Project/Demographical Data Norway/UN_Data_Norway/Norway/R reading files/"
##########################
### These are files from the UN tables in regards to age, fertility, mortality and population size
### These files you find in the folder R reading files, they contain the same information as the files in the UN data tables folder but in a simpler way
DF_Obs_Age_Pop<-read.xlsx(file = paste(path,"Pop_Age.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Age_Mal<-read.xlsx(file = paste(path,"Mal_Age.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Age_Fem<-read.xlsx(file = paste(path,"Fem_Age.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Fertility<-read.xlsx(file = paste(path,"Fertility.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Mort_Mal<-read.xlsx(file = paste(path,"Mal_Mort.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Mort_Fem<-read.xlsx(file = paste(path,"Fem_Mort.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Pop_By_Age_Fem<-read.xlsx(file = paste(path,"POPULATION_BY_AGE_FEMALE.xlsx",sep = ""),sheetIndex = 1,header = T)
DF_Obs_Pop_By_Age_Mal<-read.xlsx(file = paste(path,"POPULATION_BY_AGE_MALE.xlsx",sep = ""),sheetIndex = 1,header = T)

#### For some reason I kept the sex ration in the file "Observed UN values", so here I take the values from that file
### different directory, different path
path<-"C:/Users/ivanpg/OneDrive - NORCE/Models BEAST/Cohort_Component_Model_George_Changes 2020 09 18/"
DF_Obs_SexRatio<-read.xlsx(file = paste(path,"Observed_UN_values.xlsx",sep=""),sheetIndex = 1,header = T)
DF_Obs_SexRatio<-DF_Obs_SexRatio[c(2:31),c("Period","Sex_Ratio_At_Birth")]


### For each of these dataframes I first rearrange data and change names of variables so I can plot them
###############################
#### AGE ANALYSIS
#### WE START WITH THE AGE ANALYSIS, THE END PRODUCT IS A CUMULATIVE DISTRIBUTION OF THE POPULAITON (MALES/FEMALES) BY AGE
#### I DID THIS TO COMPARE THAT THE AGE DISTRIBUTIONS WERE SIMILAR IN THE MODEL AND UN; WHEN THEY ARE NOT THIS ALLOWS YOU TO SEE WHEN THEY START TO DIFFER

### I found it easier to change the labels here than in each single excel file
Period<-c("1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999","2000-2004","2005-2009","2010-2014","2015-2019","2020-2024",
          "2025-2029","2030-2034","2035-2039","2040-2044","2045-2049","2050-2054","2055-2059","2060-2064","2065-2069","2070-2074","2075-2079","2080-2084","2085-2089","2090-2094","2095-2099")

### selects the rows and columns needed from each file
DF_Obs_Age_Pop<-DF_Obs_Age_Pop[c(2:31),c(8:29)]
DF_Obs_Age_Mal<-DF_Obs_Age_Mal[c(2:31),c(8:29)]
DF_Obs_Age_Fem<-DF_Obs_Age_Fem[c(2:31),c(8:29)]
Period_DF<-data.frame(Period)
### creates the cumulative distribution by 10 years intervals
### and binds the period DF with correct names to the cumulative distribution
for(a in seq(3,21,by=2)){
  DF_Obs_Pop_Age<-cbind(Period_DF,( rowSums(DF_Obs_Age_Pop[,c(2:a)]) / rowSums(DF_Obs_Age_Pop[,c(2:22)]) ) )
  DF_Obs_Mal_Age<-cbind(Period_DF,( rowSums(DF_Obs_Age_Mal[,c(2:a)]) / rowSums(DF_Obs_Age_Mal[,c(2:22)]) ) )
  DF_Obs_Fem_Age<-cbind(Period_DF,( rowSums(DF_Obs_Age_Fem[,c(2:a)]) / rowSums(DF_Obs_Age_Fem[,c(2:22)]) ) )
}
## reset column names
colnames(DF_Obs_Pop_Age)<-c("Period","<10","<20","<30","<40","<50","<60","<70","<80","<90","<100")
colnames(DF_Obs_Mal_Age)<-c("Period","<10","<20","<30","<40","<50","<60","<70","<80","<90","<100")
colnames(DF_Obs_Fem_Age)<-c("Period","<10","<20","<30","<40","<50","<60","<70","<80","<90","<100")

### split period sequence in two datasets, otherwise plots will look clogged 
Period1<-c("1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999","2000-2004","2005-2009","2010-2014","2015-2019","2020-2024")
Period2<-c("2025-2029","2030-2034","2035-2039","2040-2044","2045-2049","2050-2054","2055-2059","2060-2064","2065-2069","2070-2074","2075-2079","2080-2084","2085-2089","2090-2094","2095-2099")
DF_Obs_Pop_Age_1<-DF_Obs_Pop_Age %>% filter (Period == Period1)
DF_Obs_Pop_Age_2<-DF_Obs_Pop_Age %>% filter (Period == Period2)

DF_Obs_Mal_Age_1<-DF_Obs_Mal_Age %>% filter (Period == Period1)
DF_Obs_Mal_Age_2<-DF_Obs_Mal_Age %>% filter (Period == Period2)

DF_Obs_Fem_Age_1<-DF_Obs_Fem_Age %>% filter (Period == Period1)
DF_Obs_Fem_Age_2<-DF_Obs_Fem_Age %>% filter (Period == Period2)


#### POP SIZE BY AGE AND GENDER
#### THE END PRODUCT ARE PLOTS WHERE YOU COMPARE THE POP SIZE (MALES/FEMALES) PER PERIOD AND AGE GROUP
Period<-c("1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999","2000-2004","2005-2009","2010-2014","2015-2019","2020-2024",
          "2025-2029","2030-2034","2035-2039","2040-2044","2045-2049","2050-2054","2055-2059","2060-2064","2065-2069","2070-2074","2075-2079","2080-2084","2085-2089","2090-2094","2095-2099")

### Select rows and colums needed from the table
DF_Obs_Pop_By_Age_Fem<-DF_Obs_Pop_By_Age_Fem[c(2:31),c(8:29)]
DF_Obs_Pop_By_Age_Mal<-DF_Obs_Pop_By_Age_Mal[c(2:31),c(8:29)]
### replace the first column of the dataframe with the period vector 
DF_Obs_Pop_By_Age_Fem[,1]<-Period
DF_Obs_Pop_By_Age_Mal[,1]<-Period

DF_Obs_Pop_Fem<-data.frame(Period=Period,Pop=rowSums(DF_Obs_Pop_By_Age_Fem[,c(2:22)]))
DF_Obs_Pop_Mal<-data.frame(Period=Period,Pop=rowSums(DF_Obs_Pop_By_Age_Mal[,c(2:22)]))

colnames(DF_Obs_Pop_By_Age_Fem)<-c("Period","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-100","100+")
colnames(DF_Obs_Pop_By_Age_Mal)<-c("Period","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-100","100+")

### split main data frames into smaller ones so they can be plotted
DF_Obs_Pop_By_Age_Fem_1<-DF_Obs_Pop_By_Age_Fem[,c("Period","0-4","5-9","10-14","15-19")]
DF_Obs_Pop_By_Age_Fem_2<-DF_Obs_Pop_By_Age_Fem[,c("Period","20-24","25-29","30-34","35-39")]
DF_Obs_Pop_By_Age_Fem_3<-DF_Obs_Pop_By_Age_Fem[,c("Period","40-44","45-49","50-54","55-59")]
DF_Obs_Pop_By_Age_Fem_4<-DF_Obs_Pop_By_Age_Fem[,c("Period","60-64","65-69","70-74","75-79")]
DF_Obs_Pop_By_Age_Fem_5<-DF_Obs_Pop_By_Age_Fem[,c("Period","80-84","85-89","90-95","95-100","100+")]

DF_Obs_Pop_By_Age_Mal_1<-DF_Obs_Pop_By_Age_Mal[,c("Period","0-4","5-9","10-14","15-19")]
DF_Obs_Pop_By_Age_Mal_2<-DF_Obs_Pop_By_Age_Mal[,c("Period","20-24","25-29","30-34","35-39")]
DF_Obs_Pop_By_Age_Mal_3<-DF_Obs_Pop_By_Age_Mal[,c("Period","40-44","45-49","50-54","55-59")]
DF_Obs_Pop_By_Age_Mal_4<-DF_Obs_Pop_By_Age_Mal[,c("Period","60-64","65-69","70-74","75-79")]
DF_Obs_Pop_By_Age_Mal_5<-DF_Obs_Pop_By_Age_Mal[,c("Period","80-84","85-89","90-95","95-100","100+")]


###############
#### FERTILITY
#### THE END PRODUCT ARE BOXPLOTS WITH THE PROBABILITY OF A FEMALE GIVING BIRTH IN THE MODEL ACCORDING TO AGE GROUP AND PERIOD
#### THESE VALUES ARE COMPARED TO THE UN FERTILITY VALUES.
#### THESE ARE NOT SO USEFUL PLOTS IN MY VIEW, BUT I LEAVE THE CODE....

DF_temp<-DF_Obs_Fertility[,c(8:15)]
DF_temp[,c(2:8)]<-DF_temp[,c(2:8)] / 1000
DF_temp$Period<-Period
DF_Fertility_Obs<-data.frame(DF_temp$Period,DF_temp$X15.19,DF_temp$X15.19,DF_temp$X15.19,DF_temp$X15.19,DF_temp$X15.19,
                        DF_temp$X20.24,DF_temp$X20.24,DF_temp$X20.24,DF_temp$X20.24,DF_temp$X20.24,
                        DF_temp$X25.29,DF_temp$X25.29,DF_temp$X25.29,DF_temp$X25.29,DF_temp$X25.29,
                        DF_temp$X30.34,DF_temp$X30.34,DF_temp$X30.34,DF_temp$X30.34,DF_temp$X30.34,
                        DF_temp$X35.39,DF_temp$X35.39,DF_temp$X35.39,DF_temp$X35.39,DF_temp$X35.39,
                        DF_temp$X40.44,DF_temp$X40.44,DF_temp$X40.44,DF_temp$X40.44,DF_temp$X40.44,
                        DF_temp$X45.49,DF_temp$X45.49,DF_temp$X45.49,DF_temp$X45.49,DF_temp$X45.49)
colnames(DF_Fertility_Obs)<-c("Period",as.character(seq(15,49,by=1)))
DF_Fertility_Obs<-melt(DF_Fertility_Obs,id.vars = c("Period"),variable_name = "Measurement")

DF_Fertility_Obs_1<-DF_Fertility_Obs[DF_Fertility_Obs$Measurement ==15,]
DF_Fertility_Obs_2<-DF_Fertility_Obs[DF_Fertility_Obs$Measurement ==20,]
CAT1<-c(16:19,40:49)
CAT2<-c(21:39)
for ( a in 1:length(CAT1)){
  temp<-DF_Fertility_Obs[DF_Fertility_Obs$Measurement== CAT1[a],]
  DF_Fertility_Obs_1<-rbind(DF_Fertility_Obs_1,temp)
}
for ( a in 1:length(CAT2)){
  temp<-DF_Fertility_Obs[DF_Fertility_Obs$Measurement== CAT2[a],]
  DF_Fertility_Obs_2<-rbind(DF_Fertility_Obs_2,temp)
}


###############
#### MORTALITY
#### THE END PRODUCT ARE BOXPLOTS WITH THE PROBABILITY OF AN AGENT DYING ACCORDING TO GENDER, AGE GROUP AND PERIOD
#### THESE VALUES ARE COMPARED TO THE UN INPUT VALUES.
#### AGAIN, THESE ARE NOT SO USEFUL PLOTS IN MY VIEW, BUT I LEAVE THE CODE....

### PLEASE NOTE THAT THESE TABLES ARE CUSTOMIZED FOR THE SURVIVAL RATIO, SO THEY DIFFER WITH THE MAIN UN TABLES IN THE FIRST TWO AGE GROUPS
### THE 0-05 GROUP AND THE 0-4. THIS IS NOT EVIDENT IN THE TABLE! THE AGE GROUP NAMES WERE KEPT THE SAME AND THUS MAY BE CONFUSING!
### BUT THE VALUES ARE INDEED DIFFERENT FROM THE ORIGINAL UN TABLES!

### ALSO WHEN USING THE CENTRAL DEATH RATE, THE EXCEL TABLES MAY HAVE TO BE REDONE
### THIS IS BECAUSE THE CDR TABLE USED IN THE MODEL HAVE ONE EXTRA AGE GROUP, THE 0-0.5 GROUP
### AND THE 0-1 AND 0-4 GROUP IS NOT COLLAPSED, AS IT IS IN THE SURVIVAL RATIO TABLES!

### MALES
DF_temp<-DF_Obs_Mort_Mal[,c(8,21)]
DF_temp$Measurement<-rep(c("0-0.5","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-99","100-104","105"),30)
DF_temp$Period<-rep(Period,each=23)
colnames(DF_temp)<-c("Period","value","Measurement")

### I split the age groups because the probabilities may be quite different and putting them in one plot does not allow to see differences well
### so I group age categories that have similar probability values.

Prob_Death_Mal_1_Obs<-DF_temp[DF_temp$Measurement =="0-0.5",]

Prob_Death_Mal_2_Obs<-DF_temp[DF_temp$Measurement =="0-4",]
CAT<-c("5-9","10-14")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Mal_2_Obs<-rbind(Prob_Death_Mal_2_Obs,temp)
}

Prob_Death_Mal_3_Obs<-DF_temp[DF_temp$Measurement =="15-19",]
CAT<-c("20-24","25-29","30-34","35-39")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Mal_3_Obs<-rbind(Prob_Death_Mal_3_Obs,temp)
}

Prob_Death_Mal_4_Obs<-DF_temp[DF_temp$Measurement =="40-44",]
CAT<-c("45-49","50-54","55-59")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Mal_4_Obs<-rbind(Prob_Death_Mal_4_Obs,temp)
}

Prob_Death_Mal_5_Obs<-DF_temp[DF_temp$Measurement =="60-64",]
CAT<-c("65-69","70-74","75-79","80-84","85-89")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Mal_5_Obs<-rbind(Prob_Death_Mal_5_Obs,temp)
}

Prob_Death_Mal_6_Obs<-DF_temp[DF_temp$Measurement =="90-95",]
CAT<-c("95-99","100-104")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Mal_6_Obs<-rbind(Prob_Death_Mal_6_Obs,temp)
}

Prob_Death_Mal_7_Obs<-DF_temp[DF_temp$Measurement =="105",]



### FEMALES
DF_temp<-DF_Obs_Mort_Fem[,c(8,21)]
DF_temp$Measurement<-rep(c("0-0.5","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-99","100-104","105"),30)
DF_temp$Period<-rep(Period,each=23)
colnames(DF_temp)<-c("Period","value","Measurement")

### I split the age groups because the probabilities may be quite different and putting them in one plot does not allow to see differences well
### so I group age categories that have similar probability values.

Prob_Death_Fem_1_Obs<-DF_temp[DF_temp$Measurement =="0-0.5",]

Prob_Death_Fem_2_Obs<-DF_temp[DF_temp$Measurement =="0-4",]
CAT<-c("5-9","10-14")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Fem_2_Obs<-rbind(Prob_Death_Fem_2_Obs,temp)
}

Prob_Death_Fem_3_Obs<-DF_temp[DF_temp$Measurement =="15-19",]
CAT<-c("20-24","25-29","30-34","35-39")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Fem_3_Obs<-rbind(Prob_Death_Fem_3_Obs,temp)
}

Prob_Death_Fem_4_Obs<-DF_temp[DF_temp$Measurement =="40-44",]
CAT<-c("45-49","50-54","55-59")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Fem_4_Obs<-rbind(Prob_Death_Fem_4_Obs,temp)
}

Prob_Death_Fem_5_Obs<-DF_temp[DF_temp$Measurement =="60-64",]
CAT<-c("65-69","70-74","75-79","80-84","85-89")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Fem_5_Obs<-rbind(Prob_Death_Fem_5_Obs,temp)
}

Prob_Death_Fem_6_Obs<-DF_temp[DF_temp$Measurement =="90-95",]
CAT<-c("95-99","100-104")
for ( a in 1:length(CAT)){
  category = CAT[a]
  temp<-DF_temp[DF_temp$Measurement== category,]
  Prob_Death_Fem_6_Obs<-rbind(Prob_Death_Fem_6_Obs,temp)
}

Prob_Death_Fem_7_Obs<-DF_temp[DF_temp$Measurement =="105",]


################################################################
###### MODEL DATA
DF<-read.csv(file = "CRED_Annual_Stats.csv",header = T)
DF_SexRatio<-DF[,c("Period","Sex_Ratio_At_Birth")]

#### AGE
CDF_Pop<-DF[,c(2,30:39)]
CDF_Fem<-DF[,c(2,40:49)]
CDF_Mal<-DF[,c(2,50:59)]
colnames(CDF_Pop)<-c("Period","<10","<20","<30","<40","<50","<60","<70","<80","<90","<100")
colnames(CDF_Fem)<-c("Period","<10","<20","<30","<40","<50","<60","<70","<80","<90","<100")
colnames(CDF_Mal)<-c("Period","<10","<20","<30","<40","<50","<60","<70","<80","<90","<100")


CAT1<-c("1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989","1990-1994","1995-1999","2000-2004","2005-2009","2010-2014","2015-2019","2020-2024")
CAT2<-c("2030-2034","2035-2039","2040-2044","2045-2049","2050-2054","2055-2059","2060-2064","2065-2069","2070-2074","2075-2079","2080-2084","2085-2089","2090-2094","2095-2099")

CDF_Pop_1<-CDF_Pop[CDF_Pop$Period =="1950-1954",]
CDF_Mal_1<-CDF_Mal[CDF_Mal$Period =="1950-1954",]
CDF_Fem_1<-CDF_Fem[CDF_Fem$Period =="1950-1954",]
for ( a in 1:length(CAT1)){
  category = CAT1[a]
  
  temp<-CDF_Pop[CDF_Pop$Period== category,]
  CDF_Pop_1<-rbind(CDF_Pop_1,temp); remove(temp)
  
  temp<-CDF_Mal[CDF_Mal$Period== category,]
  CDF_Mal_1<-rbind(CDF_Mal_1,temp); remove(temp)
  
  temp<-CDF_Fem[CDF_Fem$Period== category,]
  CDF_Fem_1<-rbind(CDF_Fem_1,temp); remove(temp)
}


CDF_Pop_2<-CDF_Pop[CDF_Pop$Period =="2025-2029",]
CDF_Mal_2<-CDF_Mal[CDF_Mal$Period =="2025-2029",]
CDF_Fem_2<-CDF_Fem[CDF_Fem$Period =="2025-2029",]
for ( a in 1:length(CAT2)){
  category = CAT2[a]
  
  temp<-CDF_Pop[CDF_Pop$Period== category,]
  CDF_Pop_2<-rbind(CDF_Pop_2,temp); remove(temp)
  
  temp<-CDF_Mal[CDF_Mal$Period== category,]
  CDF_Mal_2<-rbind(CDF_Mal_2,temp); remove(temp)
  
  temp<-CDF_Fem[CDF_Fem$Period== category,]
  CDF_Fem_2<-rbind(CDF_Fem_2,temp); remove(temp)
}

### POP BY AGE AND GENDER
DF_Pop_By_Age_Mal<-DF[,c(2,143:163)]
DF_Pop_By_Age_Fem<-DF[,c(2,164:184)]
colnames(DF_Pop_By_Age_Mal)<-c("Period","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-100","100+")
colnames(DF_Pop_By_Age_Fem)<-c("Period","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-100","100+")

DF_Pop_Fem<-data.frame(Period=DF_Pop_By_Age_Fem$Period,Pop=rowSums(DF_Pop_By_Age_Fem[,c(2:22)]))
DF_Pop_Mal<-data.frame(Period=DF_Pop_By_Age_Mal$Period,Pop=rowSums(DF_Pop_By_Age_Mal[,c(2:22)]))
levels(DF_Pop_Fem$Period)<-Period
levels(DF_Pop_Mal$Period)<-Period

DF_Pop_Fem$Gender<-rep("Female",dim(DF_Pop_Fem)[1])
DF_Pop_Mal$Gender<-rep("Male",dim(DF_Pop_Mal)[1])
DF_Obs_Pop_Fem$Gender<-rep("Female",dim(DF_Obs_Pop_Fem)[1])
DF_Obs_Pop_Mal$Gender<-rep("Male",dim(DF_Obs_Pop_Mal)[1])

DF_Obs_Pop_Gender<-rbind(DF_Obs_Pop_Fem,DF_Obs_Pop_Mal)
DF_Pop_Gender<-rbind(DF_Pop_Fem,DF_Pop_Mal)

pextra<-ggplot(DF_Pop_Gender, aes(x=Period, y=Pop)) + geom_boxplot() +  geom_point(data = DF_Obs_Pop_Gender, colour = "red", size = 2) +
  facet_grid(rows=vars(Gender),scales = "free")  +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Tot_Pop_By_Gender.pdf",pextra,width = 19, height = 11, units = "in",dpi = 300)


DF_Pop_By_Age_Fem_1<-DF_Pop_By_Age_Fem[,c("Period","0-4","5-9","10-14","15-19")]
DF_Pop_By_Age_Fem_2<-DF_Pop_By_Age_Fem[,c("Period","20-24","25-29","30-34","35-39")]
DF_Pop_By_Age_Fem_3<-DF_Pop_By_Age_Fem[,c("Period","40-44","45-49","50-54","55-59")]
DF_Pop_By_Age_Fem_4<-DF_Pop_By_Age_Fem[,c("Period","60-64","65-69","70-74","75-79")]
DF_Pop_By_Age_Fem_5<-DF_Pop_By_Age_Fem[,c("Period","80-84","85-89","90-95","95-100","100+")]

DF_Pop_By_Age_Mal_1<-DF_Pop_By_Age_Mal[,c("Period","0-4","5-9","10-14","15-19")]
DF_Pop_By_Age_Mal_2<-DF_Pop_By_Age_Mal[,c("Period","20-24","25-29","30-34","35-39")]
DF_Pop_By_Age_Mal_3<-DF_Pop_By_Age_Mal[,c("Period","40-44","45-49","50-54","55-59")]
DF_Pop_By_Age_Mal_4<-DF_Pop_By_Age_Mal[,c("Period","60-64","65-69","70-74","75-79")]
DF_Pop_By_Age_Mal_5<-DF_Pop_By_Age_Mal[,c("Period","80-84","85-89","90-95","95-100","100+")]



### FERTILITY
Prob_Birth<-DF[,c(2,60:94)]
colnames(Prob_Birth)<-c("Period",as.character(seq(15,49,by=1)))
DF_Fertility<-melt(Prob_Birth,id.vars = c("Period"),variable_name = "Measurement")

DF_Fertility_1<-DF_Fertility[DF_Fertility$Measurement ==15,]
DF_Fertility_2<-DF_Fertility[DF_Fertility$Measurement ==20,]
CAT1<-c(16:19,40:49); CAT2<-c(21:39)

for ( a in 1:length(CAT1)){
  temp<-DF_Fertility[DF_Fertility$Measurement== CAT1[a],]
  DF_Fertility_1<-rbind(DF_Fertility_1,temp)
}
for ( a in 1:length(CAT2)){
  temp<-DF_Fertility[DF_Fertility$Measurement== CAT2[a],]
  DF_Fertility_2<-rbind(DF_Fertility_2,temp)
}

#### MORTALITY
Prob_Death_Fem<-DF[,c(2,95:117)]
Prob_Death_Mal<-DF[,c(2,118:140)]
colnames(Prob_Death_Mal)<-c("Period","0-0.5","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-99","100-104","105")
colnames(Prob_Death_Fem)<-c("Period","0-0.5","0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-95","95-99","100-104","105")
Prob_Death_Mal_1<-Prob_Death_Mal[,c("Period","0-0.5")]
Prob_Death_Mal_2<-Prob_Death_Mal[,c("Period","0-4","5-9","10-14")]
Prob_Death_Mal_3<-Prob_Death_Mal[,c("Period","15-19","20-24","25-29","30-34","35-39")]
Prob_Death_Mal_4<-Prob_Death_Mal[,c("Period","40-44","45-49","50-54","55-59")]
Prob_Death_Mal_5<-Prob_Death_Mal[,c("Period","60-64","65-69","70-74","75-79","80-84","85-89")]
Prob_Death_Mal_6<-Prob_Death_Mal[,c("Period","90-95","95-99","100-104")]
Prob_Death_Mal_7<-Prob_Death_Mal[,c("Period","105")]

Prob_Death_Fem_1<-Prob_Death_Fem[,c("Period","0-0.5")]
Prob_Death_Fem_2<-Prob_Death_Fem[,c("Period","0-4","5-9","10-14")]
Prob_Death_Fem_3<-Prob_Death_Fem[,c("Period","15-19","20-24","25-29","30-34","35-39")]
Prob_Death_Fem_4<-Prob_Death_Fem[,c("Period","40-44","45-49","50-54","55-59")]
Prob_Death_Fem_5<-Prob_Death_Fem[,c("Period","60-64","65-69","70-74","75-79","80-84","85-89")]
Prob_Death_Fem_6<-Prob_Death_Fem[,c("Period","90-95","95-99","100-104")]
Prob_Death_Fem_7<-Prob_Death_Fem[,c("Period","105")]


### RESHAPE DATA FRAMES
library(reshape)
library(ggplot2)

### CDF age
DF_CDF_Pop_1<-melt(CDF_Pop_1,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Pop_Obs_1<-melt(DF_Obs_Pop_Age_1,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Pop_2<-melt(CDF_Pop_2,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Pop_Obs_2<-melt(DF_Obs_Pop_Age_2,id.vars = c("Period"),variable_name = "Measurement")

DF_CDF_Mal_1<-melt(CDF_Mal_1,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Mal_Obs_1<-melt(DF_Obs_Mal_Age_1,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Mal_2<-melt(CDF_Mal_2,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Mal_Obs_2<-melt(DF_Obs_Mal_Age_2,id.vars = c("Period"),variable_name = "Measurement")

DF_CDF_Fem_1<-melt(CDF_Fem_1,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Fem_Obs_1<-melt(DF_Obs_Fem_Age_1,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Fem_2<-melt(CDF_Fem_2,id.vars = c("Period"),variable_name = "Measurement")
DF_CDF_Fem_Obs_2<-melt(DF_Obs_Fem_Age_2,id.vars = c("Period"),variable_name = "Measurement")

### Pop by age and gender
DF_Obs_Pop_By_Age_Fem1<-melt(DF_Obs_Pop_By_Age_Fem_1,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Fem2<-melt(DF_Obs_Pop_By_Age_Fem_2,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Fem3<-melt(DF_Obs_Pop_By_Age_Fem_3,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Fem4<-melt(DF_Obs_Pop_By_Age_Fem_4,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Fem5<-melt(DF_Obs_Pop_By_Age_Fem_5,id.vars = c("Period"),variable_name = "Cohort")

DF_Obs_Pop_By_Age_Mal1<-melt(DF_Obs_Pop_By_Age_Mal_1,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Mal2<-melt(DF_Obs_Pop_By_Age_Mal_2,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Mal3<-melt(DF_Obs_Pop_By_Age_Mal_3,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Mal4<-melt(DF_Obs_Pop_By_Age_Mal_4,id.vars = c("Period"),variable_name = "Cohort")
DF_Obs_Pop_By_Age_Mal5<-melt(DF_Obs_Pop_By_Age_Mal_5,id.vars = c("Period"),variable_name = "Cohort")

DF_Pop_By_Age_Fem1<-melt(DF_Pop_By_Age_Fem_1,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Fem2<-melt(DF_Pop_By_Age_Fem_2,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Fem3<-melt(DF_Pop_By_Age_Fem_3,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Fem4<-melt(DF_Pop_By_Age_Fem_4,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Fem5<-melt(DF_Pop_By_Age_Fem_5,id.vars = c("Period"),variable_name = "Cohort")

DF_Pop_By_Age_Mal1<-melt(DF_Pop_By_Age_Mal_1,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Mal2<-melt(DF_Pop_By_Age_Mal_2,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Mal3<-melt(DF_Pop_By_Age_Mal_3,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Mal4<-melt(DF_Pop_By_Age_Mal_4,id.vars = c("Period"),variable_name = "Cohort")
DF_Pop_By_Age_Mal5<-melt(DF_Pop_By_Age_Mal_5,id.vars = c("Period"),variable_name = "Cohort")


#### mortality
DF_Mort_Mal_1<-melt(Prob_Death_Mal_1,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Mal_2<-melt(Prob_Death_Mal_2,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Mal_3<-melt(Prob_Death_Mal_3,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Mal_4<-melt(Prob_Death_Mal_4,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Mal_5<-melt(Prob_Death_Mal_5,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Mal_6<-melt(Prob_Death_Mal_6,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Mal_7<-melt(Prob_Death_Mal_7,id.vars = c("Period"),variable_name = "Measurement")

DF_Mort_Fem_1<-melt(Prob_Death_Fem_1,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Fem_2<-melt(Prob_Death_Fem_2,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Fem_3<-melt(Prob_Death_Fem_3,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Fem_4<-melt(Prob_Death_Fem_4,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Fem_5<-melt(Prob_Death_Fem_5,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Fem_6<-melt(Prob_Death_Fem_6,id.vars = c("Period"),variable_name = "Measurement")
DF_Mort_Fem_7<-melt(Prob_Death_Fem_7,id.vars = c("Period"),variable_name = "Measurement")

                         #### PLOTS ###

#### SEX RATIO
p1<-ggplot(DF_SexRatio, aes(x=Period, y=Sex_Ratio_At_Birth)) + geom_boxplot() +  geom_point(data = DF_Obs_SexRatio, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Sex_Ratio_At_Birth.pdf",p1,width = 19, height = 11, units = "in",dpi = 300)


### AGE
p2<-ggplot(DF_CDF_Pop_1, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,ncol=8,scales = "free") +  geom_point(data = DF_CDF_Pop_Obs_1, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2))) + scale_y_continuous(breaks = seq(0.05,1,by=0.05))
ggsave("Pop_CDF_Age_Periods_1-15.pdf",p2,width = 22, height = 15, units = "in",dpi = 300)
p3<-ggplot(DF_CDF_Pop_2, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,ncol=8,scales = "free") +  geom_point(data = DF_CDF_Pop_Obs_2, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2))) + scale_y_continuous(breaks = seq(0.05,1,by=0.05))
ggsave("Pop_CDF_Age_Periods_16-30.pdf",p3,width = 22, height = 15, units = "in",dpi = 300)

p4<-ggplot(DF_CDF_Mal_1, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,ncol=8,scales = "free") +  geom_point(data = DF_CDF_Mal_Obs_1, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2))) + scale_y_continuous(breaks = seq(0.05,1,by=0.05))
ggsave("Mal_CDF_Age_Periods_1-15.pdf",p4,width = 22, height = 15, units = "in",dpi = 300)
p5<-ggplot(DF_CDF_Mal_2, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,ncol=8,scales = "free") +  geom_point(data = DF_CDF_Mal_Obs_2, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2))) + scale_y_continuous(breaks = seq(0.05,1,by=0.05))
ggsave("Mal_CDF_Age_Periods_16-30.pdf",p5,width = 22, height = 15, units = "in",dpi = 300)

p6<-ggplot(DF_CDF_Fem_1, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,ncol=8,scales = "free") +  geom_point(data = DF_CDF_Fem_Obs_1, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2))) + scale_y_continuous(breaks = seq(0.05,1,by=0.05))
ggsave("Fem_CDF_Age_Periods_1-15.pdf",p6,width = 22, height = 15, units = "in",dpi = 300)
p7<-ggplot(DF_CDF_Fem_2, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,ncol=8,scales = "free") +  geom_point(data = DF_CDF_Fem_Obs_2, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2))) + scale_y_continuous(breaks = seq(0.05,1,by=0.05))
ggsave("Fem_CDF_Age_Periods_16-30.pdf",p7,width = 22, height = 15, units = "in",dpi = 300)

### FERTILITY
p8<-ggplot(DF_Fertility_1, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = DF_Fertility_Obs_1, colour = "red", size = 2) +
theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Giving_Birth_Ages_15-19_40-49.pdf",p8,width = 25, height = 11, units = "in",dpi = 300)

p9<-ggplot(DF_Fertility_2, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = DF_Fertility_Obs_2, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Giving_Birth_20-39.pdf",p9,width = 25, height = 11, units = "in",dpi = 300)

### MORTALITY
p10<-ggplot(DF_Mort_Mal_1, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_1_Obs, colour = "red", size = 2) +
theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_0-0.5.pdf",p10,width = 19, height = 11, units = "in",dpi = 300)

p11<-ggplot(DF_Mort_Mal_2, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_2_Obs, colour = "red", size = 2) +
theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_0-14.pdf",p11,width = 19, height = 11, units = "in",dpi = 300)

p12<-ggplot(DF_Mort_Mal_3, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_3_Obs, colour = "red", size = 2) +
theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_15-39.pdf",p12,width = 19, height = 11, units = "in",dpi = 300)

p13<-ggplot(DF_Mort_Mal_4, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_4_Obs, colour = "red", size = 2) +
theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_40-59.pdf",p13,width = 19, height = 11, units = "in",dpi = 300)

p14<-ggplot(DF_Mort_Mal_5, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_5_Obs, colour = "red", size = 2) +
theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_60-89.pdf",p14,width = 19, height = 11, units = "in",dpi = 300)

p15<-ggplot(DF_Mort_Mal_6, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_6_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_90-104.pdf",p15,width = 19, height = 11, units = "in",dpi = 300)

p16<-ggplot(DF_Mort_Mal_7, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Mal_7_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Male_Age_105.pdf",p16,width = 19, height = 11, units = "in",dpi = 300)


p17<-ggplot(DF_Mort_Fem_1, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_1_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_0-0.5.pdf",p17,width = 19, height = 11, units = "in",dpi = 300)

p18<-ggplot(DF_Mort_Fem_2, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_2_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_0-14.pdf",p18,width = 19, height = 11, units = "in",dpi = 300)

p19<-ggplot(DF_Mort_Fem_3, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_3_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_15-39.pdf",p19,width = 19, height = 11, units = "in",dpi = 300)

p20<-ggplot(DF_Mort_Fem_4, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_4_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_40-59.pdf",p20,width = 19, height = 11, units = "in",dpi = 300)

p21<-ggplot(DF_Mort_Fem_5, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_5_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_60-89.pdf",p21,width = 19, height = 11, units = "in",dpi = 300)

p22<-ggplot(DF_Mort_Fem_6, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_6_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_90-104.pdf",p22,width = 19, height = 11, units = "in",dpi = 300)

p23<-ggplot(DF_Mort_Fem_7, aes(x=Measurement, y=value)) + geom_boxplot() + facet_wrap(~Period,scales = "free") +  geom_point(data = Prob_Death_Fem_7_Obs, colour = "red", size = 2) +
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Prob_Dying_Fem_Age_105.pdf",p23,width = 19, height = 11, units = "in",dpi = 300)


### POP BY AGE AND GENDER
p24<-ggplot(DF_Pop_By_Age_Fem1, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Fem1, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Fem_Age_0-19.pdf",p24,width = 19, height = 11, units = "in",dpi = 300)
p25<-ggplot(DF_Pop_By_Age_Fem2, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Fem2, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Fem_Age_20-39.pdf",p25,width = 19, height = 11, units = "in",dpi = 300)
p26<-ggplot(DF_Pop_By_Age_Fem3, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Fem3, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Fem_Age_40-59.pdf",p26,width = 19, height = 11, units = "in",dpi = 300)
p27<-ggplot(DF_Pop_By_Age_Fem4, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Fem4, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Fem_Age_60-79.pdf",p27,width = 19, height = 11, units = "in",dpi = 300)
p28<-ggplot(DF_Pop_By_Age_Fem5, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Fem5, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Fem_Age_80-100+.pdf",p28,width = 19, height = 11, units = "in",dpi = 300)

p29<-ggplot(DF_Pop_By_Age_Mal1, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Mal1, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Mal_Age_0-19.pdf",p29,width = 19, height = 11, units = "in",dpi = 300)
p30<-ggplot(DF_Pop_By_Age_Mal2, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Mal2, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Mal_Age_20-39.pdf",p30,width = 19, height = 11, units = "in",dpi = 300)
p31<-ggplot(DF_Pop_By_Age_Mal3, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Mal3, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Mal_Age_40-59.pdf",p31,width = 19, height = 11, units = "in",dpi = 300)
p32<-ggplot(DF_Pop_By_Age_Mal4, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Mal4, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Mal_Age_60-79.pdf",p32,width = 19, height = 11, units = "in",dpi = 300)
p33<-ggplot(DF_Pop_By_Age_Mal5, aes(x=Period, y=value)) + geom_boxplot() + facet_grid(rows=vars(Cohort),scales = "free") +  geom_point(data = DF_Obs_Pop_By_Age_Mal5, colour = "red", size = 2) + 
  theme(strip.text.y = element_text(size = 20)) + theme(axis.text.x=element_text(size=rel(2), angle=60, vjust = 0.5)) +theme(axis.text.y=element_text(size=rel(2)))
ggsave("Pop_Mal_Age_80-100+.pdf",p33,width = 19, height = 11, units = "in",dpi = 300)