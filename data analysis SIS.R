#Stroke_Impact_Scale.csv
library("dplyr")
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
SIS<-read.csv("sheets/Stroke_Impact_Scale.csv",stringsAsFactors = FALSE)
SIS<-arrange(SIS, Trial_Entry_No)

# data cleaning 

# get headers' name
SIS_clonames<-colnames(SIS) 
#subset for machine read data only: row 1:794
SIS$SIS_SCORE<-rowSums(SIS[,c("Q1a",           
                               "Q1b",            "Q1c",            "Q1d",            "Q2a",            "Q2b",            "Q2c",            "Q2d",           
                               "Q2e",            "Q2f",            "Q2g",            "Q3a",            "Q3b",            "Q3c" ,           "Q3d",           
                               "Q3e",            
                             # "Q3f",            
                              "Q3g",            
                              #"Q3h",            "Q3i",            
                              "Q4a",            "Q4b",           
                               "Q4c",            "Q4d",            "Q4e",            "Q4f",            "Q4g",            "Q5a",            "Q5b",           
                               "Q5c",            "Q5d",            "Q5e",            "Q5f",            "Q5g",            "Q5h",            "Q5i",           
                               "Q5j",            "Q6a",            "Q6b",            "Q6c",            "Q6d" ,           "Q6e",            "Q6f",           
                               "Q6g",            "Q6h",            "Q6i",            "Q7a",            "Q7b",            "Q7c",            "Q7d",           
                               "Q7e",            "Q8a",            "Q8b",            "Q8c",            "Q8d",            "Q8e",            "Q8f",           
                               "Q8g",            "Q8h")],na.rm=TRUE)
SIS<-SIS[1:498,c('Trial_Entry_No','SIS_SCORE')]

#############where the function starts
#data cleaning for entry error 
#1st check and delete duplicates
#
## check the duplicated rows
table(duplicated(SIS)) # 
SIS_duplicate<-as.data.frame(SIS[which(duplicated(SIS)),])
# number of duplicated rows
nrow(SIS_duplicate)

# delete duplicated rows
SIS<-SIS[!duplicated(SIS),]


#table(table(SIS$Document_Name)>1)
#2nd check and delete more than two assessments 
# check duplicated patient numbers
table(sort(table(SIS$Trial_Entry_No)))#P0335, P0430, P0614
lessthan2_ID<-rownames(data.frame(which(sort(table(SIS$Trial_Entry_No))<2)))
equal2_ID<-rownames(data.frame(which(sort(table(SIS$Trial_Entry_No))==2))) # Thanks god

# check the data the occurs three times
equal2_data<-SIS[SIS$Trial_Entry_No %in% equal2_ID,]
#SIS<-SIS[!SIS$Trial_Entry_No %in% morethan2rownames,]
SIS[SIS$Trial_Entry_No %in% equal2_ID,]
#no duplicated row now 
table(sort(table(SIS$Document_Name))) # five duplicate

# add new row and average the two
SIS$SIS_SCORE[SIS$Trial_Entry_No=='P0335']<-mean(SIS$SIS_SCORE[SIS$Trial_Entry_No=='P0335'])
# or drop the first one
SIS<-SIS[!duplicated(SIS),]
# no more than 1 assessments. 


#
summary(SIS)
str(SIS)
sort(table(SIS$SIS_SCORE,exclude = NULL))
sort(table(as.integer(as.character(SIS$SIS_SCORE)),exclude = NULL))
str(SIS)


# find doubled tested patients and sepreate first test and follow ups 
SIS$Sheet_Name<-'SIS'
SIS_number_of_ppl<-SIS[unique(SIS$Trial_Entry_No) %in% SIS$Trial_Entry_No,] # total number of people who did HIS_SS
nrow(SIS_number_of_ppl)
ppl_with_SIS_score<-SIS$Trial_Entry_No
write.csv(SIS, file = "SIS_data_pb.csv")