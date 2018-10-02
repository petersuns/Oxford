#Header_Sheet_Follow_Up.csv


library("dplyr")
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
FP<-read.csv("sheets/Header_Sheet_Follow_Up.csv",stringsAsFactors = FALSE)
FP<-arrange(FP, Trial_Entry_No)

# data cleaning 

# get headers' name
FP_clonames<-colnames(FP) 
#subset for machine read data only: row 1:794
FP<-FP[1:730,c('Trial_Entry_No','Case_Details')]

#############where the function starts
#data cleaning for entry error 
#1st check and delete duplicates
#
## check the duplicated rows
table(duplicated(FP)) # 
FP_duplicate<-as.data.frame(FP[which(duplicated(FP)),]) # 335, 430, 626
# number of duplicated rows
nrow(FP_duplicate)

# delete duplicated rows
FP<-FP[!duplicated(FP),]


#table(table(FP$Document_Name)>1)
#2nd check and delete more than two assessments 
# check duplicated patient numbers
table(sort(table(FP$Trial_Entry_No)))#P0335, P0430, P0614
# no more than 1 assessments. 


#
summary(FP)
str(FP)
sort(table(FP$Case_Details,exclude = NULL))

PPL_NOT_FollowUp<- c(FP$Trial_Entry_No[FP$Case_Details=='Deceased'],
                 FP$Trial_Entry_No[FP$Case_Details=='Withdrawal'],
                 FP$Trial_Entry_No[FP$Case_Details=='Lost to follow up'])


c(FP$Trial_Entry_No[FP$Case_Details=='Deceased'],FP$Trial_Entry_No[FP$Case_Details=='Withdrawal'])
# find doubled tested patients and sepreate first test and follow ups 
FP$Sheet_Name<-'FP'
FP_number_of_ppl<-FP[unique(FP$Trial_Entry_No) %in% FP$Trial_Entry_No,] # total number of people who did HIS_SS
nrow(FP_number_of_ppl)
ppl_with_FP_score<-FP$Trial_Entry_No
write.csv(FP, file = "FP_data_pb.csv")