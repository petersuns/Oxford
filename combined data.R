#patients.csv

library("dplyr")

setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
patients<-read.csv('sheets/patients.csv', sep = ';',stringsAsFactors = FALSE)
patients$Trial_Entry_No<-sprintf("P%04d", patients$id)
patients$OCS_MOCA[patients$arm==0]<-'OCS'
patients$OCS_MOCA[patients$arm==1]<-'MOCA'
OCS_ppl<-patients$Trial_Entry_No[patients$OCS_MOCA=='OCS']
MOCA_ppl<-patients$Trial_Entry_No[patients$OCS_MOCA=='MOCA']

ppl<-patients[,c('Trial_Entry_No','OCS_MOCA')]

#merge
#data <- merge(x=patients, y=NIH_SS_First,suffixes = c('.OCS',".NIH"), by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=ppl, y=NIH_SS_First, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=ocs_First, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=MOCA_First, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=NIH_SS_FP, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=ocs_FP, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=MOCA_FP, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=SIS, by=c("Trial_Entry_No"),all=TRUE)
data<-data[c('Trial_Entry_No','OCS_MOCA','Date_of_Exam.x','Score_1.x','Date.x',
             'impairment_number.x','Capture_ChoiceList_Total.x','Date_of_Exam.y','Score_1.y','Date.y',
             'impairment_number.y','Capture_ChoiceList_Total.y','Capture_ChoiceList_Total.y','SIS_SCORE')]
str(data)
