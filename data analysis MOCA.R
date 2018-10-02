########################################
##MOCA.csv      
##@auther Shuo Sun sunshuo116@gmail.com
#######################################

setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
MOCA<-read.csv("sheets/MOCA.csv",stringsAsFactors = FALSE)

# data cleaning 

# get headers' name
osc_clonames<-colnames(MOCA) 
unwantedClos<-c("Suspense_File","Form_Notes","BatchNo","BatchCust1","BatchCust2")
#subset for machine read data only: row 1:794
#MOCA<-MOCA[1:788,!names(MOCA) %in% unwantedClos]
MOCA<-MOCA[1:850,c('Trial_Entry_No','Capture_ChoiceList_Total','Folder_Name','Document_Name')]

summary(MOCA)
str(MOCA)

MOCA$Trial_Entry_No<-as.character(MOCA$Trial_Entry_No)

table(duplicated(MOCA)) # 2
MOCA_duplicate<-as.data.frame(MOCA[which(duplicated(MOCA)),])
MOCA_duplicate
## check the duplicated rows
##MOCA[MOCA$Trial_Entry_No=="P0430",] #duplicate
# delete duplicated rows
MOCA<-MOCA[!duplicated(MOCA),]

# check duplicated patient numbers
sort(table(MOCA$Trial_Entry_No)) #P0335, P0430, P0614

#morethan2rownames<-rownames(data.frame(which(sort(table(MOCA$Trial_Entry_No))>2)))
#MOCA<-MOCA[!MOCA$Trial_Entry_No %in% morethan2rownames,]

# find doubled tested patients and sepreate first test and follow ups 
MOCA_unique<-MOCA[unique(MOCA$Trial_Entry_No),]
MOCA_duplicated<-MOCA[duplicated(MOCA$Trial_Entry_No) | duplicated(MOCA$Trial_Entry_No, fromLast=TRUE), ]
MOCA_single_test<-MOCA[!(MOCA$Trial_Entry_No %in% MOCA_duplicated$Trial_Entry_No), ]



#no duplicated row now 
table(grepl("_3", MOCA$Folder_Name))
MOCA$Folder_Name[grepl("_3", MOCA$Folder_Name)]
MOCA_FP<-MOCA[grepl("_3", MOCA$Folder_Name),]
MOCA_First<-MOCA[!grepl("_3", MOCA$Folder_Name),]
sort(table(MOCA_First$Trial_Entry_No,exclude = NULL))
sort(table(MOCA_FP$Trial_Entry_No,exclude = NULL))



MOCA_duplicated$OCS_MOCA<-'MOCA'
MOCA_duplicated_1st<-MOCA_duplicated[!duplicated(MOCA_duplicated$Trial_Entry_No),]
MOCA_duplicated_2nd<-MOCA_duplicated[duplicated(MOCA_duplicated$Trial_Entry_No),]
MOCA_duplicated_1st$FirstTrailORFollowUp<-"FirstTrail"
MOCA_duplicated_2nd$FirstTrailORFollowUp<-"FollowUp"

ocs_duplicated<-arrange(ocs_duplicated, Trial_Entry_No,Date)

#combine the 1st trail and follow up
MOCA_1st_2nd<-rbind(MOCA_duplicated_1st,MOCA_duplicated_2nd)
write.csv(MOCA_1st_2nd, file = "MOCA_data_pb.csv")


#merge

mydata1st <- merge(x=ocs_duplicated_1st, y=NIH_SS_duplicated_1st,suffixes = c('.OCS',".NIH"), by=c("Trial_Entry_No"),all=TRUE)
str(mydata1st)
mydata1st <- merge(x=MOCA_duplicated_1st, y=mydata1st,suffixes = c('.MOCA',".mydata"), by=c("Trial_Entry_No"),all=TRUE)
str(mydata1st)
mydata1st$FirstTrailorFollowUpall<-'FirstTrail'

mydata2nd <- merge(ocs_duplicated_2nd,NIH_SS_duplicated_2nd, suffixes = c('.OCS',".NIH"), by=c("Trial_Entry_No"),all=TRUE)
mydata2nd <- merge(MOCA_duplicated_2nd, mydata2nd,suffixes = c('.MOCA',".mydata"), by=c("Trial_Entry_No"),all=TRUE)
mydata2nd$FirstTrailorFollowUpall<-'FollowUp'


data_1st_2nd<-rbind(mydata1st,mydata2nd)
#lazy temp solution
data_1st_2nd$OCS_MOCA.MOCA[is.na(data_1st_2nd$OCS_MOCA.MOCA)] <- 'OCS'

write.csv(data_1st_2nd, file = "data_1st_2nd_pb.csv")


