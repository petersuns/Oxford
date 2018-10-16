# 11/10/2018
# comnbine script
##############################


###Step 1
###Header_Sheet_Follow_Up.csv
library("dplyr")
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
FP<-read.csv("sheets/Header_Sheet_Follow_Up.csv",stringsAsFactors = FALSE)
FP<-arrange(FP, Trial_Entry_No)
sort(table(FP$Patient_Initials ,exclude = NULL))
sort(table(FP$Site_Name ,exclude = NULL))

# data cleaning 

# get headers' name
FP_clonames<-colnames(FP) 
#subset for machine read data only: row 1:794
FP<-FP[,c('Trial_Entry_No','Case_Details')]

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

###########################
####Step 2
###########################
#NIH_SS 
library("dplyr")
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
NIH_SS<-read.csv("sheets/NIH_SS.csv",stringsAsFactors = FALSE)
# correct data entry/manual mistake
NIH_SS$Folder_Name[930]<-'P0565_2'
NIH_SS$Folder_Name[1225]<-'P0741_3'

NIH_SS<-arrange(NIH_SS, Trial_Entry_No,Date_of_Exam)

# data cleaning 

# get headers' name
NIH_SS_clonames<-colnames(NIH_SS) 
#subset for machine read data only: row 1:794
NIH_SS<-NIH_SS[,c('Trial_Entry_No',"Date_of_Exam",'Score_1','Folder_Name','Document_Name')]
NIH_SS<-NIH_SS[1:1246,c('Trial_Entry_No',"Date_of_Exam","Interval","Q1a_Level_Of_Conciousness",        "Q1b_LOC_questions"  ,             
                        "Q1c_LOC_commands",                 "Q2_Best_Gaze",                     "Q3_Visual",                       
                        "Q4_Facial_Palsy",                  "Q5a_Motor_Arm_Left",               "Q5b_Motor_Arm_Right",             
                        "Q6a_Motor_Leg_Left",               "Q6b_Motor_Left_Right",             "Q7_Limb_Ataxia",                  
                        "Q8_Sensory",                       "Q9_Best_Language",                 "Q10_Dysarthria",                  
                        "Q11_Extiction_Inattention",'Score_1','Document_Name')]

NIH_SS$Date_of_Exam <- as.Date(as.character(NIH_SS$Date_of_Exam),"%d/%m/%Y")


#############where the function starts
#data cleaning for entry error 
#1st check and delete duplicates
#
## check the duplicated rows
table(duplicated(NIH_SS)) # 335 626 624
NIH_SS_duplicate<-as.data.frame(NIH_SS[which(duplicated(NIH_SS)),])
# number of duplicated rows
nrow(NIH_SS_duplicate)

#NIH_SS[NIH_SS$Trial_Entry_No=="P0624",] #duplicate

# delete duplicated rows
NIH_SS<-NIH_SS[!duplicated(NIH_SS),]


#table(table(NIH_SS$Document_Name)>1)
#2nd check and delete more than two assessments 
# check duplicated patient numbers
table(sort(table(NIH_SS$Trial_Entry_No)))#P0335, P0430, P0614
lessthan2_ID<-rownames(data.frame(which(sort(table(NIH_SS$Trial_Entry_No))<2)))
equal2_ID<-rownames(data.frame(which(sort(table(NIH_SS$Trial_Entry_No))==2))) # Thanks god
morethan2_ID<-rownames(data.frame(which(sort(table(NIH_SS$Trial_Entry_No))>2)))

# check the data the occurs three times
morethan2_data<-NIH_SS[NIH_SS$Trial_Entry_No %in% morethan2_ID,]
#NIH_SS<-NIH_SS[!NIH_SS$Trial_Entry_No %in% morethan2rownames,]
NIH_SS[NIH_SS$Trial_Entry_No %in% morethan2_ID,]
#no duplicated row now 
table(sort(table(NIH_SS$Document_Name))) # five duplicate


# morethan2rownames<-rownames(data.frame(which(table(NIH_SS$Document_Name)>1)))
# check the data the occurs three times
morethan2<-NIH_SS[NIH_SS$Trial_Entry_No %in% morethan2_ID,]
#drop the one without date information
morethan2drop<-morethan2_data[(is.na(morethan2$Date_of_Exam)),]
NIH_SS<-NIH_SS[ !(rownames(NIH_SS) %in% rownames(morethan2drop)), ]

# no more than 3 assessments. 

summary(NIH_SS)
str(NIH_SS)
sort(table(NIH_SS$Date_of_Exam,exclude = NULL))
sort(table(NIH_SS$Score_1,exclude = NULL))


sort(table(as.integer(as.character(NIH_SS$Score_1)),exclude = NULL))

NIH_SS$Score_1<-as.integer(as.character(NIH_SS$Score_1))

str(NIH_SS)



table(grepl("_3", NIH_SS$Folder_Name))
NIH_SS$Folder_Name[grepl("_3", NIH_SS$Folder_Name)]
NIH_SS_FP<-NIH_SS[grepl("_3", NIH_SS$Folder_Name),]
NIH_SS_First<-NIH_SS[!grepl("_3", NIH_SS$Folder_Name),]

colnames(NIH_SS_First)[colnames(NIH_SS_First)=="Score_1"] <- "NIH_SS_Score_1st"
colnames(NIH_SS_FP)[colnames(NIH_SS_FP)=="Score_1"] <- "NIH_SS_Score_FP"



sort(table(NIH_SS_First$Trial_Entry_No,exclude = NULL))
sort(table(NIH_SS_FP$Trial_Entry_No,exclude = NULL))

NIH_SS_1st<-NIH_SS_First[NIH_SS_First$Trial_Entry_No %in% NIH_SS_FP$Trial_Entry_No,]
table(NIH_SS_First$Trial_Entry_No %in% NIH_SS_FP$Trial_Entry_No)
table(NIH_SS_1st$Date_of_Exam-NIH_SS_FP$Date_of_Exam,exclude=NULL)
#
NIH_SS_1st$Trial_Entry_No[NIH_SS_1st$Date_of_Exam-NIH_SS_FP$Date_of_Exam>0,]

#

NIH_SS_FP<-NIH_SS_FP[NIH_SS_FP$Trial_Entry_No %in% NIH_SS_1st$Trial_Entry_No,]

setequal(NIH_SS_1st$Trial_Entry_No, NIH_SS_FP$Trial_Entry_No)
all(NIH_SS_1st$Trial_Entry_No %in% NIH_SS_FP$Trial_Entry_No)
table(NIH_SS_1st$Trial_Entry_No %in% NIH_SS_FP$Trial_Entry_No)


##
###########################
####Step 3
###########################

#OCS
library(stargazer)
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
ocs<-read.csv("sheets/OCS.csv",stringsAsFactors = FALSE)
ocs$Folder_Name[649]<-'P0565_2'
ocs$Folder_Name[720]<-'P0626_2'

ocs<-ocs[-c(902,909,911),]
ocs<-ocs[-710,]
ocs<-ocs[-248,]


#ocs$Q9_A_Total
# data cleaning 

# get headers' name
ocs_clonames<-colnames(ocs) 
#subset for machine read data only: row 1:794


ocs$Q1_impairment[ocs$Q1_Total<3]<-1
ocs$Q1_impairment[ocs$Q1_Total>=3]<-0
ocs$Q2_impairment[ocs$Q2_Total<3]<-1
ocs$Q2_impairment[ocs$Q2_Total>=3]<-0

ocs$Q3_impairment[ocs$Q3_Total<4]<-1
ocs$Q3_impairment[ocs$Q3_Total==4]<-0

ocs$Q4_impairment[ocs$Q4_Total<4]<-1
ocs$Q4_impairment[ocs$Q4_Total==4]<-0

ocs$Q5_impairment[ocs$Q5_Total<14]<-1
ocs$Q5_impairment[ocs$Q5_Total>=14]<-0

ocs$Q6_Number_Writing_impairment[ocs$Q6_Number_Writing_Total<3]<-1
ocs$Q6_Number_Writing_impairment[ocs$Q6_Number_Writing_Total==3]<-0

ocs$Q6_Calcs_impairment[ocs$Q6_Calcs_Total<3]<-1
ocs$Q6_Calcs_impairment[ocs$Q6_Calcs_Total>=3]<-0

ocs$Q7_Total_impairment[ocs$Q7_Total_Correct<42]<-1
ocs$Q7_Total_impairment[ocs$Q7_Total_Correct>=42]<-0

ocs$Q7_Object_impairment[ocs$Q7_Object_Asymmetry>1|ocs$Q7_Object_Asymmetry<(-1)]<-1
ocs$Q7_Object_impairment[ocs$Q7_Object_Asymmetry<=1|ocs$Q7_Object_Asymmetry>=(-1)]<-0

ocs$Q7_Space_impairment[ocs$Q7_Space_Asymmetry>3|ocs$Q7_Space_Asymmetry<(-2)]<-1
ocs$Q7_Space_impairment[ocs$Q7_Space_Asymmetry<=3|ocs$Q7_Space_Asymmetry>=(-2)]<-0

ocs$Q8_impairment[ocs$Q8_MGI_Total<8]<-1
ocs$Q8_impairment[ocs$Q8_MGI_Total>=8]<-0

ocs$Q9_A_impairment[ocs$Q9_A_Total<3]<-1
ocs$Q9_A_impairment[ocs$Q9_A_Total>=3]<-0

ocs$Q9_B_impairment[ocs$Q9_B_Total<3]<-1
ocs$Q9_B_impairment[ocs$Q9_B_Total>=3]<-0

ocs$Q9_C_impairment[ocs$Q9_C_Total<3]<-1
ocs$Q9_C_impairment[ocs$Q9_C_Total>=3]<-0

ocs$Q10_impairment[ocs$Q10_4_Executive_Score>4]<-1
ocs$Q10_impairment[ocs$Q10_4_Executive_Score<=4]<-0


ocs_question<-c('Q1_',           'Q2_',        'Q3_', 
                'Q4_',           'Q5_',        'Q6_Calcs_',  
                'Q6_Number_Writing_', 
                'Q7_Total_',  'Q7_Object_',
                'Q7_Space_',
                'Q8_',           'Q9_A_',      'Q9_B_',
                'Q9_C_',         'Q10_')
ocs_recovery<-paste(ocs_question,'recovery',sep='')

total_score_name<-c('Q1_Total','Q2_Total','Q3_Total','Q4_Total','Q5_Total','Q6_Calcs_Total',
                    'Q6_Number_Writing_Total',
                    'Q7_Total_Correct','Q8_MGI_Total','Q9_A_Total','Q9_B_Total','Q9_C_Total',
                    'Q10_4_Executive_Score')

impairment_name<-c('Q1_impairment',           'Q2_impairment',        'Q3_impairment', 
                   'Q4_impairment',           'Q5_impairment',        'Q6_Calcs_impairment',  
                   'Q6_Number_Writing_impairment', 
                   'Q7_Total_impairment',  'Q7_Object_impairment',
                   'Q7_Space_impairment',
                   'Q8_impairment',           'Q9_A_impairment',      'Q9_B_impairment',
                   'Q9_C_impairment',         'Q10_impairment')

id_name<-c('Trial_Entry_No','Date','Folder_Name','Document_Name')



ocs$impairment_number<-rowSums(ocs[,c('Q1_impairment','Q2_impairment','Q3_impairment','Q4_impairment',
                                      'Q5_impairment','Q6_Calcs_impairment','Q6_Number_Writing_impairment',
                                      'Q7_Total_impairment','Q7_Object_impairment','Q7_Space_impairment',
                                      'Q8_impairment','Q9_A_impairment','Q9_B_impairment',
                                      'Q9_C_impairment','Q10_impairment')],na.rm=T)

osc<-c(id_name,impairment_name,total_score_name)

#ocs<-ocs[1:800,c('Trial_Entry_No','Date','Q1_Total','Q2_Total','Q3_Total','Q4_Total','Q5_Total','Q6_Calcs_Total',
#                 'Q6_Number_Writing_Total',
#                 'Q7_Total_Correct','Q8_MGI_Total','Q9_A_Total','Q9_B_Total','Q9_C_Total',
#                 'Q10_4_Executive_Score','Folder_Name','Document_Name')]
ocs$Date <- as.Date(as.character(ocs$Date),"%d/%m/%Y")



#ocs<-ocs[1:794,]

#OCS<-OCS[,c("","","","","","","","","","")]


summary(ocs)
table(ocs$Q7_Total_Correct)
table(as.integer(ocs$Q7_Total_Correct),exclude = NULL)
ocs$Q7_Total_Correct<-as.integer(ocs$Q7_Total_Correct)
table(ocs$Q8_MGI_Total)
table(as.integer(ocs$Q8_MGI_Total),exclude = NULL)
ocs$Q8_MGI_Total<-as.integer(ocs$Q8_MGI_Total)
table(ocs$Q10_4_Executive_Score)
table(as.integer(ocs$Q10_4_Executive_Score),exclude = NULL)
ocs$Q10_4_Executive_Score<-as.integer(ocs$Q10_4_Executive_Score)



ocs_structure<-capture.output(str(ocs),list.len=ncol(ocs))
write.csv(ocs_structure, file='ocs_structure.csv')

table(duplicated(ocs)) # 2

## check the duplicated rows
##ocs[ocs$Trial_Entry_No=="P0430",] #duplicate
# delete duplicated rows
ocs<-ocs[!duplicated(ocs),]

# check duplicated patient numbers
sort(table(ocs$Trial_Entry_No)) #P0335, P0430, P0614
table(sort(table(ocs$Trial_Entry_No)))
# check erro number row
ocs[ocs$Trial_Entry_No=="P0335",] #duplicate, EO col is empty
ocs[ocs$Trial_Entry_No=="P0614",] #999

# delete duplicated row 
which(ocs$Trial_Entry_No=="P0335")
#ocs<-ocs[-381,]


# delete error value 999
library(reshape)

#ocs <- melt(ocs, variable = "impairment_Questions",value='impired_or_not',
#           id=c("Trial_Entry_No","Date",'impairment_number','Folder_Name','Document_Name'))


#
#no duplicated row now 




table(grepl("_3", ocs$Folder_Name))
ocs$Folder_Name[grepl("_3", ocs$Folder_Name)]
ocs_FP<-ocs[grepl("_3", ocs$Folder_Name),]
ocs_First<-ocs[!grepl("_3", ocs$Folder_Name),]


ocs_FP$Q1_impairment[ocs_FP$Q1_Total<3]<-10
ocs_FP$Q2_impairment[ocs_FP$Q2_Total<3]<-10
ocs_FP$Q3_impairment[ocs_FP$Q3_Total<4]<-10
ocs_FP$Q4_impairment[ocs_FP$Q4_Total<4]<-10
ocs_FP$Q5_impairment[ocs_FP$Q5_Total<14]<-10
ocs_FP$Q6_Number_Writing_impairment[ocs_FP$Q6_Number_Writing_Total<3]<-10
ocs_FP$Q6_Calcs_impairment[ocs_FP$Q6_Calcs_Total<3]<-10
ocs_FP$Q7_Total_impairment[ocs_FP$Q7_Total_Correct<42]<-10
ocs_FP$Q7_Object_impairment[ocs_FP$Q7_Object_Asymmetry>1|ocs_FP$Q7_Object_Asymmetry<(-1)]<-10
ocs_FP$Q7_Space_impairment[ocs_FP$Q7_Space_Asymmetry>3|ocs_FP$Q7_Space_Asymmetry<(-2)]<-10
ocs_FP$Q8_impairment[ocs_FP$Q8_MGI_Total<8]<-10
ocs_FP$Q9_A_impairment[ocs_FP$Q9_A_Total<3]<-10
ocs_FP$Q9_B_impairment[ocs_FP$Q9_B_Total<3]<-10
ocs_FP$Q9_C_impairment[ocs_FP$Q9_C_Total<3]<-10
ocs_FP$Q10_impairment[ocs_FP$Q10_4_Executive_Score>4]<-10



table(ocs_FP$Q1_impairment)



summary(ocs_FP)












colnames(ocs_First)[colnames(ocs_First)=="impairment_number"] <- "OCS_impairment_1st"
colnames(ocs_FP)[colnames(ocs_FP)=="impairment_number"] <- "OCS_impairment_FP"

sort(table(ocs_First$Trial_Entry_No,exclude = NULL))
sort(table(ocs_FP$Trial_Entry_No,exclude = NULL))

#OCS_WHO_impairment_what <- merge(x=ppl, y=ocs_First, by=c("Trial_Entry_No"),all=TRUE)
#OCS_WHO_impairment_what <- merge(x=OCS_WHO_impairment_what, y=ocs_FP,suffixes = c('.First','.FollowUp'), by=c("Trial_Entry_No"),all=TRUE)


#make recovery col for every impairment 
#OCS_WHO_impairment_what$Q1_recovery<-OCS_WHO_impairment_what$Q1_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q2_recovery<-OCS_WHO_impairment_what$Q2_impairment.First- OCS_WHO_impairment_what$Q2_impairment.FollowUp
#OCS_WHO_impairment_what$Q3_recovery<-OCS_WHO_impairment_what$Q3_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q4_recovery<-OCS_WHO_impairment_what$Q4_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q5_recovery<-OCS_WHO_impairment_what$Q5_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q6_Calcs_recovery<-OCS_WHO_impairment_what$Q6_Calcs_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q6_Number_Writing_recovery<-OCS_WHO_impairment_what$Q6_Number_Writing_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q7_Total_recovery<-OCS_WHO_impairment_what$Q1_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q7_Object_recovery<-OCS_WHO_impairment_what$Q1_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q7_Space_recovery<-OCS_WHO_impairment_what$Q1_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q8_recovery<-OCS_WHO_impairment_what$Q1_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q9_A_recovery<-OCS_WHO_impairment_what$Q1_impairment.First- OCS_WHO_impairment_what$Q1_impairment.FollowUp
#OCS_WHO_impairment_what$Q9_B_recovery
#OCS_WHO_impairment_what$Q9_C_recovery
#OCS_WHO_impairment_what$Q10_recovery
# alternative way of doing the same thing


#OCS_WHO_impairment_what



ocs_1st_impairment<-merge(x=ppl,y=ocs_First,  by=c("Trial_Entry_No"),all=TRUE)
ocs_FP_impairment<-merge(x=ppl,y=ocs_FP,  by=c("Trial_Entry_No"),all=TRUE)

table(duplicated(ocs_1st_impairment$Trial_Entry_No))
table(duplicated(ocs_FP_impairment$Trial_Entry_No))

ocs_FP_impairment<-ocs_FP_impairment[!duplicated(ocs_FP_impairment$Trial_Entry_No),]

age<-c('age')
#table(ocs_1st_impairment[,colnames(ocs_1st_impairment) %in% impairment_name]-ocs_FP_impairment[,colnames(ocs_FP_impairment) %in% impairment_name])
table(ocs_1st_impairment[,colnames(ocs_1st_impairment) %in% age]-ocs_FP_impairment[,colnames(ocs_FP_impairment) %in% age])

recovery_df<-ocs_1st_impairment[,colnames(ocs_1st_impairment) %in% impairment_name]-ocs_FP_impairment[,colnames(ocs_FP_impairment) %in% impairment_name]
recovery_df<-cbind(ocs_1st_impairment$Trial_Entry_No,recovery_df)
colnames(recovery_df)<-c('Trial_Entry_No',ocs_recovery)
recovery_df[recovery_df==1]<-'Improved'
recovery_df[recovery_df==0]<-'Keep healthy'
recovery_df[recovery_df==-9]<-'Saty impaired'
recovery_df[recovery_df==-10]<-'Worsed'


sapply(recovery_df, function(x) table(x))


#sapply(ocs, function(x) table(x))


sapply(ocs_FP, function(x) table(x))

table(ocs_1st_impairment$Trial_Entry_No)
table(ocs_FP_impairment$Trial_Entry_No)







a<-cbind(OCS_WHO_impairment_what$Q1_impairment.First,OCS_WHO_impairment_what$Q1_impairment.FollowUp,OCS_WHO_impairment_what$Q1_recovery)





#

#


# Overview of the Descriptive statistics 
#stargazer(ocs, type = "html", title="Descriptive statistics", digits=2, out="Descriptive_statistics.htm")

sort(table(ocs$Date,exclude = NULL))

# find doubled tested patients and sepreate first test and follow ups 
ocs$Sheet_Name<-'OCS'
# KEY ppl_with_NIH_SS
ppl_with_OCS<-unique(ocs$Trial_Entry_No) # total number of people who did HIS_SS

# find doubled tested patients and sepreate first test and follow ups 
ocs_duplicated<-ocs[duplicated(ocs$Trial_Entry_No) | duplicated(ocs$Trial_Entry_No, fromLast=TRUE), ]
#NIH_SS_single_test<-NIH_SS_number_of_ppl[!(unique(NIH_SS$Trial_Entry_No) %in% NIH_SS_duplicated$Trial_Entry_No), ]
OCS_single_test<-ocs[!(ocs$Trial_Entry_No %in% ocs_duplicated$Trial_Entry_No), ]
ocs_duplicated$OCS_MOCA<-'OCS'
#ocs_duplicated<-arrange(ocs_duplicated, Trial_Entry_No,Date)
ocs_duplicated<-arrange(ocs_duplicated, Trial_Entry_No)

ocs_duplicated_1st<-ocs_duplicated[!duplicated(ocs_duplicated$Trial_Entry_No),]
ocs_duplicated_2nd<-ocs_duplicated[duplicated(ocs_duplicated$Trial_Entry_No),]

ocs_duplicated_1st$FirstTrailORFollowUp<-"FirstTrail"
ocs_duplicated_2nd$FirstTrailORFollowUp<-"FollowUp"


table(ocs_duplicated_1st$Date-ocs_duplicated_2nd$Date,exclude=NULL)

#combine the 1st trail and follow up

ocs_1st_2nd<-rbind(ocs_duplicated_1st,ocs_duplicated_2nd)

write.csv(ocs_1st_2nd, file = "ocs_data_pb.csv")

###########################
####Step 4
###########################

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
MOCA<-MOCA[,c('Trial_Entry_No','Capture_ChoiceList_Total','Folder_Name','Document_Name')]

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

colnames(MOCA_First)[colnames(MOCA_First)=="Capture_ChoiceList_Total"] <- "MOCA_Score_1st"
colnames(MOCA_FP)[colnames(MOCA_FP)=="Capture_ChoiceList_Total"] <- "MOCA_Score_FP"


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




###########################
####Step 5
###########################
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
SIS<-SIS[,c('Trial_Entry_No','SIS_SCORE')]

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
###########################
####Step 6
###########################

#patients.csv

library("dplyr")

setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
patients<-read.csv('sheets/patients.csv', sep = ';',stringsAsFactors = FALSE)
patients$Trial_Entry_No<-sprintf("P%04d", patients$id)
patients$OCS_MOCA[patients$arm==0]<-'OCS'
patients$OCS_MOCA[patients$arm==1]<-'MOCA'
OCS_ppl<-patients$Trial_Entry_No[patients$OCS_MOCA=='OCS']
MOCA_ppl<-patients$Trial_Entry_No[patients$OCS_MOCA=='MOCA']

patients$age<-patients$year_of_stroke-patients$year_of_birth
ppl<-patients[,c('Trial_Entry_No','age','gender','ethnicity','OCS_MOCA')]

#



#merge
#data <- merge(x=patients, y=NIH_SS_First,suffixes = c('.OCS',".NIH"), by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=ppl, y=NIH_SS_First, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=ocs_First, suffixes = c('.NIH_SS','.First'),by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=MOCA_First, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=NIH_SS_FP, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=ocs_FP,suffixes = c('.First','.FollowUp'), by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=MOCA_FP, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=SIS, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=FP, by=c("Trial_Entry_No"),all=TRUE)
data <- merge(x=data, y=recovery_df, by=c("Trial_Entry_No"),all=TRUE)

#data<-data[,c('Trial_Entry_No','age','gender','ethnicity','OCS_MOCA','Date_of_Exam.x','NIH_SS_Score_1st','Date.x',
#             'OCS_impairment_1st','MOCA_Score_1st','Date_of_Exam.y','NIH_SS_Score_FP','Date.y',
#             'OCS_impairment_FP','MOCA_Score_FP','SIS_SCORE',
#             'Case_Details','Q1_impairment','Q2_impairment','Q3_impairment','Q4_impairment',
#             'Q5_impairment','Q6_Calcs_impairment','Q6_Number_Writing_impairment',
#             'Q7_Total_impairment','Q7_Object_impairment','Q7_Space_impairment','Q8_impairment','Q9_A_impairment','Q9_B_impairment',
#             'Q9_C_impairment','Q10_impairment')]

str(data)

#write.csv(data, file = "ocs_arm_data_pb.csv")
write.csv(data, paste0("ocs_arm_data_pb", format(Sys.time(), " %d:%m:%Y %H;%M"), ".csv"))

t.test(data$NIH_SS_Score_FP[data$OCS_MOCA=='OCS' & data$Case_Details=='Complete'],data$NIH_SS_Score_FP[data$OCS_MOCA=='MOCA' & data$Case_Details=='Complete'])
t.test(data$SIS_SCORE[data$OCS_MOCA=='OCS' & data$Case_Details=='Complete'],data$SIS_SCORE[data$OCS_MOCA=='MOCA' & data$Case_Details=='Complete'])
t.test(data$SIS_SCORE[data$OCS_MOCA=='OCS' & (data$Case_Details=='Complete'|data$Case_Details=='Incomplete')],
       data$SIS_SCORE[data$OCS_MOCA=='MOCA' & (data$Case_Details=='Complete'|data$Case_Details=='Incomplete')])

t.test(data$SIS_SCORE[data$OCS_MOCA=='OCS'],data$SIS_SCORE[data$OCS_MOCA=='MOCA'])
#data trabsfornatin to selct impiarment 

###########################
####Step 7
###########################
# the difference between MMOCA_Score_1st and MOCA_Score_FP


data$MOCA_Recovery<-data$MOCA_Score_1st-data$MOCA_Score_FP
data$OCS_Recovery<-data$OCS_impairment_1st-data$OCS_impairment_FP

table(data$OCS_Recovery)


a<-c(1,2,NA)
b<-c(4,5,6)
c<-cbind(a,b)
c<-data.frame(c)
c$a-c$b
# who recovies from what impairment 

table(ocs_1st_impairment[,colnames(ocs_1st_impairment) %in% impairment_name]-ocs_FP_impairment[,colnames(ocs_FP_impairment) %in% impairment_name])



data$Q1_impairment.First-data$Q1_impairment.FollowUp


impairment_name_First<-c(Q1_impairment)
data$Q2_impairment.FollowUp
impairment_name_First<-paste(impairment_name, "First", sep=".")
impairment_name_FP<-paste(impairment_name, "FollowUp", sep=".")


colnames(recovery_df)<-c('Trial_Entry_No',ocs_recovery)



col_name<-c('Trial_Entry_No','age','gender','ethnicity','OCS_MOCA','Date_of_Exam.x','NIH_SS_Score_1st',
             'OCS_impairment_1st','MOCA_Score_1st','NIH_SS_Score_FP',
             'OCS_impairment_FP','MOCA_Score_FP','SIS_SCORE',
             'Case_Details')
data<- data[,colnames(data) %in% c(col_name,id_name,impairment_name,impairment_name_First,impairment_name_FP,ocs_recovery)]
data<- data[,colnames(data) %in% c(ocs_recovery)]

data$Q2_recovery

table(duplicated(data$Trial_Entry_No))
which(duplicated(data$Trial_Entry_No))
data<-data[!duplicated(data$Trial_Entry_No),]
write.csv(data, paste0("ocs_arm_data_pb", format(Sys.time(), " %d:%m:%Y %H;%M"), ".csv"))



sapply(ocs_recovery_df,function(x) table(x))




prop.table(table(data$Q1_impairment.First-data$Q1_impairment.FollowUp))*100
sum(table(data$Q1_impairment.First-data$Q1_impairment.FollowUp))
table(data$Q1_impairment.First,exclude=NULL)
table(data$Q1_impairment.FollowUp,exclude=NULL)
######################################################################
######################################################################
######################################################################
# for power bi 

bi_data <- melt(data, variable = "impairment_Questions",value='impired_or_not',
           id=c("Trial_Entry_No","Date",'impairment_number','Folder_Name','Document_Name'))

######################################################################
######################################################################
######################################################################