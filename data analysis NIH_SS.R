#NIH_SS 
library("dplyr")
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
NIH_SS<-read.csv("sheets/NIH_SS.csv",stringsAsFactors = FALSE)

# data cleaning 

# get headers' name
NIH_SS_clonames<-colnames(NIH_SS) 
#subset for machine read data only: row 1:794
NIH_SS<-NIH_SS[1:1246,c('Trial_Entry_No',"Date_of_Exam",'Score_1','Document_Name')]
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





#
summary(NIH_SS)
str(NIH_SS)
sort(table(NIH_SS$Date_of_Exam,exclude = NULL))
sort(table(NIH_SS$Score_1,exclude = NULL))


sort(table(as.integer(as.character(NIH_SS$Score_1)),exclude = NULL))

NIH_SS$Score_1<-as.integer(as.character(NIH_SS$Score_1))

str(NIH_SS)






# find doubled tested patients and sepreate first test and follow ups 
NIH_SS$Sheet_Name<-'NIH_SS'
# KEY ppl_with_NIH_SS
ppl_with_NIH_SS<-unique(NIH_SS$Trial_Entry_No) # total number of people who did HIS_SS
#NIH_SS_single_test<-NIH_SS_number_of_ppl[!(unique(NIH_SS$Trial_Entry_No) %in% NIH_SS_duplicated$Trial_Entry_No), ]
NIH_SS_single_test<-NIH_SS[!(NIH_SS$Trial_Entry_No %in% NIH_SS_duplicated$Trial_Entry_No), ]
#####c<-NIH_SS[duplicated(NIH_SS$Trial_Entry_No),]
NIH_SS_duplicated<-NIH_SS[duplicated(NIH_SS$Trial_Entry_No) | duplicated(NIH_SS$Trial_Entry_No, fromLast=TRUE), ]
NIH_SS_duplicated<-arrange(NIH_SS_duplicated, Trial_Entry_No,Date_of_Exam)
#NIH_SS_duplicated<-arrange(NIH_SS_duplicated, Trial_Entry_No)

#NIH_SS_duplicated<-group_by(NIH_SS_duplicated,Trial_Entry_No,Date_of_Exam)


NIH_SS_duplicated_1st<-NIH_SS_duplicated[!duplicated(NIH_SS_duplicated$Trial_Entry_No),]
NIH_SS_duplicated_2nd<-NIH_SS_duplicated[duplicated(NIH_SS_duplicated$Trial_Entry_No),]

table(NIH_SS_duplicated_1st$Date_of_Exam-NIH_SS_duplicated_2nd$Date_of_Exam,exclude=NULL)

NIH_SS_duplicated_1st$NIH_First_or_FP<-"NIH_First"
NIH_SS_duplicated_2nd$NIH_First_or_FP<-"NIH_FP"

#combine the 1st trail and follow up

NIH_SS_1st_2nd<-cbind(NIH_SS_duplicated_1st,NIH_SS_duplicated_2nd)


write.csv(NIH_SS_1st_2nd, file = "NIH_SS_data_pb.csv")

