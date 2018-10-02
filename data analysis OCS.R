#OCS
library(stargazer)
setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
# read data file
ocs<-read.csv("sheets/OCS.csv",stringsAsFactors = FALSE)
ocs$Folder_Name[649]<-'P0565_2'
ocs$Folder_Name[720]<-'P0626_2'
ocs<-ocs[-710,]
#ocs$Q9_A_Total
# data cleaning 

# get headers' name
ocs_clonames<-colnames(ocs) 
#subset for machine read data only: row 1:794


ocs$Q1_impairment[ocs$Q1_Total<3]<-1
ocs$Q2_impairment[ocs$Q2_Total<3]<-1
ocs$Q3_impairment[ocs$Q3_Total<4]<-1
ocs$Q4_impairment[ocs$Q4_Total<4]<-1
ocs$Q5_impairment[ocs$Q5_Total<14]<-1
ocs$Q6_Calcs_impairment[ocs$Q6_Calcs_Total<3]<-1
ocs$Q6_Number_Writing_impairment[ocs$Q6_Number_Writing_Total<3]<-1
ocs$Q7_impairment[ocs$Q7_Total_Correct<42]<-1
ocs$Q8_impairment[ocs$Q8_MGI_Total<8]<-1
ocs$Q9_A_impairment[ocs$Q9_A_Total<3]<-1
ocs$Q9_B_impairment[ocs$Q9_B_Total<3]<-1
ocs$Q9_C_impairment[ocs$Q9_C_Total<3]<-1
ocs$Q10_impairment[ocs$Q10_4_Executive_Score>4]<-1

ocs$impairment_number<-rowSums(ocs[,c('Q1_impairment','Q2_impairment','Q3_impairment','Q4_impairment',
                                      'Q5_impairment','Q6_Calcs_impairment','Q6_Number_Writing_impairment',
                                      'Q7_impairment','Q8_impairment','Q9_A_impairment','Q9_B_impairment',
                                      'Q9_C_impairment','Q10_impairment')],na.rm=T)

a<-data.frame(c(1,2,3,NA))
b<-data.frame(c(3,NA,3,4))
c<-cbind(a,b)
rowSums(c[,c('c.1..2..3..NA.','c.3..NA..3..4.')],na.rm=T)
a+b


ocs<-ocs[1:800,c('Trial_Entry_No','Date','impairment_number','Folder_Name','Document_Name')]
ocs<-ocs[1:800,c('Trial_Entry_No','Date','Q1_Total','Q2_Total','Q3_Total','Q4_Total','Q5_Total','Q6_Calcs_Total',
                 'Q6_Number_Writing_Total',
                 'Q7_Total_Correct','Q8_MGI_Total','Q9_A_Total','Q9_B_Total','Q9_C_Total',
                 'Q10_4_Executive_Score','Folder_Name','Document_Name')]
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
#ocs<-ocs[!ocs$Q2_Total==999,]

#no duplicated row now 
table(grepl("_3", ocs$Folder_Name))
ocs$Folder_Name[grepl("_3", ocs$Folder_Name)]
ocs_FP<-ocs[grepl("_3", ocs$Folder_Name),]
ocs_First<-ocs[!grepl("_3", ocs$Folder_Name),]
sort(table(ocs_First$Trial_Entry_No,exclude = NULL))
sort(table(ocs_FP$Trial_Entry_No,exclude = NULL))



#


# Overview of the Descriptive statistics 
stargazer(ocs, type = "html", title="Descriptive statistics", digits=2, out="Descriptive_statistics.htm")

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












#cor
temp=ocs_duplicated_1st[,c('Q1_Total','Q2_Total','Q3_Total','Q4_Total','Q5_Total','Q6_Calcs_Total',
                        'Q7_Total_Correct','Q8_MGI_Total','Q10_4_Executive_Score')]

temp=ocs_duplicated_1st[,c('Q1_Total','Q2_Total','Q3_Total','Q4_Total','Q5_Total')]
temp$Q7_Total_Correct<-as.numeric(temp$Q7_Total_Correct)
temp$Q8_MGI_Total<-as.numeric(temp$Q8_MGI_Total)
temp$Q10_4_Executive_Score<-as.numeric(temp$Q10_4_Executive_Score)
cor(temp)
summary(temp)
table(temp$Q5_Total,exclude=NULL)
na.omit(temp$Q5_Total)
which(temp$Q5_Total == NA)
which(is.na(temp$Q5_Total))

temp=temp[-38,]
write.csv(temp, file = "temp.csv")

#
stocks=ocs_duplicated_1st
numericVars <- which(sapply(stocks, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
# ?cat, it is easier than print cat{base}
all_numVar <- stocks[, numericVars] #subset the dataframe for numeric variable only
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

cor_numVar <- cor(all_numVar) #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'ReturnJan'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.02)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#
