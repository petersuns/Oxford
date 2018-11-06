    # all patients
    library(dplyr)
    library(summarytools)
    library(knitr)
    library(kableExtra)


    options(digits=2)

    setwd("~/Google Drive (petersun)/Oxford 09:2018/data analysis")
    # read data file
    data_NIH_SS_SIS<-read.csv("data_NIH_SS_SIS.csv",stringsAsFactors = FALSE)

    data_NIH_SS_SIS<-data_NIH_SS_SIS[!is.na(data_NIH_SS_SIS$age), ]

    #data_NIH_SS_SIS<-data_NIH_SS_SIS[data_NIH_SS_SIS$OCS_MOCA=='OCS', ]


    data_NIH_SS_SIS$Q3_1_Date_Stroke[data_NIH_SS_SIS$Trial_Entry_No=='P0697']<-"2015-10-22"
    data_NIH_SS_SIS$Q3_1_Date_Stroke[data_NIH_SS_SIS$Trial_Entry_No=='P0606']<-"2015-08-05"
    data_NIH_SS_SIS$Q3_1_Date_Stroke[data_NIH_SS_SIS$Trial_Entry_No=='P0009']<-"2014-08-15"
    data_NIH_SS_SIS$Q3_1_Date_Stroke[data_NIH_SS_SIS$Trial_Entry_No=='P0178']<-"2015-03-02"
    data_NIH_SS_SIS$Q3_1_Date_Stroke[data_NIH_SS_SIS$Trial_Entry_No=='P0127']<-"2015-01-22"
    data_NIH_SS_SIS$Q3_1_Date_Stroke[data_NIH_SS_SIS$Trial_Entry_No=='P0708']<-"2015-10-31"



    table<-as.Date(data_NIH_SS_SIS$timestamp)-as.Date(data_NIH_SS_SIS$Q3_1_Date_Stroke)
    table<-as.data.frame(table)
    table(as.Date(data_NIH_SS_SIS$timestamp)-as.Date(data_NIH_SS_SIS$Q3_1_Date_Stroke),exclude=NULL)
    #data_NIH_SS_SIS<-data_NIH_SS_SIS[! rownames(data_NIH_SS_SIS) %in% c('824','825','826','693','696'),]

    print('summary of age')
    summary(data_NIH_SS_SIS$age)
    print('sd of age')
    sd(data_NIH_SS_SIS$age)
    freq(data_NIH_SS_SIS$gender, order = "freq")
    freq(data_NIH_SS_SIS$gender, order = "freq")
    number_of_female<-freq(data_NIH_SS_SIS$gender, order = "freq")[2]
    pct_of_female<-freq(data_NIH_SS_SIS$gender, order = "freq")[2+4]

    #kable(freq(data_NIH_SS_SIS$gender, order = "freq"), digits = 1)



    #freq(data_NIH_SS_SIS$ethnicity, order = "freq")
    freq(data_NIH_SS_SIS$OCS_MOCA, order = "freq")

    freq(data_NIH_SS_SIS$Case_Details, order = "freq")

    freq(data_NIH_SS_SIS$Site_Name, order = "freq")

    freq(data_NIH_SS_SIS$NIH_SS_Score_FP, order = "freq")

    freq(data_NIH_SS_SIS$SIS_SCORE, order = "freq")

    freq_table(data_NIH_SS_SIS$Site_Name)

    freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")
    Ischemic<-freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[1]
    haemorrhagic <-freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[2]+freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[5]
    TIA<-freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[7]
    Unknown<-freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[4]
    Other<-freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[5]
    missing_type_stroke<-freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[2]+
                          freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[8]+
                          freq(data_NIH_SS_SIS$Q3_3_Type_of_Stroke, order = "freq")[9]
      
      

    freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")
    left<-freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")[3]
    right<-freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")[2]
    unknown_side<-freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")[4]
    missing_side<-freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")[1]+freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")[7]
    bilateral<-freq(data_NIH_SS_SIS$Q3_9_Lesion_Side, order = "freq")[6]



    freq(data_NIH_SS_SIS$Q2_3_Handedness, order = "freq")
    number_of_lefthand<-freq(data_NIH_SS_SIS$Q2_3_Handedness, order = "freq")[2]
    pct_of_lefthand<-freq(data_NIH_SS_SIS$Q2_3_Handedness, order = "freq")[2+6]

    summary(data_NIH_SS_SIS$Q2_1c_Years_of_Education)
    mean(data_NIH_SS_SIS$Q2_1c_Years_of_Education)
    sd(as.numeric(data_NIH_SS_SIS$age),na.rm = TRUE)
    sd(as.numeric(data_NIH_SS_SIS$Q2_1c_Years_of_Education ),na.rm = TRUE)


    # useful number in writing report
    number_of_patients<-nrow(data_NIH_SS_SIS)


    number_of_sites<-length(unique(data_NIH_SS_SIS$Site_Name))-1

    min_date<-min(data_NIH_SS_SIS$timestamp)
    max_date<-max(data_NIH_SS_SIS$timestamp)

    post_stroke_time<-as.vector(as.Date(data_NIH_SS_SIS$timestamp)-as.Date(data_NIH_SS_SIS$Q3_1_Date_Stroke))
    table(as.Date(data_NIH_SS_SIS$timestamp)-as.Date(data_NIH_SS_SIS$Q3_1_Date_Stroke),exclude=NULL)
    mean(post_stroke_time, na.rm=TRUE)
    sd(post_stroke_time, na.rm=TRUE)
    #post_stroke_time<-as.data.frame(post_stroke_time)

<img src="sheets/geo%20copy.png" alt="Figure 1. Map of England and 36 sites invloved in the study." width="470" />

<font size="6"> **Participants**</font>

<font size="4">

We recruited a consecutive sample of 822 acute stroke patients from 36
sites (see appendix 1 for full list of sites) from England, UK .
Patients were recruited between July, 2014 and July 2016.

**Inclusion criteria** were: 1) acute stroke patients which should be
within 2 months of confirmed stroke and 2) be able to sufficiently
concentrate for 1 hour to in order to finish one of the assessments (as
judged by the multidisciplinary care team in the hospital) and 3) they
need to have sufficient language comprehension to pass the first
orienting tests - Picture Naming and Picture Pointing tests - in the OCS
and 4) willing and able give informed consent themselves.

**Exclusion criteria** were: 1) too unwell to take part, 2) outside of 2
months of the storke.

**For all 822 patients**

The patients' ages ranged from 23 to 95, with an average of 69.04 (SD =
12.49). The average years of education was 11.69 years (SD = 3.66 ).
There are 375 females (45.62%) and 65 left handers (8.53%). The mean
time of test was 13.78 days post stroke (SD = 15.49 ) Lesion location
for the sample were: 253 right hemisphere patients, 208 left hemisphere
patients, 1 bilateral, and 38 indeterminable (293 patients do not have
this information filled in the form).

All of the 822 patients were randomly assigned to two arms: 412 of them
were allocated to MOCA arm and 410 of them were allocated to OCS arm. A
detailed study participation can be found in figure 1. At six months
follow up, 510 patients completed the NIH SS and SIS assessments, the
major reason for the attrition were lost to follow up in six months
(n=93) and incomplete assessment (n=92), withdrawal from study (n=98)
and decease (n=29).

<img src="sheets/chart.png" alt="Figure 2. Flowchart of patient cohort at baseline and follow-up." width="900" />

**For NIH SS SIS analysis patients N=510**

The patients' ages ranged from 23 to 90, with an average of 69.47 (SD =
11.92). The average years of education was 11.89 years (SD = 3.56 ).
There are 238 females (46.67%) and 37 left handers (7.57%). The mean
time of test was 14.15 days post stroke (SD = 15.92 ) Lesion location
for the sample were: 156 right hemisphere patients, 123 left hemisphere
patients, 5 bilateral, and 29 indeterminable (182 patients do not have
this information filled in the form).

<img src="sheets/table.png" alt="Figure 3. Sociodemographic and Stroke-Related Data of the Patients." width="900" />

statistical testing
===================

![](rmarkdown_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  data_NIH_SS_SIS$NIH_SS_Score_FP[data_NIH_SS_SIS$OCS_MOCA == "OCS"] and data_NIH_SS_SIS$NIH_SS_Score_FP[data_NIH_SS_SIS$OCS_MOCA == "MOCA"]
    ## W = 30000, p-value = 0.1
    ## alternative hypothesis: true location shift is not equal to 0

![](rmarkdown_files/figure-markdown_strict/unnamed-chunk-4-2.png)

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  data_NIH_SS_SIS$SIS_SCORE_Transformed_Sum[data_NIH_SS_SIS$OCS_MOCA ==  and data_NIH_SS_SIS$SIS_SCORE_Transformed_Sum[data_NIH_SS_SIS$OCS_MOCA ==     "OCS"] and     "MOCA"]
    ## W = 30000, p-value = 0.2
    ## alternative hypothesis: true location shift is not equal to 0

![](rmarkdown_files/figure-markdown_strict/unnamed-chunk-4-3.png)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  data_NIH_SS_SIS$NIH_SS_Diff[data_NIH_SS_SIS$OCS_MOCA == "OCS"] and data_NIH_SS_SIS$NIH_SS_Diff[data_NIH_SS_SIS$OCS_MOCA == "MOCA"]
    ## t = -0.3, df = 500, p-value = 0.8
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.50  0.38
    ## sample estimates:
    ## mean of x mean of y 
    ##       1.7       1.7

permutation test
================

    #permutation test


    data_NIH_SS_SIS$NIH_SS_Diff<-data_NIH_SS_SIS$NIH_SS_Score_1st-data_NIH_SS_SIS$NIH_SS_Score_FP
    #hist(data_NIH_SS_SIS$NIH_SS_Score_FP)
    #hist(data_NIH_SS_SIS$NIH_SS_Score_1st)
    #hist(data_NIH_SS_SIS$SIS_SCORE_Transformed_Sum)



    one.test <- function(x,y) {
      #x<-data_NIH_SS_SIS$OCS_MOCA
      #y<-data_NIH_SS_SIS$NIH_SS_Score_FP
      xstar<-sample(x)
      mean(y[xstar=='OCS'],na.rm = TRUE)-mean(y[xstar=='MOCA'],na.rm = TRUE)
    }

    # difference between NIH SS follow up
    diff<-mean(data_NIH_SS_SIS$NIH_SS_Score_FP[data_NIH_SS_SIS$OCS_MOCA=='OCS'],na.rm=TRUE)-
      mean(data_NIH_SS_SIS$NIH_SS_Score_FP[data_NIH_SS_SIS$OCS_MOCA=='MOCA'],na.rm = TRUE)
    many.falsenull <- replicate(5000, one.test(data_NIH_SS_SIS$OCS_MOCA, data_NIH_SS_SIS$NIH_SS_Score_FP))

    hist(many.falsenull,main='difference between NIH SS follow up',breaks = 300)
    abline(v=diff, lwd=2, col="red")

![](rmarkdown_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    mean(abs(many.falsenull) > abs(diff))

    ## [1] 0.28

    # difference between SIS follow up
    diff<-mean(data_NIH_SS_SIS$SIS_SCORE_Transformed_Sum[data_NIH_SS_SIS$OCS_MOCA=='OCS'],na.rm=TRUE)-
      mean(data_NIH_SS_SIS$SIS_SCORE_Transformed_Sum[data_NIH_SS_SIS$OCS_MOCA=='MOCA'],na.rm = TRUE)
    many.falsenull <- replicate(5000, one.test(data_NIH_SS_SIS$OCS_MOCA, data_NIH_SS_SIS$SIS_SCORE_Transformed_Sum))

    hist(many.falsenull,main='difference between SIS follow up',breaks = 300)
    abline(v=diff, lwd=2, col="red")

![](rmarkdown_files/figure-markdown_strict/unnamed-chunk-5-2.png)

    mean(abs(many.falsenull) > abs(diff))

    ## [1] 0.16

    # difference between NIH SS difference follow up
    diff<-mean(data_NIH_SS_SIS$NIH_SS_Diff[data_NIH_SS_SIS$OCS_MOCA=='OCS'],na.rm=TRUE)-
      mean(data_NIH_SS_SIS$NIH_SS_Diff[data_NIH_SS_SIS$OCS_MOCA=='MOCA'],na.rm = TRUE)
    many.falsenull <- replicate(5000, one.test(data_NIH_SS_SIS$OCS_MOCA, data_NIH_SS_SIS$NIH_SS_Diff))

    hist(many.falsenull,main='difference between NIH SS difference follow up',breaks = 300)
    abline(v=diff, lwd=2, col="red")

![](rmarkdown_files/figure-markdown_strict/unnamed-chunk-5-3.png)

    mean(abs(many.falsenull) > abs(diff))

    ## [1] 0.79

    #site_list<-read.csv('sheets/site list and location - Sheet1.csv')
    #site_list$Sites.Name<-toupper(site_list$Sites.Name)
    #site_list<-freq(data_NIH_SS_SIS$Site_Name)

    #kable(site_list[,1], digits = 3)
    #kable(site_list[,1]) %>%
    #  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

<img src="sheets/list.png" alt="list 1. list 36 sites invloved in the study." width="900" />

</font>
