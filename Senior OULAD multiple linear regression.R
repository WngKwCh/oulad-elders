#assess studentInfo database
studentInfo=read.csv("studentInfo.csv")
str(studentInfo)

#assess courses database
courses=read.csv("courses.csv")
str(courses)

#install packages dplyr
install.packages("dplyr")
library(dplyr)

#install packages ggplot2
install.packages("ggplot2")
library(ggplot2)

#cleanising missing values 
studentInfo[complete.cases(studentInfo), ]%>%
  View()
na.omit(studentInfo)

#review course dates
typeof(courses$module_presentation_length)

courses %>%
  group_by(courses$module_presentation_length)
  mean(courses$module_presentation_length)

courses %>%
  group_by(code_module) %>%
  summarise()
  
#review age categories
studentInfo %>%
  group_by(age_band) %>%
  summarise(count=n())

#Student categories
seniorStu <- filter(studentInfo, age_band=="55<=")
middleStu <- filter(studentInfo, age_band=="35-55")
youngStu <- filter(studentInfo, age_band=="0-35")

#calculate percent of senior
studentInfo %>%
  group_by(studentInfo$count) %>%
  summarise(Young = length (which(age_band == "0-35")),
            Middle = length (which(age_band == "35-55")),
            Senior = length(which(age_band == "55<="))) %>%
  mutate(SeniorModule = (Senior/(Senior+Young+Middle)*100))

#Young Middle Senior SeniorModule
#<int>  <int>  <int>  <dbl>
#22944   9433    216  0.663
ageCat <- c(22944,9433,216)
ageLbl <- c("0-35","35-55","55<=")
pie_percent <- round(100*ageCat/sum(ageCat), 1)

#plot piechart of age in percentage
pie (ageCat,
    labels = pie_percent,
    col = rainbow(length(ageCat)),
    main = "Distribution of age in percentage %")
legend (1,1, c(ageLbl),
        cex=0.8,
        fill = rainbow(length(ageCat)))

#calculate highest education
studentInfo %>%
  group_by(highest_education) %>%
  summarise(count=n())

#barplot of students highest education
StudentEd <- c(347,13158,14045,4730,313)
EdLevel <- c("No formal","Lower A","A Levels","HE Qual","Post grad")
barplot(StudentEd,
        ylim = c(0,15000),
        ylab = "Frequency",
        names.arg = EdLevel,
        main = "All OU Student Prior Educational Level",
        col = rainbow(length(StudentEd)))

#calculate highest education with age_band
studentInfo %>%
  group_by(age_band,highest_education) %>%
  summarise(count=n())

#barplot of senior highest education
SeniorEd <- c(0,33,40,132,11)
EdLevel <- c("No formal","Lower A","A Levels","HE Qual","Post grad")
barplot(SeniorEd,
        ylim = c(0,150),
        ylab = "Frequency",
        names.arg = EdLevel,
        main = "Senior Learner Prior Educational Level",
        col = rainbow(length(SeniorEd)))

#removing empty social econ status rows
SESclean <- filter(studentInfo,imd_band != "")

#calculate percent of social econ status
SESclean %>%
  group_by(imd_band) %>%
  summarise(Young = length (which(age_band == "0-35")),
            Middle = length (which(age_band == "35-55")),
            Senior = length(which(age_band == "55<="))) %>%
  mutate(SeniorModule = (Senior/(Senior+Young+Middle)*100))

#barplot of social econ status
SeniorSES <- c(0,16,6,10,5,30,20,24,34,56)
barplot(SeniorSES,
        ylim = c(0,60),
        ylab = "Frequency",
        xlab = "0-10% ----- < ----- Low ----- < ------ > ------ High ----- > ----- 90-100%",
        main = "Senior Learner Social Economic Status",
        col = rainbow(length(SeniorSES)))

#calculate percentage gender
studentInfo %>%
  group_by(gender) %>%
  summarise(Young = length (which(age_band == "0-35")),
            Middle = length (which(age_band == "35-55")),
            Senior = length(which(age_band == "55<="))) %>%
  mutate(SeniorModule = (Senior/(Senior+Young+Middle)*100))

#calculate percentage module
studentInfo %>%
  group_by(code_module) %>%
  summarise(Young = length (which(age_band == "0-35")),
            Middle = length (which(age_band == "35-55")),
            Senior = length(which(age_band == "55<="))) %>%
  mutate(SeniorModule = (Senior/(Senior+Young+Middle)*100))

#calculate percentage dist final result
studentInfo %>%
  group_by(age_band) %>%
  summarise(Dist = length (which(final_result == "Distinction")),
            Pass = length (which(final_result == "Pass")),
            Witd = length (which(final_result == "Withdrawn")),
            Fail = length (which(final_result == "Fail"))) %>%
  mutate(DistPercentModule=Dist/(Dist+Pass+Fail+Witd)*100)

#barplot of Percentage Distinction across age_band 
DistPercent <- c(8.12,11.9,19.0)
barplot(DistPercent,
        ylim = c(0,20),
        ylab = "Percentage %",
        xlab = "Ages range",
        names.arg = ageLbl,
        main = "Final Result Distinction Percentage",
        col = rainbow(length(SeniorEd)))

#calculate percentage withdrawn final result
studentInfo %>%
  group_by(age_band) %>%
  summarise(Dist = length (which(final_result == "Distinction")),
            Pass = length (which(final_result == "Pass")),
            Witd = length (which(final_result == "Withdrawn")),
            Fail = length (which(final_result == "Fail"))) %>%
  mutate(WitdPercent=Witd/(Dist+Pass+Fail+Witd)*100)

#barplot of Percentage Withdrawal across age_band 
WithdPercent <- c(32.2,28.8,25)
barplot(WithdPercent,
        ylim = c(0,40),
        ylab = "Percentage %",
        xlab = "Age range",
        names.arg = ageLbl,
        main = "Module Withdrawal Percentage",
        col = rainbow(length(SeniorEd)))

#calculate percentage final result
studentInfo %>%
  group_by(age_band,
           highest_education) %>%
  summarise(Dist = length (which(final_result == "Distinction")),
            Pass = length (which(final_result == "Pass")),
            Witd = length (which(final_result == "Withdrawn")),
            Fail = length (which(final_result == "Fail"))) %>%
  mutate(WitdPercent=Witd/(Dist+Pass+Fail+Witd)*100)

#check previous attempts format type
typeof(seniorStu$num_of_prev_attempts)

#review senior previous attempts
seniorStu %>%
  group_by(num_of_prev_attempts) %>%
  summarise(count=n())

#histogram of senior studied credit
hist(seniorStu$studied_credits,
     xlab = "Studied credits",
     xlim = c(0,300),
     ylim = c(0,140),
     col = "yellow",
     main = "Histogram of Senior Learner studied credits",)

#histogram of senior previous attempts
hist(seniorStu$num_of_prev_attempts,
     xlab = "Previous Attempts",
     #     breaks = 0:2,
     freq = TRUE,
     #     xlim = c(0,2),
     main = "Histogram of Senior Learner previous attempts",
     col = "green")
#could not make x-axis as whole integer :(

#Read assessments
assessments=read.csv("assessments.csv")

#Read studentAssessment
studentAssessment=read.csv("studentAssessment.csv")

#Read studentVle
studentVle=read.csv("studentVle.csv")
#Summarize each student's total mumber of clicks in each module each term

studentVleSumC <- studentVle %>%
  group_by(code_module, code_presentation, id_student) %>%
  summarize(sumClick=sum(sum_click))%>%
  mutate(sumClick)

#Combine studentAssessment and assessments by assessment ID 
# To know which assessments are for which module which term
studentAssModule <- left_join(studentAssessment,assessments, 
                              by = c("id_assessment" = "id_assessment"))

#Combine studentAssModule and studentInfo by Student ID, code_module, code_presentation
# Each student can take multiple assessments, so not only combine by studentID
# But also by code_module, and code_presentation
studentInfoAssModule <- left_join(studentAssModule, studentInfo, 
                                  by = c("id_student" = "id_student",
                                         "code_module"="code_module",
                                         "code_presentation"="code_presentation"))

#Combine studentInfoAssModule and studentVle by student ID, code_module, code_presentation
studentInfoAssModVle <- left_join(studentInfoAssModule,studentVleSumC, 
                                  by = c("id_student" = "id_student",
                                         "code_module"="code_module",
                                         "code_presentation"="code_presentation"))

# save ascsv file
write.csv(studentInfoAssModVle, file="studentInfoAssModVle.csv")

# Select Module AAA assessment 1752 
#as we only want to work with one module and one assessment
#to avoid nested data issues
studentInfoAssVleAAA1752 <- filter(studentInfoAssModVle,
                                   code_module == "AAA",
                                   id_assessment == "1752")
# save csv file
write.csv(studentInfoAssVleAAA1752, file="studentInfoAssVleAAA1752.csv")

# Check the types of variables
str(studentInfoAssVleAAA1752)

# check unique values of highest education
unique (studentInfoAssVleAAA1752$highest_education)

# Recode highest education
studentInfoAssVleAAA1752$highest_education <- dplyr::recode(
  studentInfoAssVleAAA1752$highest_education,
  "No Formal quals"=1,
  "Lower Than A Level"=2,
  "A Level or Equivalent"=3,
  "HE Qualification"=4,
  "Post Graduate Qualification"=5)

#linear regression on AAA1752 score with highest education, studied credits & sumClick
model <- lm(score ~ highest_education +
              studied_credits +
              sumClick, 
            data = studentInfoAssVleAAA1752)
#summary(model)

#select senior learner for AAA1752
seniorAssVleAAA1752 <- filter (studentInfoAssVleAAA1752,
                               age_band == "55<=")
# save csv file
write.csv(seniorAssVleAAA1752, file="seniorInfoAssVleAAA1752.csv")

# Check the types of variables
str(seniorAssVleAAA1752)

#linear regression AAA1752 senior on score with highest education, studied credits & sumClick
seniorAAA1752model <- lm(score ~ highest_education +
              studied_credits +
              sumClick, 
            data = seniorAssVleAAA1752)
#summary(seniorAAA1752model)

#select all senior learner
seniorAssVle <- filter (studentInfoAssModVle,
                        age_band == "55<=")
# save csv file
write.csv(seniorAssVle, file="seniorInfoAssVle.csv")

# Recode highest education
seniorAssVle$highest_education <- dplyr::recode(
  seniorAssVle$highest_education,
  "No Formal quals"=1,
  "Lower Than A Level"=2,
  "A Level or Equivalent"=3,
  "HE Qualification"=4,
  "Post Graduate Qualification"=5)

# Check the types of variables
str(seniorAssVle)

#linear regression all senior on score with highest education, studied credits & sumClick
seniormodel <- lm(score ~ highest_education +
                           studied_credits +
                           sumClick, 
                         data = seniorAssVle)
#summary(seniormodel)

#compare linear regression for 3 profiles
summary(seniorAAA1752model)
summary(seniormodel)
summary(model)