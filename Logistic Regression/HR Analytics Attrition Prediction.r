##############################################################################################################
######################################### HR ANALYTICS #######################################################
##############################################################################################################
# Objective - Identify Factors affecting employee churn

# Install and Load the required packages
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")

library(MASS)
library(car)
library(caret)
library(e1071)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(tidyr)
library(corrplot)
library(readr)
library(lubridate)
library(stringr)
library(stringi)
library(woeBinning)
library(InformationValue)

########################## Loading datasets into R ##########################

# Employee Satisfcation data
employee_survey_data <- read_csv("~/OneDrive/Documents/01b ML/01 Upgrad Course - Machine Learning/04 Assignments/06 HR Analytics/PA-I_Case_Study_HR_Analytics/employee_survey_data.csv", 
                                 na = "NA")
#View(employee_survey_data)

# Employee Information Data
general_data <- read_csv("~/OneDrive/Documents/01b ML/01 Upgrad Course - Machine Learning/04 Assignments/06 HR Analytics/PA-I_Case_Study_HR_Analytics/general_data.csv", 
                         na = "NA")
#View(general_data)

# In-Time and Out-Time
employee_in_time <- read_csv("~/OneDrive/Documents/01b ML/01 Upgrad Course - Machine Learning/04 Assignments/06 HR Analytics/PA-I_Case_Study_HR_Analytics/in_time.csv", 
                             na = "NA")
employee_out_time <- read_csv("~/OneDrive/Documents/01b ML/01 Upgrad Course - Machine Learning/04 Assignments/06 HR Analytics/PA-I_Case_Study_HR_Analytics/out_time.csv",
                              na = "NA")
#View(employee_in_time)
#View(employee_out_time)

# Manager Survey Data
manager_survey_data <- read_csv("~/OneDrive/Documents/01b ML/01 Upgrad Course - Machine Learning/04 Assignments/06 HR Analytics/PA-I_Case_Study_HR_Analytics/manager_survey_data.csv", 
                                na = "NA")
#View(manager_survey_data)

employee_in_time <- as.data.frame(employee_in_time)
employee_out_time <- as.data.frame(employee_out_time)
employee_survey_data <- as.data.frame(employee_survey_data)
general_data <- as.data.frame(general_data)

###############################################################################################################
############################################### Structure of Datasets #########################################
dim(employee_out_time) # 4410 rows, 262 Columns
dim(employee_in_time)  # 4410 rows, 262 Columns
dim(employee_survey_data) # 4410 rows, 4 Columns
dim(general_data) # 4410 rows, 24 columns
           # Similar No. of Rows Indicates there is unique Id which can combine all above datasets into one 

#str(employee_out_time)
#str(employee_in_time)
#str(employee_survey_data)
#str(general_data)

######################## Identification of Unique Id among all datasets ########
length(unique(employee_survey_data$EmployeeID)) # 4410, confirming Employee ID is key
length(unique(general_data$EmployeeID)) # 4410, confirming Employee ID is key
setdiff(employee_survey_data$EmployeeID, general_data$EmployeeID) # Identitical Employee ID across datasets

length(unique(employee_in_time$X1))  # 4410, confirming X1 i.e Employee ID is key 
setdiff(employee_in_time$X1,employee_survey_data$EmployeeID)

length(unique(employee_out_time$X1)) # , confirming X1 i.e Employee ID is key
setdiff(employee_out_time$X1, employee_survey_data$EmployeeID)

length(unique(manager_survey_data$EmployeeID)) # 4410, confirming Employee ID is key
setdiff(manager_survey_data$EmployeeID, employee_survey_data$EmployeeID)

colnames(employee_in_time)[1] <- "EmployeeID"
colnames(employee_out_time)[1] <- "EmployeeID"

######################### Duplicates Check ###############################################################
which(duplicated(general_data))
which(duplicated(employee_survey_data))
which(duplicated(employee_in_time))
which(duplicated(employee_out_time))

##########################################################################################################
######################## Data Prep - Employee In and Out #################################################
missing_values <- employee_in_time %>%   summarise_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')
holidays <- filter(missing_values, missing_percentage == 1)   # Extracting Public Holidays
missing_values <- employee_out_time %>%   summarise_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')
holidays_1 <- filter(missing_values, missing_percentage == 1)   # Extracting Public Holidays
setdiff(holidays$missing_percentage,holidays_1$missing_percentage) # In and Out Datasets confirmed Holidays
holidays <- holidays[,-2]

#1# ###### In Time Dataset

employee_in_time[sapply(employee_in_time, is.POSIXct)] <- 
  lapply(employee_in_time[sapply(employee_in_time, is.POSIXct)], as.character)

emp_in <- employee_in_time %>% gather(key = "In_Date", value = "In_Time", -EmployeeID)

emp_in[sapply(emp_in, is.character)] <- lapply(emp_in[sapply(emp_in, is.character)], as.POSIXct)
                        # Converting In-Time Columns into Timestamp Format for extract time
emp_in$timein <- strftime(emp_in$In_Time, format="%H:%M:%S")



#2# ###### Out time dataset

employee_out_time[sapply(employee_out_time, is.POSIXct)] <- 
  lapply(employee_out_time[sapply(employee_out_time, is.POSIXct)], as.character)

emp_out <- employee_out_time %>% gather(key = "Out_Date", value = "Out_Time", -EmployeeID)

emp_out[sapply(emp_out, is.character)] <- lapply(emp_out[sapply(emp_out, is.character)], as.POSIXct)
                        # Converting In-Time Columns into Timestamp Format for extract time

emp_out$timeout <- strftime(emp_out$Out_Time, format="%H:%M:%S")

colnames(emp_in)[2] <- "Date"
colnames(emp_out)[2] <- "Date"



#3# ###### Merge In and Out time of employees to a single dataset
emp_attendance <- merge(emp_in, emp_out, by = c("EmployeeID","Date"), all = F)
emp_attendance <- emp_attendance[,-3] # Removing In_Time Variable
emp_attendance <- emp_attendance[,-4] # Removing Out-Time Variable



#4# ###### Removing Public Holidays
holidays <- as.POSIXct(holidays)
str(holidays)
emp_attendance <- filter(emp_attendance, !Date %in% holidays)



#5# ####### Daily Working Hours in minutes
emp_attendance$timein[is.na(emp_attendance$timein)] <- "00:00:00"
emp_attendance$timeout[is.na(emp_attendance$timeout)] <- "00:00:00"
emp_attendance$timein <- as.numeric(hms(emp_attendance$timein))
emp_attendance$timeout <- as.numeric(hms(emp_attendance$timeout))
str(emp_attendance)
emp_attendance$work_hrs <- round((emp_attendance$timeout - emp_attendance$timein)/3600, 0) 

# Capture Leaves Information
emp_attendance$leave <- ifelse(emp_attendance$work_hrs == 0, 1, 0)

# Aggregated Attendance Info of employees i.e. Avg WOrking hrs and No. of Leaves/Absencees
emp_working_hrs <- emp_attendance[!emp_attendance$work_hrs == 0, ]   
                     # Removing the leaves Info to calculate average working hrs in a day

emp_working_hrs <- emp_working_hrs %>% group_by(EmployeeID) %>% summarise(avg_working_hrs = mean(work_hrs))
emp_leaves <- emp_attendance %>% group_by(EmployeeID) %>% summarise(no_of_leaves = sum(leave))
emp_att <- merge(emp_working_hrs, emp_leaves, by = "EmployeeID")

# Checking Outliers in In-Time & Out-Time data
summary(emp_att)  # Max leaves taken by an employee seems 24 but it is quite possible to take though it may be an outlier and hence, not removing or replacing outlier. Similar is the case for Avg_working_hours


##############################################################################################################
#6#  ###################### Merging Data across all Datasets #################################################

employee <- merge(general_data, employee_survey_data, by = "EmployeeID")
employee <- merge(employee, manager_survey_data, by = "EmployeeID")
employee <- merge(employee, emp_att, by = "EmployeeID")
employee_master <- employee # Another MasterCopy of Employee merged Data

#7# ####### Derive overtime

#employee$overtime <- ifelse(round(employee$avg_working_hrs,0) > 8, 1,0)

#7b# ########## Structure of the Employee Dataset 

str(employee)
dim(employee) # 4410 Employees data, 31 Attributes
summary(employee) 
sapply(employee, sd) # Employee Count, Standard Hours Columns has no variation in data

levels(factor(employee$Attrition)) # No, Yes
levels(factor(employee$BusinessTravel)) # Non-Travel, Travel_Frequently, Travel_Rarely
levels(factor(employee$Department)) # Human Resources, Research & Development, Sales
levels(factor(employee$EducationField)) # Human Resources, Life Sciences, Marketing, Medical, Other, Tech Degree
levels(factor(employee$Gender)) # Female, Male
levels(factor(employee$JobRole)) # 9 Levels of Roles exists
levels(factor(employee$MaritalStatus)) # Divorced, Married, Single
levels(factor(employee$Over18))  # No Variation in the data - Every Employee is over 18 years of age

employee <- employee[, !colnames(employee) %in% c("EmployeeCount", "StandardHours", "Over18")]
                                # Remove columns - Employeecount, Over18 and StandardHours



#8# ########## Missing values

msg_value = data.frame(lapply(employee, function(x) NAs = sum(is.na(x))) )
msg_values <- gather(msg_value, key = "Feature", value = "Missing Values Count")
msg_values <- msg_values[msg_values$`Missing Values Count` > 0, ]
View(msg_values)
           # shows 19 NAs are in NumCompaniesWorked column
           # shows 25 NAs are in EnvironmentSatisfaction column
           # shows 20 NAs are in JobSatisfaction column
           # shows 38 NAs are in WorkLifeBalance column
           # shows 09 NAs are in Total working years

#9# ############# Defining Function for Plots

# Barcharts for categorical features with employee information
library(scales)

bar_theme1 <- theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.y=element_blank())

univariate_categorical <- function(dataset,var1,var2,var_l,var_name1,var_name2){
  
  plot_grid(
    
    dataset %>% ggplot(aes(x = as.factor(var1), fill = var_l)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),position = "fill") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25,, position = "fill") + 
      scale_y_continuous(labels = percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Percent", x = var_name1)+theme(
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.y=element_blank()),
    
    dataset %>% ggplot(aes(x = as.factor(var2), fill = var_l)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),position = "fill") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25,, position = "fill") + 
      scale_y_continuous(labels = percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Percent", x = var_name2)+theme(
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.y=element_blank()),
    
    dataset %>% ggplot(aes(x = as.factor(var1), fill = var_l)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),position = "dodge") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25,, position = position_dodge(0.9)) + 
      scale_y_continuous(labels = percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Percent", x = var_name1)+theme(
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.y=element_blank()),
    
    dataset %>% ggplot(aes(x = as.factor(var2), fill = var_l)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),position = "dodge") +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25,, position = position_dodge(0.9)) + 
      scale_y_continuous(labels = percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Percent", x = var_name2)+theme(
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.y=element_blank()), align = "h"
  )
}

#10# ############# Continous & Categorical Variables

# Categorical Variables - Attrition, BusinessTravel, Department, Education, EducationField, Gender,
#                         JobLevel, JobRole, MaritalStatus, NumCompaniesWorked, StockOptionLevel,
#                         TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion,
#                         YearsWithCurrManager, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance,
#                         JobInvolvement, PerformanceRating

# Continous Variables - DistanceFromHome, MonthlyIncome, PercentSalaryHike, avg_working_hrs
#                       no_of_leaves,Age

###########################################################
#11-A# ############## Plots for Business Travel, Department
############################################################

univariate_categorical(employee, employee$BusinessTravel,employee$Department,employee$Attrition,"Business Travel","Department")
            # Department - HR seems to have more attrition but less no. of employees against employee strength
            # Business Travel - Travel Frequently sees more attrition

            # R&D (10%), Sales(5%) Attrition rates
            # Travel Rarely has >10% Atrrition rate

employee %>% group_by(Attrition, BusinessTravel) %>% summarise( Count = n(), Count_pr = n()/nrow(employee)*100)

###########################################################
#11-B# ############## Plots for Gender, Education Field
###########################################################

univariate_categorical(employee, employee$Gender,employee$EducationField, employee$Attrition,"Gender","Education Field")
            # All Education fields are having equivalent proportion of Attrition rate
            # However, HR seems high attrition rate but very less proportion of employees as against emp strength
            # Not much major difference in Attrition rate w.r.t Gender

            # Medical, Lifescience has > 5% Attrition rate

employee %>% group_by(Attrition, EducationField) %>% summarise( Count = n(), Count_pr = n()/nrow(employee)*100)


##########################################################
#11-C# ############## Plots for JobRole, JobLevel
###########################################################

univariate_categorical(employee, employee$JobLevel,employee$JobRole, employee$Attrition,"JobLevel","JobRole")
            
# All Job levels seems to have proportionally similar attrition rates as per their employee count
            # Research Scientist, Sales Executive, Lab Technician has proportionally higher attrition rate compared to their employee count as well as their head count is also reasonabily higher in company
            # Job levels - 1 & 2 has higher attrition rates in general

###########################################################
#11-D# ############## Plots for MaritalStatus, NumCompaniesWorked
###########################################################

univariate_categorical(employee, employee$MaritalStatus,employee$NumCompaniesWorked, employee$Attrition,"MaritalStatus","NumCompaniesWorked")
           # Maritial Status - Divorce has lower attrition rate, Unmarried has higher attrition rate
           # Num of companies worked >=5 tend to have attrition rate but relatively less employee in this category
           # Num of companies worked = 1 has higher attrition rate when compared to overall employee count
           # Can bin into 4 Categories for Num of companies worked - 0,1,2-3-4,>5

  # 0.3% of data points has NAs in Num of companies worked - Can be removed from dataset
  # It seems missing data is by random and hence, can be removed

###########################################################
#11-E# ############## Plots for YearsAtCompany, TotalWorkingYears
###########################################################

univariate_categorical(employee, employee$TotalWorkingYears,employee$YearsAtCompany, employee$Attrition,"TotalWorkingYears","YearsAtCompany")
           # As no. of total work experience increase - Attriton rate decreases
           # Higher Attrition rates at exp levels 1 - 7 Years
           # <2 years of tenure spend by employees tend to leave company as per Years at Current Company


###########################################################
#11-E# ############## Plots for TrainingTimesLastYear, StockOptionLevel
###########################################################

univariate_categorical(employee, employee$StockOptionLevel,employee$TrainingTimesLastYear, employee$Attrition,"StockOptionLevel","TrainingTimesLastYear")
           # Stock Options relatively not much impact but no stock options or 1 tends to get churn
           # Trainings with 2 or 3 seems to get churn lot 


###########################################################
#11-E# ############## Plots for YearsSinceLastPromotion, YearsWithCurrManager
###########################################################

univariate_categorical(employee, employee$YearsSinceLastPromotion,employee$YearsWithCurrManager, employee$Attrition,"YearsSinceLastPromotion","YearsWithCurrManager")
           # Years Since Promotion may not be having much impact, but interesting, immediately after promotion getting churn - Need to check whether people having experience of <2 Years
          

###########################################################
#11-E# ############## Plots for EnvironmentSatisfaction, JobSatisfaction
###########################################################

univariate_categorical(employee, employee$EnvironmentSatisfaction,employee$JobSatisfaction, employee$Attrition,"EnvironmentSatisfaction","JobSatisfaction")
      # Lower environment satisfaction seems to have higher attrition ratio
      # 0.5% of NA's in environment satisfaction
      # 0.4% of Job Satisfaction - NA's
      # Lower Job Satisfaction seems to have higher employeer churn 

###########################################################
#11-F# ############## Plots for WorkLifeBalance, JobInvolvement
###########################################################

univariate_categorical(employee, employee$WorkLifeBalance,employee$JobInvolvement, employee$Attrition,"WorkLifeBalance","JobInvolvement")
      # unsatisfied employees seems to churn more & 0.8% of NA's in Worklifebalance
      # During lesser job involvement, employee seems to churn more

###########################################################
#11-G# ############## Plots for PerformanceRating, no_of_leaves
###########################################################

univariate_categorical(employee, factor(employee$no_of_leaves), employee$PerformanceRating, employee$Attrition, "no_of_leaves","PerformanceRating")
      # Performance Rating for all employees is between Excellent and Outstanding i.e. 3 and 4


#############################
# Continous Variables - DistanceFromHome, MonthlyIncome, PercentSalaryHike, avg_working_hrs,                                              no_of_leaves_s, Age
# Histogram, Density Plots and Boxplots for numeric variables

##########################################################
### Few Foundations for Continous Variables Plots
##########################################################

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())


###########################################################
#11-H# ############## Plots for Average Working Hours
###########################################################

plot_grid(ggplot(employee, aes(avg_working_hrs, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=avg_working_hrs, fill = Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, align = "v",ncol = 1)

summary(employee$avg_working_hrs)

ggplot(employee, aes(x="",y=avg_working_hrs, fill = Attrition))+ geom_boxplot(width=0.1)
 # More than 50% of Attrition rate is fallling above >8 hrs of work time

employee$avg_working_hrs <- ifelse(employee$avg_working_hrs >8, "overtime",
                                          ifelse(employee$avg_working_hrs >= 7, "7-8 Hours","< 7 Hours"))

ggplot(employee) + geom_bar(aes(avg_working_hrs, fill = Attrition),position = "fill")
        # If Avg_working_hrs > 8 then more attrition is the trend observed
        # Clearly, Overtime employee tend to get move out of organisation

###########################################################
#11-I# ############## Plots for Distance from home
###########################################################

plot_grid(ggplot(employee, aes(DistanceFromHome, fill = Attrition))+ geom_histogram(binwidth = 2),
          ggplot(employee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "h",ncol = 1)
ggplot(employee, aes(DistanceFromHome, fill = Attrition))+ geom_density(alpha=0.7)
ggplot(employee, aes(DistanceFromHome, fill = Attrition))+ geom_histogram(binwidth = 1,position = "fill")
        # No outliers
        # Most of the employees who have left the organization are <2 KMs range which is near to office



###########################################################
#11-J# ############## Plots for Monthly Income
###########################################################

plot_grid(ggplot(employee, aes(MonthlyIncome, fill = Attrition))+ geom_histogram(binwidth = 5000),
          ggplot(employee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=1)+coord_flip()+box_theme, 
          align = "h",ncol = 1)
ggplot(employee, aes(MonthlyIncome, fill = Attrition))+ geom_density(alpha = 0.7)

ggplot(employee, aes(x="",y=MonthlyIncome, fill = Attrition))+ geom_boxplot() + facet_grid(~JobLevel)
   # Job Leve - 3 who moved out of organisation has most of them receiving median salary of same job level who stayed in the organisation

 # Derive a new Interaction variable - who has lesser income than median salary of people who stayed in the organisation

a <- employee %>% group_by(JobLevel, Attrition) %>% summarise(median_salary = median(MonthlyIncome))
a <- data.frame(a)
a1 <- a$median_salary[a$JobLevel == 3 & a$Attrition == "No"] # Median Salary - 51070 for JL - 3 & Att - No

employee$salary_joblevel3_lesserthanmedian_no <- ifelse(employee$MonthlyIncome < a1, 1, 0)
ggplot(employee, aes(salary_joblevel3_lesserthanmedian_no, fill = Attrition))+geom_bar()

 # Monthly Oncome vs Job Role
plot_grid(ggplot(employee, aes(JobRole, fill = Attrition))+ geom_bar(),
          ggplot(employee, aes(x="",y=MonthlyIncome, fill = Attrition))+ geom_boxplot() + facet_grid(~JobRole)+box_theme, align = "v",ncol = 1)


# Highly skewed monthly income 

summary(employee$MonthlyIncome)

IQR(employee$MonthlyIncome)*1.5 + quantile(employee$MonthlyIncome, 0.75) # Higher than this amount are outliers

quantile(employee$MonthlyIncome, seq(0, 1, 0.01))

# Dervie Salary Ratio as against Median - Is it above median or low by Job Level
Median_Salary <- aggregate(employee$MonthlyIncome, list(JobLevel = employee$JobLevel), median)

employee <- merge(employee, Median_Salary, by = c("JobLevel"))

colnames(employee)[30] <- "Median Salary by JobLevel"

employee$salary_ratio <- employee$MonthlyIncome/employee$`Median Salary by JobLevel`

ggplot(employee, aes(salary_ratio, fill = Attrition))+ geom_density(alpha = 0.7)
      # More Attrition when employee salary is lesser than median salary range by Job Level
      # More Attrition when employee salary is 2 times above median salary

ggplot(employee, aes(x = factor(JobLevel), y = salary_ratio, color = Attrition)) + geom_boxplot() + facet_wrap(~Attrition)
      # Attrition is there since median salary by Joblevel is decreasing from 1 - 3 levels

ggplot(employee, aes(x = "", y = salary_ratio, fill = Attrition)) + geom_boxplot() 
    # Not much different in salary difference when compared to median salaries at Job level. However, top end salaries for churned employees is lesser than the current employees who are earning higher salaries

employee$MonthlyIncome[employee$MonthlyIncome > (IQR(employee$MonthlyIncome)*1.5 + quantile(employee$MonthlyIncome, 0.75))] <- IQR(employee$MonthlyIncome)*1.5 + quantile(employee$MonthlyIncome, 0.75)
          #Treatment of outliers

ggplot(employee, aes(x="",y=MonthlyIncome))+ geom_boxplot() # No outliers

employee$salary_ratio[employee$salary_ratio > (quantile(employee$salary_ratio,0.75)+1.5*IQR(employee$salary_ratio))] <- (quantile(employee$salary_ratio,0.75)+1.5*IQR(employee$salary_ratio)) #Treatment of outliers

ggplot(employee, aes(x = "", y = salary_ratio)) + geom_boxplot()  # No outliers

###########################################################
#11-K# ############## Plots for Percentage Hike
###########################################################

plot_grid(ggplot(employee, aes(PercentSalaryHike, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "h",ncol = 1)
    # No Outliers, 

ggplot(employee, aes(PercentSalaryHike, fill = Attrition))+ geom_histogram(binwidth = 1, position = "fill") 

ggplot(employee, aes(PercentSalaryHike, color = factor(PerformanceRating), fill = Attrition))+ geom_histogram(binwidth = 1)
    # Percent hike dont have any major impact

###########################################################
#11-L# ############## Plots for Age
###########################################################


plot_grid(ggplot(employee, aes(Age, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "h",ncol = 1)

plot_grid(ggplot(employee, aes(Age, fill = Attrition))+ geom_histogram(binwidth = 1,, position = "dodge"),
          ggplot(employee, aes(Age, fill = Attrition)) + geom_density(alpha = 0.7), 
          align = "v",ncol = 1)

    # Attrition is increasing during Age Period 18 - 31 and later decreasing

###########################################################
#11-L# ############## Corelation Analysis - Bivariate Continous variable analysis
###########################################################

# Correlation between numeric variables
library(GGally)
ggpairs(employee[, c("DistanceFromHome", "salary_ratio", "PercentSalaryHike", "avg_working_hrs", "Age")])
        # No Corelation among these variables

##########################################################################################################
##################### Bi-Variate analysis ################################################################
##########################################################################################################


### 1. Department vs JobRole vs Attrition Rate

a <-employee %>% group_by(Department, JobRole, JobLevel, Attrition) %>% summarise(n()/nrow(employee))
View(a)   # Clearly Indicating that Department Data seems to be incorrect and hence, better to drop and if             need can be added later
          # Deparments are wrongly mapped to JobRole

### 2. JobRole vs EducationField vs Attrition Rate

a <- employee %>% group_by(JobRole, EducationField, Attrition) %>% summarise(n()/nrow(employee)*100)
View(a)

### 


#############################################################################################################
# # ################ Binning Variables - Based on above plots
#############################################################################################################

### Assumption ###
# As per above plots and data, it is clear that data in "Total Working Years" and "Number of Companies Worked" has descpriances and hence, following data transformation are performed with an key assumption that - "No. of companies worked is minimum - 1 by considering current work experience in current organisation

################################################
#Bin-1# ########### Number of Companies Worked
################################################

a = employee
a$diff = a$TotalWorkingYears - a$YearsAtCompany

a = subset(employee, NumCompaniesWorked == 0)
a$diff = a$TotalWorkingYears - a$YearsAtCompany # Total Work Experience = Years @ Company + 1 if Number of Companies worked = 0

length(which(a$YearsAtCompany == 0))
length(which(a$TotalWorkingYears == 1))
length(which(a$diff == 1 & a$TotalWorkingYears == 1)) # Total Work Experience = Years @ Company + 1 if Number of Companies worked = 0

b = subset(employee, NumCompaniesWorked == 1)
b$diff = b$TotalWorkingYears - b$YearsAtCompany # Total Work Experience - Years @ Company >= 2 if Number of Companies Worked = 2
        # Total Work Experience = Years @ Company + 1 (or) Years @ Company if Number of Companies Worked = 1
        # Hence, 
           # change the Total Work Experience with (Years at Company + 1) for No. of Companies Worked = 1 
           # change the No. of Companies worked as 1 for No. of companies worked as 0 based on above explanation

sum(is.na(employee$TotalWorkingYears)) # 9 NA's
sum(is.na(employee$NumCompaniesWorked)) # 19 NA's
sum(is.na(employee$YearsAtCompany)) # 0 NA's

employee$NumCompaniesWorked[employee$NumCompaniesWorked == 0] <- 1 # Replacing No. of Companies Worked as 1 for 0's 

employee$TotalWorkingYears[which(is.na(employee$TotalWorkingYears) & employee$NumCompaniesWorked == 1)] <- employee$YearsAtCompany[which(is.na(employee$TotalWorkingYears) & employee$NumCompaniesWorked == 1)] + 1 
# Replacing few NA's for condition No. of companies worked = 1
sum(is.na(employee$TotalWorkingYears)) # 5 NA's

# Missing Value Imputation for Num of companies worked feature
employee$NumCompaniesWorked[which(employee$TotalWorkingYears-employee$YearsAtCompany == 1 & is.na(employee$NumCompaniesWorked))] <- 1 

employee$NumCompaniesWorked[which(employee$TotalWorkingYears-employee$YearsAtCompany == 0 & is.na(employee$NumCompaniesWorked))] <- 1 

employee$TotalWorkingYears[which(employee$NumCompaniesWorked == 1)] <- employee$YearsAtCompany[which(employee$NumCompaniesWorked == 1)]+1
           # Total Work Experience with (Years at Company + 1) for No. of Companies Worked = 1 

sum(is.na(employee$NumCompaniesWorked)) # 9 NA's

employee <- employee[!is.na(employee$NumCompaniesWorked), ] # 9 Rows were removed

# Binning based on Univariate into grops - 1 company, 2-4 companies, 5-6 companies, 9 companies, Missing
employee$NumCompaniesWorked1 <- ifelse(employee$NumCompaniesWorked >=5, ">=5 Companies",
                                 ifelse(employee$NumCompaniesWorked >=2, "2-4 Companies",
                                        ifelse(employee$NumCompaniesWorked >=0, "1 Company","Missing")))

# Handling Missing Values of No. of companies worked
employee <- employee[!(employee$NumCompaniesWorked == "Missing"), ] # 9 Rows were removed

################################################
#Bin-2# ########### TrainingTimesLastYear
################################################

employee$TrainingTimesLastYear1 <- ifelse(employee$TrainingTimesLastYear >= 4, "4-6 Trainings",
                                         ifelse(employee$TrainingTimesLastYear >=2, "2-3 Trainings","0-1 Trainings"))

################################################
#Bin-3# ########### TotalWorkingYears
################################################

(sum(is.na(employee$TotalWorkingYears)))/nrow(employee)  # 0.1% of overall dataset - Missing Info for Total WOrking Experinece variable
(sum(is.na(employee$TotalWorkingYears) & employee$Attrition == "Yes"))/nrow(employee) # 0.02% for Attrition-Yes for Missing Info of Total Working Experence Variable. Hence, we can remove from dataset as % is very less

employee <- employee[!is.na(employee$TotalWorkingYears), ] # 5 Rows were removed
sum(is.na(employee$TotalWorkingYears)) # No Missing Values for Total Working Years

employee$TotalWorkingYears1 <- ifelse(employee$TotalWorkingYears >= 35, ">=35 Years",
                                     ifelse(employee$TotalWorkingYears >= 31, "31-34 Years",
                                            ifelse(employee$TotalWorkingYears >= 21, "21-30 Years",
                                                   ifelse(employee$TotalWorkingYears >= 11, "11-20 Years",
                                                          ifelse(employee$TotalWorkingYears >= 6, "6-10 Years",
                                                                 ifelse(employee$TotalWorkingYears >=3, "3-5 Years", "1-2 Years"))))))


################################################
#Bin-4# ########### Years at Current Company
################################################

employee$YearsAtCompany1 <- ifelse(employee$YearsAtCompany >= 26, ">25 Years",
                                ifelse(employee$YearsAtCompany >= 21, "21-25 Years",
                                     ifelse(employee$YearsAtCompany >= 11, "11-20 Years",
                                         ifelse(employee$YearsAtCompany >=5, "5-10 Years",
                                             ifelse(employee$YearsAtCompany >= 2, "2-4 Years","<2 Years")))))





################################################
#Bin-5# ########### Stock Options - Combine 2 and 3
################################################

employee$StockOptionLevel1<- ifelse((employee$StockOptionLevel == 2 | employee$StockOptionLevel == 3), "2-3 Stocks",ifelse(employee$StockOptionLevel == 1, "1 Stock", "No Stocks Option"))



################################################
#Bin-6# ########### Years Since Last Promotion
################################################

employee$YearsSinceLastPromotion1 <- ifelse(employee$YearsSinceLastPromotion >=11 , ">10 years",
                                        ifelse(employee$YearsSinceLastPromotion >= 7, "7-10 Years", 
                                               ifelse(employee$YearsSinceLastPromotion >=4, "4-6 Years",
                                                     "0-3 Years")))


################################################
#Bin-7# ########### Years with Current Manager
################################################

employee$YearsWithCurrManager1 <- ifelse(employee$YearsWithCurrManager >=8 , "> 8 Years",
                                        ifelse(employee$YearsWithCurrManager >= 3, "3-7 Years", "0-3 Years"))

################################################
#Bin-8# ########### Distance from Home
################################################

employee1 <- employee
employee1$Attrition <- ifelse(employee$Attrition == "Yes", 1, 0)
woe.binning(employee1, 'Attrition', 'DistanceFromHome', min.perc.total=0.02, min.perc.class=0.01, stop.limit=0.1, event.class=1) # 4.6 % Prediction Power, Not good

employee$DistanceFromHome1 <- ifelse(employee$DistanceFromHome >=20, ">20 KMS",
                                    ifelse(employee$DistanceFromHome >=10, "10-20 KMS",
                                           ifelse(employee$DistanceFromHome >=5, "5-10 KMS", 
                                                  ifelse(employee$DistanceFromHome >=3, "3-4.9 KMS", "0-3 KMS"))))

################################################
#Bin-9# ########### Percent Salary Hike
################################################

ggplot(employee, aes(PercentSalaryHike, fill =factor(PerformanceRating)))+ geom_bar()
          # 10-19% Hike for performance rating - 3
          # >20% Hike for performance rating - 4

ggplot(employee, aes(PercentSalaryHike, fill =factor(Attrition)))+ geom_bar()
          # More Attrition for Performance rating - 3 that who got hike < 15% 

employee$PercentSalaryHike1 <- ifelse(employee$PercentSalaryHike > 20, "Over 20%",
                                     ifelse(employee$PercentSalaryHike > 15, "15.01 - 20.00%","10.00-15.00%"))

ggplot(employee, aes(PercentSalaryHike1, fill =factor(Attrition)))+ geom_bar()
          # More Attrition for Performance rating - 3 that who got hike < 15% 

################################################
#Bin-10# ########### Age
################################################

summary(employee$Age)

employee1 <- employee
employee1$Attrition <- ifelse(employee$Attrition == "Yes", 1, 0)
woe.binning(employee1, 'Attrition', 'Age', min.perc.total=0.025, min.perc.class=0.01, stop.limit=0.1, event.class=1) # 37 % Prediction Power ==> 18-21, 22-26, 27, 28-33, 33-40, >40

ggplot(employee, aes(Age, fill=Attrition))+geom_histogram(binwidth = 2)
ggplot(employee, aes(Age, fill=Attrition))+geom_histogram(binwidth = 2, position = "fill")

employee$Age1 <- ifelse(employee$Age >= 41, ">40 Years",
                                     ifelse(employee$Age >= 32, "32-40 Years",
                                            ifelse(employee$Age >= 26, "26-31 Years", "18-25 Years")))

################################################
#Bin-11# ########### Leaves
################################################

ggplot(employee, aes(no_of_leaves, fill=Attrition))+geom_histogram()
ggplot(employee, aes(no_of_leaves, fill=Attrition))+geom_density(alpha = 0.7)

employee1 <- employee
employee1$Attrition <- ifelse(employee$Attrition == "Yes", 1, 0)
employee1$v <- factor(employee1$no_of_leaves)
library("woeBinning")
woe.binning(employee1, 'Attrition', 'no_of_leaves', event.class = 1) # 4.2 % Prediction Power
          # Prediction power is not good to consider WOE Transformation

employee$leaves <- ifelse(employee$no_of_leaves >= 10, ">=10 Leaves",
                          ifelse(employee$no_of_leaves >5, ">5 Leaves", "0-5 Leaves"))

###################################
#### NAs - Environment Satisfaction
###################################

sum(is.na(employee$EnvironmentSatisfaction))/nrow(employee) # 0.5% of data in overall data is NA's w.r.t Env. Satisfaction
table(employee$Attrition) # Yes- 707, No - 3689; Yes - 19%, No - 81%

employee1 <- employee
employee1$Attrition <- ifelse(employee$Attrition == "Yes", 1, 0)
employee1$environmentSatisfaction <- factor(employee1$EnvironmentSatisfaction)
library("woeBinning")
woe.binning(employee1, 'Attrition', 'environmentSatisfaction', event.class = 1) # 9.5 % Prediction Power
        # 3,4 of Environment satisfaction can be binned without reducing much prediction power. However, <10%               Prediction power is not good to consider WOE Transformation

View(employee[is.na(employee$EnvironmentSatisfaction),])
        # It seems missing values are randomly missing in environment satisfaction and hence, we can remove it

employee <- employee[!is.na(employee$EnvironmentSatisfaction),]


###################################
#### NAs - Job Satisfaction
###################################

sum(is.na(employee$JobSatisfaction)) # 20 NA's
sum(is.na(employee$JobSatisfaction)) / nrow(employee) # 0.4% compared to over dataset
length(which(is.na(employee$JobSatisfaction & employee$Attrition == "Yes"))) # 1 out of 20 

employee <- employee[!is.na(employee$JobSatisfaction),] # Remove NA rows from dataset w.r.t Job Satisfaction


###################################
##### NAs- Worklife balance
###################################

sum(is.na(employee$WorkLifeBalance)) # 38 NA's
length(which(is.na(employee$WorkLifeBalance & employee$Attrition == "Yes"))) # 4 NA's out of 28 are moved out

employee1 <- employee
employee1$Attrition <- ifelse(employee$Attrition == "Yes", 1, 0)
employee1$WorkLifeBalance <- factor(employee1$WorkLifeBalance)
woe.binning(employee1, 'Attrition', 'WorkLifeBalance', event.class = 1) # 5.5 % Prediction Power

employee <- employee[!is.na(employee$WorkLifeBalance),] # WOE Predictive power is not good, no specific trend observed with other variables and hence, remove NAs

####################################
############ Checking Missing values
####################################

msg_value = data.frame(lapply(employee, function(x) NAs = sum(is.na(x))) )
msg_values <- gather(msg_value, key = "Feature", value = "Missing Values Count")
msg_values <- msg_values[msg_values$`Missing Values Count` > 0, ]
View(msg_values)  # No Missing Values

#########################################################################################################
################################### Feature Standardisation    ##########################################
#########################################################################################################

employee$salary_ratio <- scale(employee$salary_ratio)
employee$MonthlyIncome <- scale(employee$MonthlyIncome)
employee$DistanceFromHome2 <- scale(employee$DistanceFromHome)
employee$PercentSalaryHike2 <- scale(employee$PercentSalaryHike)
employee$leaves2 <- scale(employee$no_of_leaves)
employee$age2 <- scale(employee$Age)

## Create a Master2 Dataset
employee_master2 <- employee
#employee <- employee_master2

# converting target variable attrition from No/Yes character to factorwith levels 0/1 
employee$Attrition1 <- ifelse(employee$Attrition=="Yes",1,0)
employee$Gender1 <- ifelse(employee$Gender == "Male", 1, 0)
employee$PerformanceRating1 <- ifelse(employee$PerformanceRating == 3, 1, 0)
employee$MaritalStatus1 <- ifelse(employee$MaritalStatus == "Single", 1, 0) # Single - 1; Divorced/Married - 0

# creating a dataframe of categorical features
employee_chr<- employee[,c("YearsWithCurrManager1", "YearsSinceLastPromotion1", "StockOptionLevel1","YearsAtCompany1", "TotalWorkingYears1", "NumCompaniesWorked1", "avg_working_hrs", "JobInvolvement", "WorkLifeBalance", "JobSatisfaction", "EnvironmentSatisfaction", "TrainingTimesLastYear1","EducationField","Education","JobLevel","JobRole","BusinessTravel","Age1")]

# Creating a datafram with continous features
employee_glm <- employee[,c("Attrition1","salary_ratio","MaritalStatus1","Gender1","PerformanceRating1","DistanceFromHome2","leaves2","PercentSalaryHike2")]

# converting categorical attributes to factor
employee_fact<- data.frame(sapply(employee_chr, function(x) factor(x)))
str(employee_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_fact))[,-1]))

# Final dataset
str(employee_glm)
str(dummies)
employee_final<- cbind(employee_glm,dummies) 
str(employee_final) #4313 obs. of  80 variables

############################################################################################################
######################################## Model Building  ###################################################
############################################################################################################

# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_final$Attrition1, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]

######################################## Logistic Regression ############################################### 

#Initial model
model_1 = glm(Attrition1 ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2003.6....nullDev 2671.1...resDev 1863.6

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2) #AIC: 1963.8....nullDev 2671.1...resDev 1875.8

# Removing multicollinearity through VIF check
sort(vif(model_2))

# Model 3 - Excluding EducationField.xMedical (p - 2.14e-06, vif - 7.252197)
model_3 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsWithCurrManager1.x3.7.Years + YearsSinceLastPromotion1.x0.3.Years + 
                 YearsSinceLastPromotion1.x4.6.Years + YearsAtCompany1.x11.20.Years + 
                 YearsAtCompany1.x2.4.Years + YearsAtCompany1.x5.10.Years + 
                 TotalWorkingYears1.x1.2.Years + TotalWorkingYears1.x21.30.Years + 
                 TotalWorkingYears1.x3.5.Years + TotalWorkingYears1.x31.34.Years + 
                 NumCompaniesWorked1.x1.Company + NumCompaniesWorked1.x2.4.Companies + 
                 avg_working_hrs.xovertime + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                 WorkLifeBalance.x3 + WorkLifeBalance.x4 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 TrainingTimesLastYear1.x4.6.Trainings + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + Education.x2 + JobLevel.x3 + 
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)


summary(model_3) # AIC: 1983.4....nullDev 2671.1...resDev 1897.4
sort(vif(model_3))

# Model 4 - Excluding BusinessTravel.xTravel_Rarely  # p - value : 0.000874, VIF 3.942162
model_4 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsWithCurrManager1.x3.7.Years + YearsSinceLastPromotion1.x0.3.Years + 
                 YearsSinceLastPromotion1.x4.6.Years + YearsAtCompany1.x11.20.Years + 
                 YearsAtCompany1.x2.4.Years + YearsAtCompany1.x5.10.Years + 
                 TotalWorkingYears1.x1.2.Years + TotalWorkingYears1.x21.30.Years + 
                 TotalWorkingYears1.x3.5.Years + TotalWorkingYears1.x31.34.Years + 
                 NumCompaniesWorked1.x1.Company + NumCompaniesWorked1.x2.4.Companies + 
                 avg_working_hrs.xovertime + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                 WorkLifeBalance.x3 + WorkLifeBalance.x4 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 TrainingTimesLastYear1.x4.6.Trainings + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + Education.x2 + JobLevel.x3 + 
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + BusinessTravel.xTravel_Frequently + 
                 Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)

summary(model_4) # AIC: 1993.9....nullDev 2671.1...resDev 1909.9
sort(vif(model_4))

# Model 5 - Excluding YearsAtCompany1.x5.10.Years # p - value : 0.000501, vif - 3.600399
model_5 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsWithCurrManager1.x3.7.Years + YearsSinceLastPromotion1.x0.3.Years + 
                 YearsSinceLastPromotion1.x4.6.Years + YearsAtCompany1.x11.20.Years + 
                 YearsAtCompany1.x2.4.Years + 
                 TotalWorkingYears1.x1.2.Years + TotalWorkingYears1.x21.30.Years + 
                 TotalWorkingYears1.x3.5.Years + TotalWorkingYears1.x31.34.Years + 
                 NumCompaniesWorked1.x1.Company + NumCompaniesWorked1.x2.4.Companies + 
                 avg_working_hrs.xovertime + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                 WorkLifeBalance.x3 + WorkLifeBalance.x4 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 TrainingTimesLastYear1.x4.6.Trainings + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + Education.x2 + JobLevel.x3 + 
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + BusinessTravel.xTravel_Frequently + 
                 Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)

summary(model_5) # AIC: 2003.9....nullDev 2671.1...resDev 1921.9
sort(vif(model_5))

# Model 6 - Excluding WorkLifeBalance.x4  # p - value : 2.71e-05 
model_6 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsWithCurrManager1.x3.7.Years + YearsSinceLastPromotion1.x0.3.Years + 
                 YearsSinceLastPromotion1.x4.6.Years + YearsAtCompany1.x11.20.Years + 
                 YearsAtCompany1.x2.4.Years + 
                 TotalWorkingYears1.x1.2.Years + TotalWorkingYears1.x21.30.Years + 
                 TotalWorkingYears1.x3.5.Years + TotalWorkingYears1.x31.34.Years + 
                 NumCompaniesWorked1.x1.Company + NumCompaniesWorked1.x2.4.Companies + 
                 avg_working_hrs.xovertime + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                 WorkLifeBalance.x3 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 TrainingTimesLastYear1.x4.6.Trainings + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + Education.x2 + JobLevel.x3 + 
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + BusinessTravel.xTravel_Frequently + 
                 Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)

summary(model_6) # AIC: 2019.6....nullDev 2671.1...resDev 1939.6
sort(vif(model_6))

# Model 7 - Excluding YearsWithCurrManager1.x3.7.Years # p - value : 0.980232 
model_7 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsSinceLastPromotion1.x0.3.Years + 
                 YearsSinceLastPromotion1.x4.6.Years + YearsAtCompany1.x11.20.Years + 
                 YearsAtCompany1.x2.4.Years + 
                 TotalWorkingYears1.x1.2.Years + TotalWorkingYears1.x21.30.Years + 
                 TotalWorkingYears1.x3.5.Years + TotalWorkingYears1.x31.34.Years + 
                 NumCompaniesWorked1.x1.Company + NumCompaniesWorked1.x2.4.Companies + 
                 avg_working_hrs.xovertime + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                 WorkLifeBalance.x3 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 TrainingTimesLastYear1.x4.6.Trainings + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + Education.x2 + JobLevel.x3 + 
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + BusinessTravel.xTravel_Frequently + 
                 Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)

summary(model_7) # AIC: 2017.6....nullDev 2671.1...resDev 1939.6
sort(vif(model_7))

# Model 8 - Excluding YearsAtCompany1.x2.4.Years # p - value : 0.658277
model_8 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsSinceLastPromotion1.x0.3.Years + 
                 YearsSinceLastPromotion1.x4.6.Years + YearsAtCompany1.x11.20.Years + 
                 TotalWorkingYears1.x1.2.Years + TotalWorkingYears1.x21.30.Years + 
                 TotalWorkingYears1.x3.5.Years + TotalWorkingYears1.x31.34.Years + 
                 NumCompaniesWorked1.x1.Company + NumCompaniesWorked1.x2.4.Companies + 
                 avg_working_hrs.xovertime + JobInvolvement.x3 + WorkLifeBalance.x2 + 
                 WorkLifeBalance.x3 + JobSatisfaction.x2 + 
                 JobSatisfaction.x3 + JobSatisfaction.x4 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 TrainingTimesLastYear1.x4.6.Trainings + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + Education.x2 + JobLevel.x3 + 
                 JobLevel.x4 + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + BusinessTravel.xTravel_Frequently + 
                 Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)

summary(model_8) # AIC: 2015.8....nullDev 2671.1...resDev 1939.8
sort(vif(model_8))

# Model 9 - Excluding EducationField.xLife.Sciences # p - value : 0.325963
model_9 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                 YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                 YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                 TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                 TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                 NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                 JobInvolvement.x3 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x2 + JobLevel.x3 + JobLevel.x4 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
               family = "binomial", data = train)
  
summary(model_9) # AIC: 2014.8....nullDev 2671.1...resDev 1940.8
sort(vif(model_9))

# Model 10 - Excluding EducationField.xMarketing # p - value : 0.143448
model_10 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  JobInvolvement.x3 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x3 + JobLevel.x4 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_10) # AIC: 2015....nullDev 2671.1...resDev 1943.0
sort(vif(model_10))

# Model 11 - Excluding JobRole.xSales.Executive # p - value : 0.14082
model_11 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  JobInvolvement.x3 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x3 + JobLevel.x4 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_11) # AIC: 2015.1...nullDev 2671.1...resDev 1945.1
sort(vif(model_11))

# Model 12 - Excluding JobRole.xResearch.Scientist  # p - value : 0.148583
model_12 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  JobInvolvement.x3 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x3 + JobLevel.x4 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_12) # AIC: 2015.2...nullDev 2671.1...resDev 1947.2
sort(vif(model_12))

# Model 13 - Excluding JobLevel.x4 # p - value : 0.133399
model_13 <- glm(formula = Attrition1 ~ MaritalStatus1 + PerformanceRating1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  JobInvolvement.x3 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x3 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_13) # AIC: 2015.6...nullDev 2671.1...resDev 1949.6
sort(vif(model_13))

# Model 14 - Excluding PerformanceRating1 # p - value : 0.110247
model_14 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  JobInvolvement.x3 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x3 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)

summary(model_14) # AIC: 2016.1...nullDev 2671.1...resDev 1952.1
sort(vif(model_14))

# Model 15 - Excluding JobInvolvement.x3 # p - value : 0.083263
model_15 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x3 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)

summary(model_15) # AIC: 2016.4...nullDev 2671.1...resDev 1954.4
sort(vif(model_15))

# Model 16 - Excluding JobLevel.x3 # p - value : 0.103023
model_16 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + EducationField.xTechnical.Degree + 
                  Education.x2 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_16) # AIC: 2017.1...nullDev 2671.1...resDev 1957.1
sort(vif(model_16))

# Model 17 - Excluding EducationField.xTechnical.Degree   # p - value : 0.063947
model_17 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  TotalWorkingYears1.x31.34.Years + NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + 
                  Education.x2 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)

  
summary(model_17) # AIC: 2018.8...nullDev 2671.1...resDev 1960.8
sort(vif(model_17))

#############################################################################################
# Model 18 - Excluding TotalWorkingYears1.x31.34.Years # p - value : 0.029795
model_18 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + 
                  Education.x2 + JobLevel.x5 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_18) # AIC: 2022.5...nullDev 2671.1...resDev 1966.5
sort(vif(model_18))

# Model 19 - Excluding JobLevel.x5 # p - value : 0.024458
model_19 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + 
                  Education.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_19) # AIC: 2026.1...nullDev 2671.1...resDev 1972.1
sort(vif(model_19))

# Model 20 - Excluding Education.x2 # p - value : 0.023441
model_20 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + YearsSinceLastPromotion1.x4.6.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)

  
summary(model_20) # AIC: 2029.2...nullDev 2671.1...resDev 1977.2
sort(vif(model_20))

# Model 21 - Excluding YearsSinceLastPromotion1.x4.6.Years  # p - value : 0.013974 
model_21 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  EducationField.xOther + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)

  
summary(model_21) # AIC: 2033.5...nullDev 2671.1...resDev 1983.5 
sort(vif(model_21))

# Model 22 - Excluding EducationField.xOther  # p - value : 0.016187 
model_22 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_22) # AIC: 2038...nullDev 2671.1...resDev 1990
sort(vif(model_22))

# Model 23 - Excluding WorkLifeBalance.x2 # p - value : 0.01617 
model_23 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + 
                  YearsAtCompany1.x11.20.Years + TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_23) # AIC: 2041.7...nullDev 2671.1...resDev 1995.7
sort(vif(model_23))

# Model 24 - Excluding YearsAtCompany1.x11.20.Years # p - value : 0.004968 
model_24 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + 
                  TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_24) # AIC: 2048.5...nullDev 2671.1...resDev 2004.5
sort(vif(model_24))

# Model 25 - Excluding JobRole.xManufacturing.Director # p - value : 0.000809 
model_25 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + 
                  TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + TrainingTimesLastYear1.x4.6.Trainings + 
                  JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_25) # AIC: 2054.8...nullDev 2671.1...resDev 2012.8
sort(vif(model_25))


# Model 26 - Excluding TrainingTimesLastYear1.x4.6.Trainings # p - value : 0.001755 
model_26 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  YearsSinceLastPromotion1.x0.3.Years + 
                  TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +  + 
                  JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_26) # AIC: 2063.2...nullDev 2671.1...resDev 2023.2
sort(vif(model_26))

#############################################################################################

# Model 27 - Excluding YearsSinceLastPromotion1.x0.3.Years # p - value : 0.002534 
model_27 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x21.30.Years + TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +  + 
                  JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)
  
summary(model_27) # AIC: 2070...nullDev 2671.1...resDev 2032
sort(vif(model_27))

# Model 28 - Excluding TotalWorkingYears1.x21.30.Years # p - value : 0.005649 
model_28 <- glm(formula = Attrition1 ~ MaritalStatus1 + 
                  TotalWorkingYears1.x1.2.Years + 
                  TotalWorkingYears1.x3.5.Years + 
                  NumCompaniesWorked1.x1.Company + 
                  NumCompaniesWorked1.x2.4.Companies + avg_working_hrs.xovertime + 
                  WorkLifeBalance.x3 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +  + 
                  JobRole.xResearch.Director + 
                  BusinessTravel.xTravel_Frequently + Age1.x18.25.Years + Age1.x26.31.Years, 
                family = "binomial", data = train)

summary(model_28) # AIC: 2076.7...nullDev 2671.1...resDev 2040.7
sort(vif(model_28))

anova(model_28,test="Chisq")


######################
##### Final Model

final_model <- model_28
# model_17 with *,**,*** with intercept p-value insignificant i.e. >0.05
# model_26 with **,*** with intercept p-value is significant i.e <0.05

####Final Model
###########################
# logit(probability of employee churn) = -0.98 + 0.8498*(MaritalStatus1)+2.2134*(TotalWorkingYears1.x1.2.Years)
# +0.6663*(TotalWorkingYears1.x3.5.Years)-1.4*(NumCompaniesWorked1.x1.Company)
# -0.9321*(NumCompaniesWorked1.x2.4.Companies)+1.5055*(avg_working_hrs.xovertime)-0.4323(WorkLifeBalance.X3)
# -0.7160*(JobSatisfaction.x2) -0.6470*(JobSatisfaction.x3)-1.1530*(JobSatisfaction.x4)
# -0.6981*(EnvironmentSatisfaction.x2)-0.9217(EnvironmentSatisfaction.x3)-1.1413*(EnvironmentSatisfaction.x4)
# +0.7905*(JobRole.xResearch.Director)+  0.8714*(BusinessTravel.xTravel_Frequently)
# +0.9025*Age1.x18.25.Years+0.7293*Age1.x26.31.Years
#############################################################################################
################################### MODEL VALIDATION ########################################
#############################################################################################

### Test Data ####

#predicted probabilities of attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

test_pred_a = predict(final_model, type = "response", 
                      newdata = train[,-1])

train1 = train
# Let's see the summary 

summary(test_pred)
summary(test_pred_a)


test$prob <- test_pred
train1$prob <- test_pred_a
#View(test)

#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.4, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition1==1,"Yes","No"))
train_actual_attrition <- factor(ifelse(train1$Attrition1==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

install.packages("caret")
#library(e1071)
library(caret)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

perform_fn1 <- function(cutoff) 
{
  predicted_attrition1 <- factor(ifelse(test_pred_a >= cutoff, "Yes", "No"))
  conf1 <- confusionMatrix(predicted_attrition1, train_actual_attrition, positive = "Yes")
  acc1 <- conf1$overall[1]
  sens1 <- conf1$byClass[1]
  spec1 <- conf1$byClass[2]
  out1 <- t(as.matrix(c(sens1, spec1, acc1))) 
  colnames(out1) <- c("sensitivity", "specificity", "accuracy")
  return(out1)
}
# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)
OUT1 = matrix(0,100,3)
colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
colnames(OUT1) <- c("sensitivity", "specificity", "accuracy")


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
  OUT1[i,] = perform_fn1(s[i])
} 

nrow(test)
nrow(train)

AUC = matrix(0,100,2)
AUC1 = matrix(0,100,2)
colnames(AUC) <- c("sensitivity", "specificity")
colnames(AUC1) <- c("sensitivity", "specificity")


for(i in 1:100)
{ 
  AUC[i,1] = OUT[i,1]
  AUC[i,2] = 1-OUT[i,2]
} 

for(i in 1:100)
{ 
  AUC1[i,1] = OUT1[i,1]
  AUC1[i,2] = 1-OUT1[i,2]
} 

par(mar = rep(2, 4))
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=2),seq(0,1,length=2),cex.lab=1.5)
axis(2,seq(0,1,length=2),seq(0,1,length=2),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.65,0.6,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#out1[,3]=abs(OUT[,1]-OUT[,2])
#a=min(out1[,3])
#s[which(out1[,3] == a)]

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)] # 0.14566 is cut-off
cutoff1 <- s[which(abs(OUT1[,1]-OUT1[,2])<0.01)] # 0.153 is cut-off


# Let's choose a cutoff value of 0.14566 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))
train_cutoff_attrition <- factor(ifelse(test_pred_a >=cutoff, "Yes", "No"))


conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

conf_final1 <- confusionMatrix(train_cutoff_attrition, train_actual_attrition, positive = "Yes")

acc1 <- conf_final1$overall[1]

sens1 <- conf_final1$byClass[1]

spec1 <- conf_final1$byClass[2]

acc1  #76% Accuracy

sens1 #75.6% Sensitivity

spec1  #76.5% Specificity

acc
sens
spec

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

train_cutoff_attrition <- ifelse(train_cutoff_attrition=="Yes",1,0)
train_actual_attrition <- ifelse(train_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

#Plotting AUC Curve & AUC Value
plot(performance_measures_test,col="black",lty=3, lwd=3)
auc <- performance(pred_object_test, "auc") # Area under curve 
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")
   # 0.76 is area under curve for test dataset

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #52% KS-Statistic which is good (>40%) 

ks_table_test


pred_object_test1<- prediction(train_cutoff_attrition, train_actual_attrition)

performance_measures_test1<- performance(pred_object_test1, "tpr", "fpr")

ks_table_test1 <- attr(performance_measures_test1, "y.values")[[1]] - 
  (attr(performance_measures_test1, "x.values")[[1]])

max(ks_table_test1)  #52% KS-Statistic which is good (>40%) 

ks_table_test1  #48% KS-Statistic
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile1 <- as.data.frame(Attrition_decile)
Attrition_decile1
ggplot(Attrition_decile1,aes(x = bucket, y = Gain)) + geom_line() + geom_point() + xlab("Decilie") + ylab("Gain%")
  # Top 30% of the employees of sorted probablities, we can find 73% of employees likely to leave org
  # Lift is 2.45 at 3rd decilie which mean model catches 2.45 times more churns/leaving organisation than a random model

Attrition_decile2 = lift(train_actual_attrition, test_pred_a, groups = 10)
Attrition_decile3 <- as.data.frame(Attrition_decile2)
Attrition_decile3


# ROC Curve for Test and Training datasets
AUC = data.frame(AUC)
AUC1 = data.frame(AUC1)

ggplot(AUC, aes(x=specificity,y=sensitivity))+geom_line()+xlab("1-Specificity")
ggplot(AUC1, aes(x=specificity,y=sensitivity))+geom_line()+xlab("1-Specificity")



#Plotting AUC Curve & AUC Value for train dataset
plot(performance_measures_test1,col="black",lty=3, lwd=3)
auc <- performance(pred_object_test1, "auc") # Area under curve 
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.3,0.6,c(minauct,maxauct,"\n"),border="white",cex=1.7,box.col = "white")
  # 0.74 is area under curve for test dataset


anova(model_28,test="Chisq") 
summary(model_28)
