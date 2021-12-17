library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)
library(skimr)
library(furniture)
library(kableExtra)
library(knitr)

#Read Data
setwd("~/BIOS625")
edu_data=as.data.frame(fread("data/Data_Long_Format.csv"))

skim(edu_data)

#COLOR BLIND FRIENDLY PALETTE
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#000000", "#0072B2", "#D55E00", "#CC79A7")

###PREPPING SCHOOL DATA
edu_data <- edu_data %>% group_by(Teacher_Education) %>% mutate(avg_math = mean(Math_Score)) %>% filter(is.na(Teacher_Education)==FALSE, is.na(Math_Score) ==FALSE, Math_Score != -9, Teacher_Education != -9, Parent1_Education != -9, Parent2_Education != -9)

#Grouping by school

edu_data_school <- edu_data %>% filter(is.na(School_ID)==FALSE, is.na(Math_Score) ==FALSE, Pub_Pri != -1) %>%
  group_by(School_ID)  %>% mutate(avg_math_school = mean(Math_Score))

edu_data_school$pub_pri = as.factor(edu_data_school$Pub_Pri)
levels(edu_data_school$pub_pri) <- c("Public","Private")

edu_data_school <- edu_data_school %>% mutate(pub_pris = ifelse(edu_data_school$Pub_Pri==1,"Public", "Private") ) 

test = data.frame(tchr_edu = unique(edu_data$Teacher_Education), avg_math = unique(edu_data$avg_math))

edu_data_school$Teacher_Education = as.factor(edu_data_school$Teacher_Education)
levels(edu_data_school$Teacher_Education) <- c("Bachelors Degree or Less", 
                                        "Master's Degree", "Advanced Degree")

#AVERAGE MATH SCHOOL BY YEAR 
edu_data_school <- edu_data_school %>% group_by(Dates) %>% 
  mutate(avg_math_yr = mean(Math_Score))

#RECODING SCHOOL YEAR TO REPRESENT GRADE LEVEL
edu_data_school$Dates = as.factor(edu_data_school$Dates)
levels(edu_data_school$Dates) <- c("K","1","2","3","4","5")

#AVERAGE MATH SCORE BY TEACHER EDUCUATION
edu_data_school <- edu_data_school %>% group_by(Teacher_Education, Dates) %>% 
  mutate(avg_math_yr_tchr = mean(Math_Score))

edu_data_school$Parent1_Education = as.factor(edu_data_school$Parent1_Education)
levels(edu_data_school$Parent1_Education) <- c("8th grade or less",
                                               "9-12th grade", 
                                               "HS Diploma/Equivalent",
                                               "Vocational/Technical School", 
                                               "Some College",
                                               "Bachelors Degree", "Graduate/Professional School", "Masters", "PhD or Professional Degree")
edu_data_school <- edu_data_school %>% group_by(Parent1_Education, Dates)%>%
  mutate(avg_math_yr_parent = mean(Math_Score))


#DIFFERENCE IN AVERAGE MATH SCHOOL
edu_data_school <- edu_data_school %>%
  group_by(Dates) %>%
  mutate(diff_math_avg = avg_math_yr - lag(avg_math_yr, default = 0))
##########################################################################################################################################################################################################################################

library(furniture)
library(knitr)
furniture::table1(edu_data,
                  "Math Score" = Math_Score,
                  "Teacher Education" = Teacher_Education, 
                  "Parent Education" = Parent1_Education, 
                  "School Type" = Pub_Pri, "Locale" = Locale,
                  test = TRUE,
                  na.rm = FALSE,
                  format_number = TRUE
) -> tab11


kable(tab11)














######################################################################################################################################
#PLOTTING VISUALIZTIONS
ggplot(edu_data) +
  geom_bar( aes(x = factor(Teacher_Education), y = avg_math), stat = "identity") +
  labs(x = "Teacher Education Level" , y = "Average Math Score (%)", title = "Average Math Score by Teacher Education Level")


#THANK YOU-- https://stackoverflow.com/questions/40249943/adding-percentage-labels-to-a-bar-chart-in-ggplot2
ggplot(test, aes(x = factor(tchr_edu), y = avg_math*100, label = scales::percent(avg_math))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5) +
  labs(x = "Teacher Education Level" , y = "Average Math Score (%)", title = "Average Math Score by Teacher Education Level") +
  theme_classic()

# Math Score by Teacher Experience Boxplot

ggplot(edu_data, aes(x = as.factor(Teacher_Education),y = Math_Score)) +
  geom_boxplot() +
  labs(x = "Teacher Education Level", y = "Theta Math Score", title = "Math Score by Teacher Education Level") +
  theme_classic()



#Public vs Private school
ggplot(edu_data, aes(x = factor(Teacher_Education), y = avg_math*100, col = Pub_Pri)) +
  geom_point() +
  labs(x = "Teacher Education Level" , y = "Average Math Score (%)", title = "Average Math Score by Teacher Education Level")

#Non-average
ggplot(edu_data, aes(x = factor(Teacher_Education), y = Math_Score, col = Pub_Pri)) +
  geom_point() +
  labs(x = "Teacher Education Level" , y = "Average Math Score (%)", title = "Average Math Score by Teacher Education Level")

#Math Scores over Time
ggplot(edu_data, aes(x = Dates, y = Math_Score, col = Pub_Pri)) +
  geom_point() +
  labs(x = "Datel" , y = "Average Math Score (%)", title = "Average Math Score over Time")




#Math Scores over Time


ggplot(edu_data_school, aes(x = Dates, y = avg_math_school)) +
  geom_point() +
  facet_wrap(as.factor(edu_data_school$pub_pri))+
  labs(x = "Date" , y = "Average Math Score (%)", title = "Average Math Score over Time") +
  theme_classic()



ggplot(edu_data_school, aes(x = Dates, y = avg_math_yr)) +
  geom_point() +
  stat_smooth()+
  facet_wrap(as.factor(edu_data_school$pub_pri))+
  labs(x = "Grade Level" , y = "Average Math Score (%)", title = "Average Math Score over Time") +
  theme_classic()

# BY TEACHER EDUCATION

ggplot(edu_data_school, aes(x = Dates, y = avg_math_yr_tchr, col =Teacher_Education)) +
  geom_point() +
  labs(x = "Grade Level" , y = "Theta Math Score", title = "Average Math Score over Time") +
  theme_classic()

# BY PARENT EDUCATION
#Parent Education 
#1 - 8th grade or less; 2- 9th-12th grade; 3- HS Diploma/Equivalent; 
#4 - Vocational/Technical School; 5 - Some College; 6 - Bachelors Degree
#7- Graduate/Professional School - No Degree; 8- Masters (MA,MS); 9- PhD or Pofessional Degree




ggplot(edu_data_school, aes(x = Dates, y = avg_math_yr_parent, col = Parent1_Education)) +
  geom_point() +
  labs(x = "Grade Level" , y = "Theta Math Score", title = "Average Math Score over Time") +
  theme_classic() +
  scale_colour_manual(values = cbp1)


ggplot(edu_data_school, aes(x = Dates, y = diff_math_avg)) +
  geom_point() +
  labs(x = "Grade Level" , y = "Difference in Theta Average Math Score", title = "Average Math Score over Time") +
  theme_classic() 



#ggplotly(math_score_time_plot)



#Teacher Education
#5 - Bachelors Degree or Less; 6 - Master's Degree; 7 - Advanced Professional Degree Beyond a Masters 
