library(tidyverse)
library(data.table)
#Read Data
dat=as.data.frame(fread("Imputed_Data.csv",sep=","))
#####Visuals######
library(ggplot2)
library(treemapify)
attach(dat)
par(mar = c(5.1,4.5,4.1,2.5))
#####School Type frequency table
ggplot(dat, aes(x = as.factor(Pub_Pri), fill = factor(Pub_Pri))) + geom_bar(width = 0.5) +
coord_flip() +
labs(title = "School Type Frequency",
subtitle = "Excluding missing data and those homeschooled",
x = "School Type",
y = "Frequency",
fill = "School Type") +
scale_x_discrete(labels = c("Public", "Private")) +
theme(legend.position = "top") +
scale_fill_discrete(labels = c("Public", "Private"))
#####Teacher Education frequency table
ggplot(dat, aes(x = as.factor(Teacher_Education), fill = factor(Teacher_Education))) + geom_bar(width = 0.5) +
coord_flip() +
labs(title = "Teacher Education Frequency",
subtitle = "Excluding missing data",
x = "Teacher Education (in years)",
y = "Frequency",
fill = "Teacher Education") +
theme(legend.position = "top") +
scale_fill_discrete(labels = c("5 yrs", "6 yrs", "7 yrs"))
#####Parent1 & Parent2 Education frequency table
ggplot(dat, aes(x = factor(Parent1_Education), fill = factor(Parent1_Education))) + geom_bar(width = 0.5) +
coord_flip() +
labs(title = "Parent Education Frequency",
subtitle = "Excluding missing data and Parent 2 data",
x = "Parent Level of Education",
y = "Frequency",
fill = "Level of Education") +
scale_x_discrete(labels = c("8th grade or less", "9th-12th grade", "High School", "Voc/Tech Program", "Some College", "Bachelor's Degree", "Grad/ Professional", "Master's Degree")) +
theme(legend.position = "top",
plot.margin = margin(r = 20, l = 20, t = 5, b = 5),
legend.spacing = unit(1.0, 'cm'),
legend.key.size = unit(1, 'cm')) +
scale_fill_discrete(labels = c("8th grade \n or less", "9th-12th grade", "High School", "Voc/Tech Program", "Some College", "Bachelor's Degree", "Grad/ Professional", "Master's Degree"))
#####Region Frequency Table
ggplot(dat, aes(x = factor(Locale), fill = factor(Locale))) +
geom_bar(width = 0.5) +
coord_flip() +
labs(title = "Locale",
subtitle = "Excluding missing data",
x = "Locale",
y = "Frequency",
fill = "Locale") +
theme(legend.position = "top") +
scale_fill_discrete(labels = c("City", "Suburb", "Town", "Rural"))

