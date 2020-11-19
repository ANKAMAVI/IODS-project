# *4. Dimensionality reduction techniques - week 5*

date()

library(dplyr)
library(corrplot)
library(ggplot2)
library(GGally)

#import
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#explore
str(hd)
str(gii)
summary(hd)
summary(gii)

#short names
colnames(hd)<-c('HDI_rank', 'country', 'HDI_index', 'life_exp', 'exp_edu', 'GNI', 'GNI_rank')
colnames(gii)<-c('GII_rank', 'country', 'GI_index', 'mat_mor', 'ado_birth', 'fem_parl', 'fem_edu','male_edu','fem_work','male_work')

#mutate gender inequality
gii<-mutate(gii, edu_ratio=fem_edu/male_edu)
gii<-mutate(gii, lab_ratio=fem_work/male_work)

human<-inner_join(hd, gii, by = c("country"))

write.csv(human, "human.csv")
                  