# ANGELA KATHERINE MARTIN VIVANCO
# (ANKAMAVI)
# ------------------------------------
# 1. Regression and model validation
# ====================================
# Data wrangling
# ====================================
# 
# ───▄▀▀▀▄▄▄▄▄▄▄▀▀▀▄───
# ───█▒▒░░░░░░░░░▒▒█───
# ────█░░█░░░░░█░░█────
# ─▄▄──█░░░▀█▀░░░█──▄▄─
# █░░█─▀▄░░░░░░░▄▀─█░░█
# 
# date()
# [1] "Tue Nov 03 17:58:00 2020"
# -----------------------------------

# read the data into memory
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
str(lrn14)
# 'data.frame':	183 obs. of  60 variables:
# $ Aa      : int  3 2 4 4 3 4 4 3 2 3 ...
# $ Ab      : int  1 2 1 2 2 2 1 1 1 2 ...
dim(lrn14)
#[1] 183  60

#structure gives more information: variable name, type of variable, some values

# _____________________________________________________
# Scaling variables
#Create an analysis dataset with the variables 
#gender, age, attitude, 
#deep, stra, surf and points by combining questions in the learning2014 dat

# _____________________________________________________
library(dplyr)

lrn14$attitude<- lrn14$Attitude/10

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")
learning2014a <- select(lrn14, one_of(keep_columns))
str(learning2014a)
#selecting values greater than zero
learning2014 <- filter(learning2014a, Points > 0)
str(learning2014)


write.csv(learning2014, , row.names = F, file = "learning2014.csv")
data<-read.csv("learning2014.csv")
