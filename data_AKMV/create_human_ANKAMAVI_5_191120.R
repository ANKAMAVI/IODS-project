# *4. Dimensionality reduction techniques - week 5*

date()

library(dplyr) 
library(corrplot)
library(GGally)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(tidyr)
library(MASS)

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

#______________________________-23.11
library(dplyr)
human <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt")
str(human)
str(human$GNI)
#library(stringr)
#str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric

variables <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(variables))
data.frame(human[-1], comp = complete.cases(human))

human_ <- filter(human, complete.cases(human)) #rows with NA values
#str(human_) #(human$Country)
human_ <- human[1:155, ]#(human_$Country)
rownames(human_) <- human_$Country
human_ <- select(human, -Country)

library(GGally)
ggpairs(human_)
str(human_)

write.csv(human_, "human.csv")

#_______________________
human <- read.csv("~/GitHub/IODS-project/IODS-project/data_AKMV/human2.txt")
pca_human<-prcomp(human2)#PCA
biplot(pca_human, choices = 1:2, cex = c(0.8, 1), col = c("grey40", "deeppink2"))


res.pca1 <- prcomp(human, center=TRUE, scale = TRUE)
res.pca1
summary(res.pca1)

fviz_pca_var(res.pca1, col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title="Not-standardized variables")

#PCA of standardized variables:
human_std <- scale(human)
pca_human <- prcomp(human_std)
biplot(pca_human,repel=TRUE,  choices = 1:2, cex = c(0.8, 1), col = c("grey40", "deeppink2"))


res.pca <- prcomp(human, center=TRUE, scale = TRUE)
res.pca
summary(res.pca)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)# Contributions of variables to PC2

fviz_pca_var(res.pca, col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title="Standardized variables")

#______________________________________


# 4.3 _MCA_ (MULTIPLE CORRESPONDENCE ANALYSIS)

data(tea)
str(tea)
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

tea_time2 <- select(tea, one_of(keep_columns))

tea_time2 <- select(tea, matches(keep_columns))
                    
str(tea_time2)
write.csv(tea_time, "tea_time.csv")

tea_time <- read.csv("~/GitHub/IODS-project/IODS-project/data_AKMV/tea_time.csv")

summary(tea_time)
str(tea_time)

gather(tea_time) %>% ggplot(aes(value)) + 
  facet_wrap("key", scales = "free") + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


mca <- MCA(tea_time, graph = TRUE)
summary(mca)
plot(mca, invisible=c("ind"), habillage = "quali")
plot(mca,repel=TRUE, habillage = "quali")

fviz_mca_var(mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_contrib(mca, choice = "var", axes = 1, top = 15)



################################26
tea_time3 <- dplyr::select(tea, one_of(keep_columns))

gather(tea_time3) %>% ggplot(aes(value)) + 
  facet_wrap("key", scales = "free") + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

mca <- MCA(tea_time3, graph = TRUE)
summary(mca)
#plot(mca, invisible=c("ind"), habillage = "quali")
#plot(mca,repel=TRUE, habillage = "quali")

fviz_mca_var(mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_contrib(mca, choice = "var", axes = 1, top = 15)
