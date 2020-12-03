# *6. GLM - week 7*

date()

library(dplyr) 
library(corrplot)
library(GGally)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(tidyr)
library(MASS)
library(readr)


rats <- read_table2("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt")
write.csv(rats, "rats.csv")
summary(rats)
str(rats)
#transformation rats->time



BPRS <- read_table2("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt")
write.csv(BPRS, "BPRS.csv")
summary(BPRS)
str(BPRS)

#transformation BPRS -> weeks
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
#.....
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))
glimpse(BPRSL)
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))








