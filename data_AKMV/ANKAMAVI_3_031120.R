# ANGELA KATHERINE MARTIN VIVANCO
# (ANKAMAVI 9.11.2020)
# ------------------------------------
# 2. LOGISTIC Regression
# ====================================
# Data wrangling
# ====================================

library(readr)
math <- read_delim("data_AKMV/student-mat.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
por <- read_delim("data_AKMV/student-por.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

str(math)
dim(math)
#Classes ‘spec_tbl_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	395 obs. of  33 variables:

str(por)
dim(por)
#Classes ‘spec_tbl_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	649 obs. of  33 variables:

library(dplyr)
por_id <- por %>% mutate(id=1000+row_number()) 
math_id <- math %>% mutate(id=2000+row_number())

# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)

pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))

pormath <- por_id %>% 
  bind_rows(math_id) %>%
  # Aggregate data (more joining variables than in the example)  
  group_by(.dots=join_cols) %>%  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  # Remove lines that do not have exactly one obs from both datasets
  #   There must be exactly 2 observations found in order to joining be succesful
  #   In addition, 2 obs to be joined must be 1 from por and 1 from math
  #     (id:s differ more than max within one dataset (649 here))
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2, #pormath$alc_use<-(pormath$Dalc+pormath$Walc)/2
    high_use = alc_use > 2,
    cid=3000+row_number()
  )

# Save created data to folder 'data' as an Excel worksheet
library(openxlsx)
write.xlsx(pormath, file="pormath.xlsx")

str(pormath)
#Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	370 obs. of  51 variables

#comparing url mixed data set with the result of this script:
alc <- read_csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt")
str(alc)
write.xlsx(alc, file="alc.xlsx")

# 1.2 _Data analysis _  

## a) Relationships between high/low alcohol consumption and some of the other variables in the data.
library(tidyr); library(dplyr); library(ggplot2); library(gridExtra)

glimpse (alc)
a<-(gather(alc) %>% glimpse)

# Attach a ID number for each row and select only the columns-of-interest
data <- mutate(alc, ID = row_number()) %>%
  select(any_of(c("ID", "high_use", "absences", "health", "freetime", "famrel")))
# Let's see...
summary(data[-1])  # Ignore ID

# Plot with ggplot2, line chart for absences and bar charts for others
p1 <- ggplot(data, aes(absences)) + stat_count(geom="line", aes(colour=high_use) )
p2 <- ggplot(data, aes(health)) + geom_bar(aes(fill=high_use), position="dodge")
p3 <- ggplot(data, aes(freetime)) + geom_bar(aes(fill=high_use), position="dodge")
p4 <- ggplot(data, aes(famrel)) + geom_bar(aes(fill=high_use), position="dodge")

grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
