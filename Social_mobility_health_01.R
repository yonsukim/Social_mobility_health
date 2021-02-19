
#####  Social mobility and self-rated health
#####  YKim Created - 2/19/21

install.packages("readstata13")
install.packages("rlang", dependencies = TRUE)
install.packages("readxl")
install.packages("tidyverse")

library(readstata13)
library("readxl")
# library(readxl)
library(dplyr)



## Import SPSS file

library(haven)
path = file.path("C:\\Users\\Dongwoo\\Documents\\Research\\Data Warehouse\\Social_Mobility" ,"filename.sav")
df0 = read_sav(path)

# View(df0)


df1 <- df0 %>%
  dplyr::select(YEAR) ## Case-sensitive

# head(df1)
