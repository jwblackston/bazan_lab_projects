#####Wnt5, F1 ROR silencing
# by Walker Blackston
# created on: 1/13/21
# modified on: 1/25/21

library(tidyverse)
library(fs)

setwd("C:/Users/jbla12/Desktop/R Analyses")

data_dir <- "f1_ror_compare"
fs::dir_ls(data_dir)

files <-
  data_dir %>% 
  dir_ls(regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source")

#shape up the names 
names(files) <- make.names(names(files))

#clean up the data
trim_df <-
  subset(files, files$Intensity.Sum.Ch.3.Img.1 != "====================")

trim_df$source <- as.factor(trim_df$source)
trim_df$Intensity.Sum.Ch.3.Img.1 <- as.numeric(trim_df$Intensity.Sum.Ch.3.Img.1)


#Attempt 1: subset each by treatment, then try to reorganize data frame after 
######################NPD1
npd1 <- 
  trim_df[grep("f1 ror con npd1", trim_df$source),]

#use if-else to generate vars
npd1$num <- 
  ifelse(npd1$source == "f1_ror_compare/f1 ror con npd1 1 statistics_Intensity_Sum_Ch=3_Img=1.csv", 
         1, 
         ifelse(npd1$source == "f1_ror_compare/f1 ror con npd1 2 statistics_Intensity_Sum_Ch=3_Img=1.csv",
                2,
                ifelse(npd1$source == "f1_ror_compare/f1 ror con npd1 3 statistics_Intensity_Sum_Ch=3_Img=1.csv",
                       3,
                       ifelse(npd1$source == "f1_ror_compare/f1 ror con npd1 4 statistics_Intensity_Sum_Ch=3_Img=1.csv", 
                              4,
                              ifelse(npd1$source == "f1_ror_compare/f1 ror con npd1 5 statistics_Intensity_Sum_Ch=3_Img=1.csv", 
                                     5, 0)))))
#further data mgmt
npd1$num <- as.factor(npd1$num)               
npd1 <- subset(npd1, select = -c(source))
names(npd1) <- c("intensity", "exp_num")
#tag each row with this type for later merging
npd1$type <-
  ifelse(!is.na(npd1$intensity), "con_npd1", NA)

####################NPD1 wnt5a:
npd1_wnt5a <- 
  trim_df[grep("f1 ror con npd1 wnt5a", trim_df$source),]

#use if-else to generate vars
npd1_wnt5a$num <- 
  ifelse(npd1_wnt5a$source == "f1_ror_compare/f1 ror con npd1 wnt5a1 statistics_Intensity_Sum_Ch=3_Img=1.csv", 
         1, 
         ifelse(npd1_wnt5a$source == "f1_ror_compare/f1 ror con npd1 wnt5a2 statistics_Intensity_Sum_Ch=3_Img=1.csv",
                2,
                ifelse(npd1_wnt5a$source == "f1_ror_compare/f1 ror con npd1 wnt5a3 statistics_Intensity_Sum_Ch=3_Img=1.csv",
                       3,
                       ifelse(npd1_wnt5a$source == "f1_ror_compare/f1 ror con npd1 wnt5a4 statistics_Intensity_Sum_Ch=3_Img=1.csv", 
                              4,
                              ifelse(npd1_wnt5a$source == "f1_ror_compare/f1 ror con npd1 wnt5a5 statistics_Intensity_Sum_Ch=3_Img=1.csv", 
                                     5, 0)))))
#further data mgmt
npd1_wnt5a$num <- as.factor(npd1_wnt5a$num)               
npd1_wnt5a <- subset(npd1_wnt5a, select = -c(source))
names(npd1_wnt5a) <- c("intensity", "exp_num")
#tag each row with this type for later merging
npd1_wnt5a$type <-
  ifelse(!is.na(npd1_wnt5a$intensity), "npd1_wnt5a", NA)



#source = as.numeric(sub("\\D*(\\d{2}).*", "\\1", source))
#source = trim_df$source
#source <- stringr::str_extract(source, "\\d{2}")
#new_source = str_extract_all(source, "\\d", simplify = T)

########can we wrap this into a function that only varies certain variables???
#what varies? 1) the df going in, 2) the specific names of the intensity data (comp), 3) and the categorization of that dataset (out_cat)
#4) the source path needs specification (path)

from_folder_csv_to_clean_df <- function(dir, text_id, out_cat, out_name){
  #1)set directories 
  require(fs)
  data_dir <- as.character(dir)
  fs::dir_ls(data_dir)
  
  files <-
    data_dir %>% 
    dir_ls(regexp = "\\.csv$") %>% 
    map_dfr(read_csv, .id = "source")
  
  #shape up the names 
  names(files) <- make.names(names(files))
  
  #2)subset data and trim
  messy_df <-
    subset(files, files$Intensity.Sum.Ch.3.Img.1 != "====================")
  
  messy_df$source <- as.factor(messy_df$source)
  messy_df$Intensity.Sum.Ch.3.Img.1 <- as.numeric(messy_df$Intensity.Sum.Ch.3.Img.1)
  
  #3)specify the out df from parameter text_id (the phrase which directly identifies DF)
  less_messy_df <- 
    messy_df[grep(text_id, messy_df$source),]
  
  #reads in the df passed to messy_df if not already in DF form
  less_messy_df <- as.data.frame(less_messy_df)
  source <- less_messy_df$source
  new_source <- as.data.frame(str_extract_all(source, "\\d", simplify = T))
  new_source <- subset(new_source, select = c(V5))
  
  #4)remove verbose source/file path:
  less_messy_df <- subset(less_messy_df, select = -c(source))
  
  #lagniappe/ data management
  almost_clean_df <- cbind(less_messy_df, new_source)
  almost_clean_df$intensity <-
    almost_clean_df$Intensity.Sum.Ch.3.Img.1
  almost_clean_df <-
    almost_clean_df[complete.cases(almost_clean_df), ]
  almost_clean_df$replicate_ID <-
    almost_clean_df$V5
  almost_clean_df <- subset(almost_clean_df, select = -c(V5))
    
  clean_df <-
    subset(almost_clean_df, select = -c(Intensity.Sum.Ch.3.Img.1))
  
  #5)finish cleaning and tag for type
  clean_df$type <-
    ifelse(!is.na(clean_df$intensity), out_cat, NA)
  
  #6)print out the df and examine some of the data:
  out_df <<- clean_df
  out_name <<- out_df
}

#apply to the key ID's 
from_folder_csv_to_clean_df(dir = "f1_ror_compare", text_id = "f1 ror con npd1 wnt5a",
                            out_cat = "npd1_wnt5a", out_name = "npd1_wnt5a")

from_folder_csv_to_clean_df(dir = "f1_ror_compare", text_id = "f1 ror con npd1 wnt5a",
                            out_cat = "npd1_wnt5a")
