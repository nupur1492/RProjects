# Aim: Perform data wrangling on seattle traffic data using dplyr, plyr and tidyr packages and R pipelines

library(dplyr)
library(tidyr)
library(plyr)


df_seattle <- read.csv("Seattle_Collision_Data.csv")
str(df_seattle)

#Data pre-processing
# 1. Replacing blank cells with NA
# 2. Removing unwanted and descriptive variables
# 3. Removing variables with high number of missing values

df_seattle <- df_seattle %>%
              mutate_each(funs(replace(., . == "", NA))) %>%
              subset(., select = -c(INCKEY, COLDETKEY, DIAGRAMLINK, REPORTLINK, 
                                    REPORTNO, SDOTCOLNUM, SEGKEY, SPDCASENO))   %>%
              select(which(colMeans(is.na(.)) < 0.8))
dim(df_seattle)

df_seattle %>% tbl_df %>% glimpse()

# Separate shape into latitude and longitude
col_name <- 'Shape'
df_seattle <- separate(df_seattle, col = Shape, into = c("Latitude", "Longitude"), sep = ",")
str(df_seattle)

#Remove '(' and ')' from latitude and Longitude and convert to numerical value
df_seattle$Latitude <- gsub("[[:punct:]]","", df_seattle$Latitude)
df_seattle$Longitude <- gsub("[[:punct:]]","", df_seattle$Longitude)

df_seattle$Latitude <- as.numeric(df_seattle$Latitude)
df_seattle$Longitude <- as.numeric(df_seattle$Longitude)


# Count number of missing values in each column
missing_data <- df_seattle %>%
                sapply(function(x) sum(is.na(x)))
View(as.data.frame(missing_data))

missing_index <- which(is.na(df_seattle))
View(df_seattle[missing_index, ])

# there are 134612 rows where all the values are NA
df_seattle <- na.omit(df_seattle)
str(df_seattle)


# Summarizing data

# 1. using summary()
summary(df_seattle)

# 2. summary of numerical variables based on a categorical variables
summary2 <- ddply(df_seattle, .(ADDRTYPE), numcolwise(median))
summary2

# Summarize using dplyr
summary3 <- df_seattle %>%
            dplyr::group_by(ADDRTYPE, HITPARKEDCAR) %>%
            dplyr::summarise(Median_PersonCount = median(PERSONCOUNT),
                             Median_Distance = median(DISTANCE)) %>%
            dplyr::arrange(ADDRTYPE)
summary3
