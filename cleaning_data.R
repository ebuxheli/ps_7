# attaching libraries
library(tidyverse)
library(fs)
library(dplyr)

#### TEMP COMMENT SO FILES AREN'T REDOWNLOADED
# # getting upshot data
# download.file(url = "https://goo.gl/ZRCBda",
#               destfile = "upshot.zip",
#               quiet = TRUE,
#               mode = "wb")
# unzip("upshot.zip")
# 
# # deleting upshot.zip once data loaded
# file_delete("upshot.zip")


### consolidating upshot data (code taken from my midterm 2)
# listing all of the file names for upshot data
files_names <- dir_ls(path = "2018-live-poll-results-master/data/")

# reading in all the csv files and extracting source info (state, wave, etc. )
upshot <- map_dfr(files_names, read_csv, .id = "source") %>% 
  mutate(source    = str_sub(source, 51, -5),
         state     = toupper(str_sub(source, 1, 2)),
         wave      = str_sub(source, -1, -1),
         district  = paste0(state, "-", parse_number(str_sub(source, 2, 4))),
         office    = case_when(str_sub(source, 3, 3) == "g" ~ "Governor",
                               str_sub(source, 3, 3) == "s" ~ "Senate",
                               str_sub(source, 3, 3) != c("s", "g") ~ "House"))

# creating sets by office
upshot_sen <- filter(upshot, office == "Senate")
upshot_gov <- filter(upshot, office == "Governor")
upshot_hou <- filter(upshot, office == "House")


### observing which demographics are associated with a greater final weight
upshot_sen %>%
  ggplot2::ggplot(aes(x = ager, y = final_weight)) +
  ggplot2::geom_boxplot()

## observing how location effects weighting
upshot_sen %>% 
  ggplot2::ggplot(aes(x = ager, y = final_weight, color = district)) +
  ggplot2::geom_boxplot()

## calculate the spread of the weighting





