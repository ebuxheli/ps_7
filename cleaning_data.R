# attaching libraries
library(tidyverse)
library(fs)
library(dplyr)
library(stringr)
library(ggplot2)

# ### TEMP COMMENT SO FILES AREN'T REDOWNLOADED
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


## looking at the file party vs the response for the house election to check for
## any flips in response. 
flipped_hou <- upshot_hou %>% 
  select(source, file_party, response) %>% 
  mutate(response = recode(response, 
                            "Dem" = "Democratic",
                            "Rep" = "Republican",
                            "Und" = "Other"),
         flip = case_when((file_party == "Republican" & response == "Democratic") ~ TRUE,
                          (file_party == "Democratic" & response == "Republican") ~ TRUE)) %>% 
  group_by(source, file_party) %>% 
  count(flip) %>% 
  group_by(source) %>% 
  mutate(total = sum(n),
         state = toupper(str_sub(source, 1, 2)),
         district = paste0(state, "-", parse_number(str_sub(source, 2, 4))),
         wave = str_sub(source, -1, -1)) %>% 
  group_by(source, file_party) %>% 
  mutate(rep_per = n / total) %>% 
  filter(flip == TRUE) 

## looking at the file party vs the response for the senate election to check for
## any flips in response. 
flipped_sen <- upshot_sen %>% 
  select(source, file_party, response) %>% 
  mutate(response = recode(response, 
                           "Dem" = "Democratic",
                           "Rep" = "Republican",
                           "Und" = "Other"),
         flip = case_when((file_party == "Republican" & response == "Democratic") ~ TRUE,
                          (file_party == "Democratic" & response == "Republican") ~ TRUE)) %>% 
  group_by(source, file_party) %>% 
  count(flip) %>% 
  group_by(source) %>% 
  mutate(total = sum(n),
         state = toupper(str_sub(source, 1, 2)),
         wave = str_sub(source, -1, -1)) %>% 
  group_by(source, file_party) %>% 
  mutate(rep_per = n / total) %>%
  filter(flip == TRUE)

## looking at the file party vs the response for the governor election to check for
## any flips in response. 
flipped_gov <- upshot_gov %>% 
  select(source, file_party, response) %>% 
  mutate(response = recode(response, 
                           "Dem" = "Democratic",
                           "Rep" = "Republican",
                           "Und" = "Other"),
         flip = case_when((file_party == "Republican" & response == "Democratic") ~ TRUE,
                          (file_party == "Democratic" & response == "Republican") ~ TRUE)) %>% 
  group_by(source, file_party) %>% 
  count(flip) %>% 
  group_by(source) %>% 
  mutate(total = sum(n),
         state = toupper(str_sub(source, 1, 2)),
         wave = str_sub(source, -1, -1)) %>% 
  group_by(source, file_party) %>% 
  mutate(rep_per = n / total) %>%
  filter(flip == TRUE)

write_rds(flipped_hou, "visual_weighting/flips_hou.rds")
write_rds(flipped_sen, "visual_weighting/flips_sen.rds")
write_rds(flipped_gov, "visual_weighting/flips_gov.rds")


