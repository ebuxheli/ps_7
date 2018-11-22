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


## looking at the file party vs the response for this election to check for
## any flips in response. TODO: calculate the percentages
upshot_hou %>% 
  select(file_party, response, state, wave, district) %>% 
  mutate(response = recode(response, 
                            "Dem" = "Democratic",
                            "Rep" = "Republican",
                            "Und" = "Other"),
         flip = case_when((file_party == "Republican" & response == "Democratic") ~ TRUE,
                          (file_party == "Democratic" & response == "Republican") ~ TRUE)) %>% 
  group_by(district, wave, file_party) %>% 
  count(flip) %>% 
  group_by(district) %>% 
  mutate(total = sum(n),
         vot_per = n / total) %>% 
  filter(flip == TRUE) %>% 
  ggplot(aes(x = district, y = vot_per, color = file_party)) + 
  geom_point()
  



