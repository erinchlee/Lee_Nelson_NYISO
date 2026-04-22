#set up session
library(here)
#set working directory
setwd("/home/guest/ENV872/final_project")
#check working directory
here()
#returns "/home/guest/ENV872/final_project"
#load in packages
library(tidyverse); library(zoo); library(trend); library(lubridate)
library(rvest); library(purrr); library(readr)

#modify this code skeleton to import temperature files in bulk
all_temp_files <- list.files(
  path = here("temp_data"),
  pattern = "temp.csv$",
  full.names = TRUE
)

# Read temperature files
list_temp_files <- lapply(all_temp_files, read.csv, header = TRUE)

#web scrape to obtain the NYISO load files for 2025, one per day
url <- "https://mis.nyiso.com/public/P-58Blist.htm"

# Step 1: scrape links
library(xml2)
links <- read_html(url) %>%
  html_elements("a") %>%
  html_attr("href") %>%
  url_absolute("https://mis.nyiso.com") %>%
  .[grepl("2025", .) & grepl("\\.zip$", .)]

# Step 4: create folder
dir.create("load_2025", showWarnings = FALSE)

# Step 5: download files
for(i in seq_along(links)){
  download.file(
    links[i],
    destfile = file.path("load_2025", basename(links[i])),
    mode = "wb"
  )
}

# Step 6: unzip
zip_files <- list.files("load_2025", pattern = "\\.zip$", full.names = TRUE)
for(z in zip_files){
  unzip(z, exdir = "load_2025")
}

# Step 7: read CSVs
csv_files <- list.files("load_2025", pattern = "\\.csv$", full.names = TRUE)
load_data <- map_dfr(csv_files, read_csv)

#dec1 <- read.csv(
  #file = here("temp_data/20251201pal.csv"),
  #stringsAsFactors = TRUE)
#filter for NYC load only
dec1_clean <- dec1 %>%
  filter(Name == "N.Y.C.")
#this time stamped data is for every 5 seconds. should we delete some and keep hourly time stamps?
#just because every 5s is a lot of data


