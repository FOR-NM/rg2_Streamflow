##==============================================================================
## Project: QuEST
## Script to merge same site PT sites in one file (using timestamp) for Santa Fe watershed
##==============================================================================

library(readxl) #to read excel 
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
#files <- list.files(path = "googledrive", full.names = TRUE)
#file.remove(files)

##########################
#### Import scan data ####
##########################
#### list and download all files in the folder ####
# this is the "formatted" folder
pt <- "data/PT/formatted/"
# list all CSV files in the folder
pt_files <- list.files(path = pt, pattern = "\\.csv$")


# create an empty list to store the cleaned data frames
pt_list <- lapply(seq_along(pt_files), function(i) {
  
  read.csv(paste0(pt, pt_files[i]), header = TRUE)
})

# assign names to the list elements based on the file names
names(pt_list) <- pt_files

#USF20 <- pt_list[["2024-10-24_USF20_WaterLevel.csv"]]


####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("USF24", "USF25", "USF40", "USF41")

# group files in `pt_list` by matching `site_names` in file names
pt_list_by_site <- lapply(site_names, function(site) {
  # names(pt_list) gives the names of all files in pt_list
  site_files <- names(pt_list)[grepl(site, names(pt_list))] 
  # grep checks if the current site (e.g., USF03) appears in each file name in pt_list 
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  pt_list[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(pt_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(pt_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

USF24 <- combined_by_site[["USF24"]]


##############################
#### Save combined files  ####
##############################

### there need to only be a 'formatted' folder, no need for different folders and file when formatting
## TO DO: fix this 

# write files to local data folder
#lapply(names(combined_by_site), function(site) {
  # define file path
  #file <- paste0("data/", site, ".csv")
  # save each data frame
  #write.csv(combined_by_site[[site]], file, row.names = FALSE, quote = FALSE)
  # this is the "merged_days" folder
#})
