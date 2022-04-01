# This script assigns the Google Drive directory based on the user's info
# Path is saved as the drive_dir variable

# The user must already be in the find_google_drive_paths.txt table for it to work

# Import user table
users <- read.table("misc/find_google_drive_paths.txt",
                    sep = ",", header = T)

# Detect username
username = as.character(Sys.info()["user"])

# Set google drive folder (to be appended to path)
drive_folder = "American_Whitewater_LHDP"

# Set path based on user
drive_dir = paste0(subset(users, user == username)$drive_path,
                   drive_folder)