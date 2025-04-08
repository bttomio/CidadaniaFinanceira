# UNZIP RAW FILES

# Specify the path to the zip file and the target directory
zip_file <- "zipfiles/Gaspar 2025.zip"
destination <- "zipfiles/Gaspar"

# Step 1: Create a temporary folder to extract files
temp_dir <- tempfile()
dir.create(temp_dir)

# Step 2: Unzip the contents into the temporary directory
unzip(zip_file, exdir = temp_dir)

# Step 3: List all files from the temporary directory, including subfolders
extracted_files <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)

# Step 4: Move all files to the destination directory
file.copy(extracted_files, destination)

# Step 5: Clean up by removing the temporary directory
unlink(temp_dir, recursive = TRUE)

# Check the files in the destination directory
list.files(destination)

folder_path <- "zipfiles/Gaspar"

# Step 1: List all the files in the folder
files <- list.files(folder_path, full.names = TRUE)

# Step 2: Remove "- Gaspar - " from each filename
new_names <- gsub("- Gaspar ", "", basename(files))

# Step 3: Combine the new filenames with the original directory path
new_paths <- file.path(folder_path, new_names)

# Step 4: Rename the files
file.rename(files, new_paths)

# Step 5: Check the renamed files
list.files(folder_path)
