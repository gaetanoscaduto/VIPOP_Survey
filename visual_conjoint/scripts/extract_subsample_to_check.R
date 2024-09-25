require(dplyr)
require(stringr)

# Set the paths to the source and destination folders

contexts=c("CZ", "IT","SW")
for(context in contexts)
{
  
source_folder <- paste0("C:/Users/GS_237332609/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/immagini_def/", context, "/")
destination_folder <- paste0("C:/Users/GS_237332609/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/immagini_check/", context, "/")

# Get a list of all files in the source folder
all_files <- list.files(source_folder, full.names = TRUE)

# Filter to only include image files (e.g., jpg, png)
image_files <- all_files %>%
  str_subset("\\.png$")

# Set the number of images to randomly select
num_images_to_select <- 250

# Randomly sample the images
selected_images <- sample(image_files, num_images_to_select)

# Copy the selected images to the destination folder
file.copy(selected_images, destination_folder, overwrite = TRUE)

cat("Selected images have been copied to the destination folder.\n")


}
