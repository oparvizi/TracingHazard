# Load necessary libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(tools)

# Specify the path to the drive and the Excel file
drive_path <- "C:/Users/Pc/Desktop/NCBI_Data"
excel_file_path <- "C:/Users/Pc/Documents/ExcelAccession - Copy.xlsx"
new_excel_file_path <- "C:/Users/Pc/Documents/ExcelAccession_Updated.xlsx"

# Read filenames from the drive and strip out file extensions
file_names <- list.files(drive_path)
file_names_no_ext <- file_path_sans_ext(file_names)

# Read the Excel file into a data frame
excel_data <- read_excel(excel_file_path)

# Convert the ACCESSION column to character type
excel_data$ACCESSION <- as.character(excel_data$ACCESSION)

# Print the first few values in the ACCESSION column and file_names list without extensions
cat("First few values in ACCESSION column:\n")
print(head(excel_data$ACCESSION))
cat("First few values in file_names list without extensions:\n")
print(head(file_names_no_ext))

# Check for the presence of filenames (without extensions) in the "ACCESSION" column and remove related rows
filtered_data <- excel_data %>% filter(!ACCESSION %in% file_names_no_ext)

# Print the number of rows in the original data
original_rows <- nrow(excel_data)
cat("Number of rows in the original data:", original_rows, "\n")

# Print the number of rows in the filtered data
filtered_rows <- nrow(filtered_data)
cat("Number of rows in the filtered data:", filtered_rows, "\n")

# Write the updated data to the new Excel file
write.xlsx(filtered_data, new_excel_file_path)

cat("Rows with matching filenames have been successfully deleted from the Excel file.")
