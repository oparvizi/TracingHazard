# Install packages if needed
install.packages(c("readxl", "dplyr"))

# Load libraries
library(readxl)
library(dplyr)

# Import Excel file
# Replace with your file path
df <- read_excel("your_file.xlsx")

# Function to summarize each column
summary_table <- data.frame(
  Column = names(df),
  Type = sapply(df, class),
  Min = sapply(df, function(x) {
    if (is.numeric(x) || is.integer(x)) {
      min(x, na.rm = TRUE)
    } else {
      NA
    }
  }),
  Max = sapply(df, function(x) {
    if (is.numeric(x) || is.integer(x)) {
      max(x, na.rm = TRUE)
    } else {
      NA
    }
  }),
  Missing_Values = sapply(df, function(x) sum(is.na(x)))
)

# Display result
print(summary_table)