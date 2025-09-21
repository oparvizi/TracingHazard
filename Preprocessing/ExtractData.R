---
title: "FetchTXT"
output: html_document
date: "2025-02-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

---
  title: "FetchTXT"
output: html_document
date: "2024-12-01"
---

#_______________________________________________________________________________
#------------------------------Fetch NCBI---------------------------------------
#_______________________________________________________________________________
#Fetch accession, strain, Locus
```{r}
# Akrotiri and Dhekelia, Bahrain, Cyprus, Egypt, Iran, Israel, Jordan, Kuwait, Lebanon, Oman, Palestine, Qatar, Saudi Arabia, Syria, Turkey, United Arab Emirates, Yemen, Armenia, Azerbaijan, Turkmenistan, Kazakhstan, Russia, Afghanistan, Pakistan
# Influenza H9N2   Salmonella in sheep Pasteurella

library(shiny)
library(rentrez)
library(dplyr)
library(openxlsx)
library(httr)
library(leaflet)
library(stringr)
library(DT)

# Increase the timeout to 600 seconds
# options(timeout = 600)

# Shiny UI
ui <- fluidPage(
  #  tags$head(
  #    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
  #    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
  #    tags$style(type = "text/css", ".link-spacing {margin-right: 20px; /* Adjust the value as needed */ }")
  #  ),
  
  #  div(class = "top-right",
  #      tags$style(type = "text/css", ".top-left {position: absolute; top: 10px; left: 10px; z-index: 500; text-align: right;}"),
  #     tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Razi_.jpg/800px-Razi_.jpg", class = "logo", height = "100px"),
  #      tags$a("About this tool", href="https://xxxx", class = "link-spacing"),
  #      tags$a("Analysis", href="https://xxxx")
  #  ),
  titlePanel("Pathogens Tracking as a Target for Novel Vaccine"),
  sidebarLayout(
    sidebarPanel(
      textInput("keywords", "Enter Keywords:", placeholder = "Pasteurella in sheep"),
      textInput("countries", "Enter Countries (comma-separated):", placeholder = "Iran, UK, Germany"),
      actionButton("search", "Search"),
      downloadButton("downloadCSV", "Download CSV"),
    ),
    mainPanel(
      DTOutput("results")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  # Function to fetch data in batches
  fetch_in_batches <- function(ids, batch_size = 300) {
    all_records <- list()
    for (i in seq(1, length(ids), by = batch_size)) {
      batch_ids <- ids[i:min(i + batch_size - 1, length(ids))]
      records <- tryCatch({
        entrez_fetch(db = "nuccore", id = batch_ids, rettype = "gb", retmode = "text")
      }, error = function(e) {
        message("Error fetching batch: ", e)
        return(NULL)
      })
      if (!is.null(records)) {
        all_records <- c(all_records, records)
      }
    }
    return(all_records)
  }
  
  # Function to extract information
  extract_info <- function(records) {
    accessions <- str_extract_all(records, "ACCESSION\\s+([A-Z0-9_]+)")
    accessions <- unlist(lapply(accessions, function(x) if (length(x) > 0) str_match(x, "ACCESSION\\s+([A-Z0-9_]+)")[,2] else NA))
    
    strains <- str_extract_all(records, "/strain=\"([^\"]+)\"")
    strains <- unlist(lapply(strains, function(x) if (length(x) > 0) str_match(x, "/strain=\"([^\"]+)\"")[,2] else "Unknown"))
    
    # Ensure the lengths match
    max_length <- max(length(accessions),length(strains))
    accessions <- c(accessions, rep(NA, max_length - length(accessions)))  
    strains <- c(strains, rep(NA, max_length - length(strains)))
    
    data <- data.frame(
      Accession = accessions,      
      Strain = strains,
    )
    return(data)
  }
  
  # Event reactive function
  search_results <- eventReactive(input$search, {
    keywords <- input$keywords
    countries <- strsplit(input$countries, ",\\s*")[[1]]
    
    if (length(countries) == 0 || (length(countries) == 1 && countries == "")) {
      query <- keywords
    } else {
      query <- paste(keywords, paste(countries, collapse = " OR "), sep = " AND ")
    }
    
    page <- input$page
    retstart <- (page - 1) * 60000
    search <- entrez_search(db = "nuccore", term = query, retmax = 60000, retstart = retstart)#, config = httr::timeout(60))
    ids <- search$ids
    
    if (length(ids) == 0) {
      return(data.frame(Geo_Local_Name = character(), Latitude = numeric(), Longitude = numeric()))
    } else {
      all_records <- fetch_in_batches(ids)
      data <- extract_info(all_records)
      return(data)
    }
  }) 
  
  
  
  output$results <- renderDT({
    datatable(search_results(), selection = 'single')
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("Agent_in_", strsplit(input$countries, ",\\s*")[[1]], "_",  Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(search_results(), file)
    }
  )
  
  output$pagination <- renderUI({
    page <- input$page
    fluidRow(
      column(6, actionButton("prevPage", "Previous")),
      column(6, actionButton("nextPage", "Next"))
    )
  })
  
  observeEvent(input$prevPage, {
    if (input$page > 1) {
      updateNumericInput(session, "page", value = input$page - 1)
    }
  })
  
  observeEvent(input$nextPage, {
    updateNumericInput(session, "page", value = input$page + 1)
  })
}

shinyApp(ui = ui, server = server)


```



#Fetch NCBI 
```{r}
library(shiny)
library(rentrez)

ui <- fluidPage(
  titlePanel("NCBI Data Fetcher"),
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Enter search term:", "Dengue Fever Virus"),
      actionButton("search", "Search"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      verbatimTextOutput("results"),
      textOutput("status")
    )
  )
)

server <- function(input, output) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$search, {
    query <- input$query
    batch_size <- 99000
    all_data <- NULL
    
    output$status <- renderText("Searching...")
    
    tryCatch({
      search_results <- entrez_search(db="nucleotide", term=query, use_history=TRUE, retmax=batch_size)
      web_history <- search_results$web_history
      total_records <- min(search_results$count, 999000)
      
      txt_file_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop", paste("NCBI_data_", Sys.Date(), ".txt", sep=""))
      dat_file_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop", paste("NCBI_data_", Sys.Date(), ".dat", sep=""))
      
      txt_file_conn <- try(file(txt_file_path, open = "wt"), silent = TRUE)
      dat_file_conn <- try(file(dat_file_path, open = "wt"), silent = TRUE)
      
      if (inherits(txt_file_conn, "try-error") || inherits(dat_file_conn, "try-error")) {
        stop("Failed to open file connections.")
      }
      
      on.exit({
        close(txt_file_conn)
        close(dat_file_conn)
      }, add = TRUE)
      
      for (start in seq(0, total_records, by=batch_size)) {
        fetch_results <- entrez_fetch(db="nucleotide", web_history=web_history, retstart=start, retmax=batch_size, rettype="gb", retmode="text")
        all_data <- c(all_data, fetch_results)
        
        writeLines(fetch_results, txt_file_conn)
        writeLines(fetch_results, dat_file_conn)
        
        output$results <- renderText({ paste(fetch_results, collapse="\n") })
        
        for (char in strsplit(fetch_results, NULL)[[1]]) {
          cat(char)
          flush.console()
        }
        
        if (start + batch_size >= total_records) {
          output$status <- renderText("Fetch complete. All records have been retrieved. The files are ready to use.")
          cat("Data has been saved")
        } else {
          output$status <- renderText(paste("Fetching records", start, "to", start + batch_size, "of", total_records))
        }
      }
      
      data(all_data)
      
    }, error = function(e) {
      output$status <- renderText(paste("An error occurred:", e$message))
    })
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("NCBI_data_", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(data(), file, sep="\t", row.names=FALSE, col.names=FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)


```


#_______________________________________________________________________________
#------------------------------Split and Count----------------------------------
#_______________________________________________________________________________
# Read the Text File Partially
```{r}
# Specify the file path
file_path <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/outputMain_NCBI_Data_H9N2.txt"

# Read the first 100 lines
lines <- readLines(file_path)#, n = 100)

# Print the lines
print(lines)
```

# Split large file into smaller one
```{r}
# Function to split text at every 1000 occurrences of "//", excluding "http://" and "https://"
split_text <- function(file_path, split_size = 1000) {
  con <- file(file_path, "r")
  part_num <- 1
  buffer <- ""
  count <- 0
  
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    buffer <- paste0(buffer, line, "\n")
    
    # Count occurrences of "//" excluding "http://" and "https://"
    count <- count + length(gregexpr("(?<!http:|https:)//", line, perl = TRUE)[[1]])
    
    if (count >= split_size && grepl("^//\\s*$", line)) {
      # Ensure the buffer starts with "LOCUS" and ends with "//" followed by a blank line
      if (!grepl("^LOCUS", buffer)) {
        buffer <- sub(".*?(LOCUS)", "\\1", buffer)
      }
      buffer <- paste0(buffer, "\n")
      
      # Write the buffer to a new file
      new_file_path <- paste0("C:/Users/Pc/Desktop/RAZI/FetchNCBI/NCBI_data_2024-12-18_Clostr/split_file_", part_num, ".txt")
      writeLines(buffer, new_file_path)
      
      # Reset buffer and count
      buffer <- ""
      count <- 0
      part_num <- part_num + 1
    }
  }
  
  # Write any remaining buffer to a new file
  if (nchar(buffer) > 0) {
    new_file_path <- paste0("C:/Users/Pc/Desktop/RAZI/FetchNCBI/NCBI_data_2024-12-18_Clostr/split_file_", part_num, ".txt")
    writeLines(buffer, new_file_path)
  }
  
  close(con)
  print(paste("Split into", part_num, "files."))
}

# Specify the file path
file_path <- "C:/Users/Pc/Desktop/RAZI/FetchNCBI/NCBI_data_2024-12-18_Clostr.txt"

# Call the function to split the file
split_text(file_path)


```

#Count small file---------------------------------------------------------------
```{r}
# Install stringr package if not already installed
#install.packages("stringr")

# Load the stringr package
library(stringr)

# Function to count occurrences of '//' in a text file
count_double_slashes <- function(file_path) {
  # Read the file content
  content <- readLines(file_path)
  # Combine all lines into a single string
  combined_content <- paste(content, collapse = "\n")
  # Count occurrences of '//'
  count <- str_count(combined_content, "ACCESSION")
  return(count)
}

# Example usage
file_path <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/outputMain_NCBI_Data_H9N2.txt"
cat("The number of occurrences of '//' in the file is:", count_double_slashes(file_path))

```

#Count the lines or specific characters in a large text file without exceeding memory limits
```{r}
# Function to count lines and specific characters in a large text file
count_lines_and_characters <- function(file_path, target_char = "//") {
  con <- file(file_path, "r")
  total_lines <- 0
  target_char_lines <- 0
  
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    total_lines <- total_lines + 1
    if (trimws(line) == target_char) {
      target_char_lines <- target_char_lines + 1
    }
  }
  
  close(con)
  
  list(total_lines = total_lines, target_char_lines = target_char_lines)
}

# Specify the file path
file_path <- "C:/mydata/Desktop-PC/Fallah/NCBI_Merge/outputMain_NCBI_Data_H9N2.txt"

# Call the function to count lines and specific characters
result <- count_lines_and_characters(file_path)

# Print the results
print(paste("Total lines:", result$total_lines))
print(paste("Lines with only '", target_char, "':", result$target_char_lines))


```

#Get files from the folder
```{r}
# Specify the folder path
folder_path <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/outputMain_NCBI_Data_H9N2.txt"

# List all text files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize a list to store the content of each file
file_contents <- list()

# Loop through each file and read its content
for (file in file_list) {
  content <- readLines(file, warn = FALSE)
  file_contents[[file]] <- content
}

# Print the content of each file
for (file in names(file_contents)) {
  cat("Content of", file, ":\n")
  print(file_contents[[file]])
  cat("\n")
}

```

#_______________________________________________________________________________
#-------------------------Create Metadata---------------------------------------
#_______________________________________________________________________________
Main data and Date--------------------------------------------------------------
```{r}
# Read data from input file
input_file <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/merged_part_3.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")

# Replace https:// with a unique placeholder
#data <- gsub("https://", "PLACEHOLDER_HTTPS", data)
library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/"))

# Split the data into individual entries
entries <- strsplit(data, "//")[[1]]

# Initialize an empty data frame with all required columns
df <- data.frame(LOCUS=character(), DATE=character(), DEFINITION=character(), ACCESSION=character(), VERSION=character(), DBLINK=character(), KEYWORDS=character(), SOURCE=character(), stringsAsFactors=FALSE)

# Function to extract data from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(LOCUS="", DATE="", DEFINITION="", ACCESSION="", VERSION="", DBLINK="", KEYWORDS="", SOURCE="")
  for (line in lines) {
    if (grepl("^LOCUS", line)) {
      locus_info <- sub("^LOCUS\\s+", "", line)
      locus_info <- gsub("\\s+", " ", locus_info)
      data_list$LOCUS <- gsub(" ", "/", locus_info)
      # Extract date from LOCUS information
      date_match <- regexpr("\\d{2}-[A-Z]{3}-\\d{4}", locus_info)
      if (date_match != -1) {
        data_list$DATE <- regmatches(locus_info, date_match)
      }
    } else if (grepl("^DEFINITION", line)) {
      data_list$DEFINITION <- sub("^DEFINITION\\s+", "", line)
    } else if (grepl("^ACCESSION", line)) {
      data_list$ACCESSION <- sub("^ACCESSION\\s+", "", line)
    } else if (grepl("^VERSION", line)) {
      data_list$VERSION <- sub("^VERSION\\s+", "", line)
    } else if (grepl("^DBLINK", line)) {
      data_list$DBLINK <- sub("^DBLINK\\s+", "", line)
    } else if (grepl("^KEYWORDS", line)) {
      data_list$KEYWORDS <- sub("^KEYWORDS\\s+", "", line)
    } else if (grepl("^SOURCE", line)) {
      data_list$SOURCE <- sub("^SOURCE\\s+", "", line)
    }
  }
  return(data_list)
}

# Process each entry and add to the data frame
for (entry in entries) {
  entry_data <- extract_data(entry)
  df <- rbind(df, as.data.frame(entry_data, stringsAsFactors=FALSE))
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/Processed/MainData_Date/PasMain3.txt", sep="|", row.names=FALSE, quote=FALSE)
cat("Data has been saved to outputMAIN.txt\n")

```

Details Reference---------------------------------------------------------------
```{r}
# Read data from input file
input_file <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/merged_part_3.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")

# Replace https:// with a unique placeholder
#data <- gsub("https://", "PLACEHOLDER_HTTPS", data)
library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/"))

# Split the data into individual entries
entries <- strsplit(data, "//")[[1]]

# Initialize an empty data frame with the required columns
df <- data.frame(ACCESSION=character(), REFERENCE=character(), AUTHORS=character(), TITLE=character(), JOURNAL=character(), PUBMED=character(), stringsAsFactors=FALSE)

# Function to extract data from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(ACCESSION="", REFERENCE="", AUTHORS="", TITLE="", JOURNAL="", PUBMED="")
  reference_lines <- c()
  authors_lines <- c()
  title_lines <- c()
  journal_lines <- c()
  pubmed_lines <- c()
  
  for (line in lines) {
    if (grepl("^ACCESSION", line)) {
      data_list$ACCESSION <- sub("^ACCESSION\\s+", "", line)
      #print(paste("ACCESSION:", data_list$ACCESSION))
    } else if (grepl("^REFERENCE", line)) {
      reference_lines <- c(reference_lines, sub("^REFERENCE\\s+", "", line))
    } else if (grepl("^AUTHORS", line) || grepl("^\\s+AUTHORS\\s+", line)) {
      authors_lines <- c(authors_lines, sub("^\\s*AUTHORS\\s+", "", line))
      #print(paste("AUTHORS:", authors_lines))
    } else if (grepl("^TITLE", line) || grepl("^\\s+TITLE", line)) {
      title_lines <- c(title_lines, sub("^\\s*TITLE\\s+", "", line))
      #print(paste("TITLE:", title_lines))
    } else if (grepl("^JOURNAL", line) || grepl("^\\s+JOURNAL", line)) {
      journal_lines <- c(journal_lines, sub("^\\s*JOURNAL\\s+", "", line))
      #print(paste("JOURNAL:", journal_lines))
    } else if (grepl("^PUBMED", line) || grepl("^\\s+PUBMED", line)) {
      pubmed_lines <- c(pubmed_lines, sub("^\\s*PUBMED\\s+", "", line))
      #print(paste("PUBMED:", pubmed_lines))
    }
  }
  
  data_list$REFERENCE <- paste(reference_lines, collapse = "/")
  data_list$AUTHORS <- paste(authors_lines, collapse = "/")
  data_list$TITLE <- paste(title_lines, collapse = "/")
  data_list$JOURNAL <- paste(journal_lines, collapse = "/")
  data_list$PUBMED <- paste(pubmed_lines, collapse = "/")
  
  #print(paste("REFERENCE:", data_list$REFERENCE))
  #print(paste("AUTHORS:", data_list$AUTHORS))
  #print(paste("TITLE:", data_list$TITLE))
  #print(paste("JOURNAL:", data_list$JOURNAL))
  #print(paste("PUBMED:", data_list$PUBMED))
  
  return(data_list)
}

# Process each entry and add to the data frame
for (entry in entries) {
  if (nchar(trimws(entry)) > 0) {  # Skip empty entries
    entry_data <- extract_data(entry)
    df <- rbind(df, as.data.frame(entry_data, stringsAsFactors=FALSE))
  }
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/Processed/DetailsReference/PasREF3.txt", sep="|", row.names=FALSE, quote=FALSE)
cat("Data has been saved to outputREF.txt\n")

```

COMMENT-------------------------------------------------------------------------
```{r}
input_file <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/merged_part_1.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")

# Replace https:// with https:/
#data <- gsub("https://", "https:/", data)  
#data <- gsub("http://", "http:/", data)  
library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/"))

# Find lines containing "https://"
lines_with_https <- grep("https://", data, value = TRUE)
print(lines_with_https)

# Find lines containing "http://"
lines_with_https <- grep("http://", data, value = TRUE)
#print(lines_with_http)


# Split the data into individual entries
entries <- strsplit(data, "//")[[1]]

# Initialize an empty data frame with the required columns
df <- data.frame(ACCESSION=character(), COMMENT=character(), stringsAsFactors=FALSE)

# Function to extract ACCESSION and COMMENT from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(ACCESSION="", COMMENT="")
  
  current_field <- NULL
  for (line in lines) {
    if (grepl("^ACCESSION", line)) {
      data_list$ACCESSION <- sub("^ACCESSION\\s+", "", line)
    } else if (grepl("^COMMENT", line)) {
      current_field <- "COMMENT"
      data_list$COMMENT <- trimws(sub("^COMMENT\\s+", "", line))
    } else if (grepl("^FEATURES", line)) {
      current_field <- NULL
    } else if (!is.null(current_field)) {
      if (trimws(line) != "") {  # Skip blank lines
        data_list[[current_field]] <- paste(data_list[[current_field]], trimws(line), sep=" ")
      }
    }
  }
  return(data_list)
}

# Process each entry and add to the data frame
for (entry in entries) {
  entry_data <- extract_data(entry)
  df <- rbind(df, as.data.frame(entry_data, stringsAsFactors=FALSE))
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/Processed/COMMENT/PasCOM1.txt", sep="|", row.names=FALSE, quote=FALSE)
cat("Data has been saved to outputCOM.txt\n")

```

Features Source-----------------------------------------------------------------
```{r}
# Read data from input file
input_file <- "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/merged_part_1.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")

# Replace https:// with https:/
#data <- gsub("https://", "https:/", data)  
library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/")) 

# Find lines containing "https://"
lines_with_https <- grep("https://", data, value = TRUE)
#print(lines_with_https)

# Find lines containing "http://"
lines_with_https <- grep("http://", data, value = TRUE)
#print(lines_with_http)

# Extract the section between "FEATURES" and the subunit
section <- sub(".*FEATURES(.*?)(rRNA|gene|CDS|tRNA).*", "\\1", data, perl = TRUE)

# Split the section into individual entries
entries <- strsplit(section, "//")[[1]]

# Initialize an empty data frame with the required columns
df <- data.frame(ACCESSION=character(),  
                 ORGANISM=character(), MOL_TYPE=character(), 
                 STRAIN=character(), ISOLATION_SOURCE=character(), 
                 Locus=character(), DB_XREF=character(), 
                 GEO_LOC_NAME=character(), COLLECTION_DATE=character(), 
                 SUB_SPECIES=character(), LAT_LON=character(), 
                 COLLECTED_BY=character(), SUBMITTER_SEQID=character(), 
                 stringsAsFactors=FALSE)

# Function to extract data from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(ACCESSION="", ORGANISM="", MOL_TYPE="", 
                    STRAIN="", ISOLATION_SOURCE="", LOCUS="", DB_XREF="", 
                    GEO_LOC_NAME="", COLLECTION_DATE="", SUB_SPECIES="", 
                    LAT_LON="", COLLECTED_BY="", SUBMITTER_SEQID="")
  for (line in lines) {
    if (grepl("^ACCESSION", line)) {
      data_list$ACCESSION <- sub("^ACCESSION\\s+", "", line)
    }  else if (grepl("^\\s+/organism=", line)) {
      data_list$ORGANISM <- sub("^\\s+/organism=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/mol_type=", line)) {
      data_list$MOL_TYPE <- sub("^\\s+/mol_type=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/strain=", line)) {
      data_list$STRAIN <- sub("^\\s+/strain=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/isolation_source=", line)) {
      data_list$ISOLATION_SOURCE <- sub("^\\s+/isolation_source=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/locus=", line)) {
      data_list$LOCUS <- sub("^\\s+/lous=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/db_xref=", line)) {
      data_list$DB_XREF <- sub("^\\s+/db_xref=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/geo_loc_name=", line)) {
      data_list$GEO_LOC_NAME <- sub("^\\s+/geo_loc_name=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/collection_date=", line)) {
      data_list$COLLECTION_DATE <- sub("^\\s+/collection_date=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/sub_species=", line)) {
      data_list$SUB_SPECIES <- sub("^\\s+/sub_species=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/lat_lon=", line)) {
      data_list$LAT_LON <- sub("^\\s+/lat_lon=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/collected_by=", line)) {
      data_list$COLLECTED_BY <- sub("^\\s+/collected_by=\"([^\"]+)\".*", "\\1", line)
    } else if (grepl("^\\s+/submitter_seqid=", line)) {
      data_list$SUBMITTER_SEQID <- sub("^\\s+/submitter_seqid=\"([^\"]+)\".*", "\\1", line)
    }
  }
  return(data_list)
}

# Process each entry and add to the data frame
for (entry in entries) {
  entry_data <- extract_data(entry)
  df <- rbind(df, as.data.frame(entry_data, stringsAsFactors=FALSE))
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "D:/ARHIVE_Bioinformatics/ARCHIVE_GeneticInfo/NCBI/NCBI_Merge/Pasteurella/Processed/FeaturesSource/PasSource1.txt", sep="|", row.names=FALSE, quote=FALSE)
cat("Data has been saved to outFsource.txt\n")

```

Feature gene/CDS----------------------------------------------------------------
```{r}
# Read data from input file
input_file <- "NCBI_data_2024-11-26_Pas.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")
library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/"))

# Split the data into individual entries
entries <- strsplit(data, "//")[[1]]

# Initialize an empty data frame with the required columns
df <- data.frame(ACCESSION=character(),  
                 FEATURE_TYPE=character(), LOCUS_TAG=character(), OLD_LOCUS_TAG=character(),
                 INFERENCE=character(), SEQUENCE=character(), GO_FUNCTION=character(),
                 NOTE=character(), GENE_PREDICTION_METHOD=character(), PRODUCT=character(),
                 PROTEIN_ID=character(), stringsAsFactors=FALSE)

# Function to extract data from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(ACCESSION="", FEATURE_TYPE="", LOCUS_TAG="", OLD_LOCUS_TAG="", 
                    INFERENCE="", SEQUENCE="", GO_FUNCTION="", NOTE="", 
                    GENE_PREDICTION_METHOD="", PRODUCT="", PROTEIN_ID="")
  in_features_section <- FALSE
  current_accession <- ""
  
  for (line in lines) {
    if (grepl("^ACCESSION", line)) {
      current_accession <- sub("^ACCESSION\\s+", "", line)
    } else if (grepl("^FEATURES", line)) {
      in_features_section <- TRUE
    } else if (in_features_section && grepl("^\\s+(gene|CDS|rRNA|tRNA)", line)) {
      if (data_list$FEATURE_TYPE != "") {
        # Save the current feature before starting a new one
        data_list$ACCESSION <- current_accession
        df <<- rbind(df, as.data.frame(data_list, stringsAsFactors=FALSE))
        data_list <- list(ACCESSION=current_accession, FEATURE_TYPE="", LOCUS_TAG="", OLD_LOCUS_TAG="", 
                          INFERENCE="", SEQUENCE="", GO_FUNCTION="", NOTE="", 
                          GENE_PREDICTION_METHOD="", PRODUCT="", PROTEIN_ID="")
      }
      current_feature <- sub("^\\s+(gene|CDS|rRNA|tRNA).*", "\\1", line)
      data_list$FEATURE_TYPE <- current_feature
    } else if (in_features_section && grepl("^\\s+/locus_tag=", line)) {
      data_list$LOCUS_TAG <- sub("^\\s+/locus_tag=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/old_locus_tag=", line)) {
      data_list$OLD_LOCUS_TAG <- sub("^\\s+/old_locus_tag=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/inference=", line)) {
      data_list$INFERENCE <- sub("^\\s+/inference=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/sequence=", line)) {
      data_list$SEQUENCE <- sub("^\\s+/sequence=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/GO_function=", line)) {
      data_list$GO_FUNCTION <- sub("^\\s+/GO_function=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/note=", line)) {
      data_list$NOTE <- sub("^\\s+/note=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/gene_prediction_method=", line)) {
      data_list$GENE_PREDICTION_METHOD <- sub("^\\s+/gene_prediction_method=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/product=", line)) {
      data_list$PRODUCT <- sub("^\\s+/product=\"([^\"]+)\".*", "\\1", line)
    } else if (in_features_section && grepl("^\\s+/protein_id=", line)) {
      data_list$PROTEIN_ID <- sub("^\\s+/protein_id=\"([^\"]+)\".*", "\\1", line)
    }
  }
  
  # Save the last feature in the entry
  if (data_list$FEATURE_TYPE != "") {
    data_list$ACCESSION <- current_accession
    df <<- rbind(df, as.data.frame(data_list, stringsAsFactors=FALSE))
  }
}

# Process each entry and add to the data frame
for (entry in entries) {
  extract_data(entry)
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "outputFgeneCDS-v10.txt", sep="|", row.names=FALSE, quote=FALSE)


```

ORIGIN
```{r}
input_file <- "xxxx.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")

library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/"))

# Split the data into individual entries
entries <- strsplit(data, "//")[[1]]

# Initialize an empty data frame with the required columns
df <- data.frame(ACCESSION=character(), ORIGIN=character(), stringsAsFactors=FALSE)

# Function to extract ACCESSION and ORIGIN from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(ACCESSION="", ORIGIN="")
  
  current_field <- NULL
  for (line in lines) {
    if (grepl("^ACCESSION", line)) {
      data_list$ACCESSION <- sub("^ACCESSION\\s+", "", line)
    } else if (grepl("^ORIGIN", line)) {
      current_field <- "ORIGIN"
      data_list$ORIGIN <- trimws(sub("^ORIGIN\\s+", "", line))
    } else if (!is.null(current_field)) {
      if (trimws(line) != "") {  # Skip blank lines
        # Remove digits from the line
        line <- gsub("[0-9]", "", line)
        data_list[[current_field]] <- paste(data_list[[current_field]], trimws(line), sep=" ")
      }
    }
  }
  return(data_list)
}

# Process each entry and add to the data frame
for (entry in entries) {
  entry_data <- extract_data(entry)
  df <- rbind(df, as.data.frame(entry_data, stringsAsFactors=FALSE))
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "outputORG.txt", sep="|", row.names=FALSE, quote=FALSE)
cat("Data has been saved to outputORG.txt\n")



```

CONTIG
```{r}
input_file <- "input.txt"
data <- readLines(input_file, warn = FALSE)

# Combine all lines into a single string
data <- paste(data, collapse = "\n")

library(stringi)
#data <- stri_replace_all_fixed(data, "https://", "https:/")
data <- stri_replace_all_fixed(data, 
                               c("https://", "http://"), 
                               c("https:/", "http:/"))

# Split the data into individual entries
entries <- strsplit(data, "//")[[1]]

# Initialize an empty data frame with the required columns
df <- data.frame(ACCESSION=character(), CONTIG=character(), stringsAsFactors=FALSE)

# Function to extract ACCESSION and CONTIG from each entry
extract_data <- function(entry) {
  lines <- strsplit(entry, "\n")[[1]]
  data_list <- list(ACCESSION="", CONTIG="")
  
  current_field <- NULL
  for (line in lines) {
    if (grepl("^ACCESSION", line)) {
      data_list$ACCESSION <- sub("^ACCESSION\\s+", "", line)
    } else if (grepl("^CONTIG", line)) {
      current_field <- "CONTIG"
      data_list$CONTIG <- trimws(sub("^CONTIG\\s+", "", line))
    } else if (!is.null(current_field)) {
      if (trimws(line) != "") {  # Skip blank lines
        # Remove digits from the line
        line <- gsub("[0-9]", "", line)
        data_list[[current_field]] <- paste(data_list[[current_field]], trimws(line), sep=" ")
      }
    }
  }
  return(data_list)
}

# Process each entry and add to the data frame
for (entry in entries) {
  entry_data <- extract_data(entry)
  df <- rbind(df, as.data.frame(entry_data, stringsAsFactors=FALSE))
}

# Save the data frame to a text file with '|' as delimiter
write.table(df, "outputCON.txt", sep="|", row.names=FALSE, quote=FALSE)
cat("Data has been saved to outputCON.txt\n")



```

#_______________________________________________________________________________
#-------------------------Create Fasta File-------------------------------------
#_______________________________________________________________________________
# Create Fasta File from TXT
```{r}
# Function to convert text file to FASTA format
convert_to_fasta <- function(input_file, output_file) {
  # Read the entire file
  lines <- readLines(input_file)
  
  # Initialize variables
  fasta_content <- ""
  sequence <- ""
  header <- ""
  in_origin <- FALSE
  
  # Process each line
  for (line in lines) {
    if (grepl("^LOCUS", line)) {
      if (sequence != "") {
        fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
        sequence <- ""
      }
      header <- paste0(">", sub("^LOCUS\\s+", "", line))
    } else if (grepl("^ORIGIN", line)) {
      in_origin <- TRUE
    } else if (grepl("^//", line)) {
      in_origin <- FALSE
      if (sequence != "") {
        fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
        sequence <- ""
      }
    } else if (in_origin && grepl("^\\s*[0-9]+", line)) {
      sequence <- paste0(sequence, gsub("[^a-zA-Z]", "", line))
    }
  }
  
  # Add the last sequence
  if (sequence != "") {
    fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
  }
  
  # Write to output file
  writeLines(fasta_content, output_file)
}

# Specify the input and output file paths
input_file <- "C:/Users/Pc/Desktop/RAZI/FetchNCBI/H9N2/NCBI_data_2024-11-27_H9N2 - Copy.txt"
output_file <- "C:/Users/Pc/Desktop/RAZI/FetchNCBI/H9N2/NCBI_data_2024-11-27_H9N2 - Copy.fasta"

# Convert the file
convert_to_fasta(input_file, output_file)



```
```{r}
# Function to convert text file to FASTA format
convert_to_fasta <- function(input_file, output_file) {
  # Read the entire file
  lines <- readLines(input_file)
  
  # Initialize variables
  fasta_content <- ""
  sequence <- ""
  header <- ""
  in_origin <- FALSE
  
  # Process each line
  for (line in lines) {
    if (grepl("^LOCUS", line)) {
      if (sequence != "") {
        fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
        sequence <- ""
      }
      header <- paste0(">", gsub("\\s+", "|", sub("^LOCUS\\s+", "", line)))
    } else if (grepl("^ORIGIN", line)) {
      in_origin <- TRUE
    } else if (grepl("^//", line)) {
      in_origin <- FALSE
      if (sequence != "") {
        fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
        sequence <- ""
      }
    } else if (in_origin && grepl("^\\s*[0-9]+", line)) {
      sequence <- paste0(sequence, gsub("[^a-zA-Z]", "", line))
    }
  }
  
  # Add the last sequence
  if (sequence != "") {
    fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
  }
  
  # Write to output file
  writeLines(fasta_content, output_file)
}

# Specify the input and output file paths
input_file <- "C:/Users/Pc/Desktop/RAZI/FetchNCBI/H9N2/NCBI_data_2024-11-27_H9N2.txt"
output_file <- "C:/Users/Pc/Desktop/RAZI/FetchNCBI/H9N2/NCBI_data_2024-11-27_H9N2_2.fasta"

# Convert the file
convert_to_fasta(input_file, output_file)

