library(shiny)
library(rentrez)
library(dplyr)
library(openxlsx)
library(httr)
library(leaflet)
library(stringr)
library(DT)
library(future)
library(promises)

# Increase the timeout to 1200 seconds
options(timeout = 300)
memory.limit(size = 8000)  # Increased the memory limit to 16GB, adjust as needed

# Shiny UI
ui <- fluidPage(
  titlePanel("P"),
  sidebarLayout(
    sidebarPanel(
      textInput("keywords", "Enter Keywords:", placeholder = "Influenza H9N2"),
      textInput("countries", "Enter Countries (comma-separated):", placeholder = "Iran, UK, Germany"),
      actionButton("search", "Search"),
      downloadButton("downloadCSV", "Download CSV")
    ),
    mainPanel(
      DTOutput("results")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  fetch_in_batches_future <- function(ids, batch_size = 300, min_delay = 1, max_delay = 3) {
    plan(multisession)
    futures <- list()
    
    for (i in seq(1, length(ids), by = batch_size)) {
      batch_ids <- ids[i:min(i + batch_size - 1, length(ids))]
      futures[[length(futures) + 1]] <- future({
        tryCatch({
          data_batch <- entrez_fetch(db = "nuccore", id = batch_ids, rettype = "gb", retmode = "text")
          data_batch <- unlist(strsplit(data_batch, "\n//\n"))
          # Print the batch IDs being fetched
          cat("Fetching batch IDs:", paste(batch_ids, collapse = ", "), "\n")
          data_batch
        }, error = function(e) {
          message("Error fetching batch: ", e)
          showNotification(paste("Error fetching batch:", e$message), type = "error")
          return(NULL)
        })
      })
      
      Sys.sleep(runif(1, min = min_delay, max = max_delay))
    }
    
    all_records <- lapply(futures, value)
    all_records <- unlist(all_records)
    return(all_records)
  }
  
  fetch_and_process_batch <- function(batch_ids, output_file) {
    future({
      tryCatch({
        # Fetch data
        data_batch <- entrez_fetch(db = "nuccore", id = batch_ids, rettype = "gb", retmode = "text")
        data_batch <- unlist(strsplit(data_batch, "\n//\n"))
        
        # Extract info
        accessions <- str_extract_all(data_batch, "ACCESSION\\s+([A-Z0-9_]+)")
        accessions <- unlist(lapply(accessions, function(x) if (length(x) > 0) str_match(x, "ACCESSION\\s+([A-Z0-9_]+)")[,2] else NA))
        
        loci <- str_extract_all(data_batch, "LOCUS\\s+(.+)")
        loci <- unlist(lapply(loci, function(x) if (length(x) > 0) str_match(x, "LOCUS\\s+(.+)")[,2] else NA))
        
        definitions <- str_extract_all(data_batch, "DEFINITION\\s+(.+)")
        definitions <- unlist(lapply(definitions, function(x) if (length(x) > 0) str_match(x, "DEFINITION\\s+(.+)")[,2] else NA))
        
        max_length <- max(length(accessions), length(loci), length(definitions))
        accessions <- c(accessions, rep(NA, max_length - length(accessions)))
        loci <- c(loci, rep(NA, max_length - length(loci)))
        definitions <- c(definitions, rep(NA, max_length - length(definitions)))
        
        data <- data.frame(
          Accession = accessions,
          Locus = loci,
          Definition = definitions
        )
        
        # Save data
        write.table(data, file = output_file, sep = "|", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
        data
      }, error = function(e) {
        message("Error fetching or processing batch: ", e)
        showNotification(paste("Error fetching or processing batch:", e$message), type = "error")
        return(NULL)
      })
    })
  }
  
  search_results <- eventReactive(input$search, {
    keywords <- input$keywords
    countries <- strsplit(input$countries, ",\\s*")[[1]]
    
    if (length(countries) == 0 || (length(countries) == 1 && countries == "")) {
      query <- keywords
    } else {
      query <- paste(keywords, paste(countries, collapse = " OR "), sep = " AND ")
    }
    
    print(paste("Constructed query:", query))
    
    search <- NULL
    attempt <- 1
    max_attempts <- 5
    delay_between_attempts <- 2
    
    while (is.null(search) && attempt <= max_attempts) {
      tryCatch({
        search <- entrez_search(db = "nuccore", term = query, retmax = 999990, retstart = 0, config = httr::timeout(120))
        str(search)
      }, error = function(e) {
        message("Error during entrez_search (attempt ", attempt, "): ", e)
        showNotification(paste("Error during entrez_search (attempt ", attempt, "): ", e$message), type = "error")
        Sys.sleep(delay_between_attempts)
        attempt <- attempt + 1
      })
    }
    
    if (is.null(search)) {
      showNotification("Failed to perform entrez_search after multiple attempts.", type = "error")
      stop("Failed to perform entrez_search after multiple attempts.")
    }
    
    ids <- search$ids
    
    if (length(ids) == 0) {
      return(data.frame(Accession = character(), Locus = character(), Definition = character()))
    } else {
      output_file <- paste0("filtered_data_", Sys.Date(), "_", gsub(" ", "_", keywords), ".txt")
      write.table(data.frame(), file = output_file, sep = "|", row.names = FALSE, col.names = TRUE, quote = FALSE)  # Initialize file
      
      batches <- list()
      for (i in seq(1, length(ids), by = 10)) {
        batch_ids <- ids[i:min(i + 9, length(ids))]
        batches[[length(batches) + 1]] <- fetch_and_process_batch(batch_ids, output_file)
      }
      
      all_data <- lapply(batches, value)
      all_data <- do.call(rbind, all_data)
      
      return(all_data)
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
      tryCatch({
        write.csv(search_results(), file)
      }, error = function(e) {
        showNotification(paste("Error saving CSV:", e$message), type = "error")
      })
    }
  )
}

shinyApp(ui = ui, server = server)

