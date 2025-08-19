gc()

library(shiny)
library(dplyr)
library(readr)
library(readxl)
library(rentrez)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Query by Week/Month and Year"),
  sidebarLayout(
    sidebarPanel(
      actionButton("Extract_Accession", "Extract Accession"),      
      actionButton("Extract_Data", "Extract Data"),
      actionButton("refresh", "Refresh") 
    ),
    mainPanel(
      #verbatimTextOutput("summary"),
      #verbatimTextOutput("progress")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  ncbi_data <- reactiveVal(NULL)
  progress <- reactiveVal("")
  
  observeEvent(input$Extract_Accession, {
    print("Reading data from Excel file...")
    data <- read_excel("C:/Users/Pc/Documents/ExcelAccession_Updated.xlsx")
    print("Data read from Excel file:")
    print(data)
    results(data)
  })
  
  output$summary <- renderText({
    req(results())
    paste(results()$ACCESSION, collapse = "\n\n")
  })
  
  observeEvent(input$Extract_Data, {
    accessions <- results()$ACCESSION
    print("Accessions fetched:")
    print(accessions)
    
    log_file <- "C:/Users/Pc/Documents/Fetch_Log.txt"
    
    lapply(accessions, function(acc) {
      file_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "NCBI_Data", paste0(acc, ".txt"))
      if (file.exists(file_path)) {
        cat(paste(Sys.time(), "File exists for accession", acc, "- skipping download\n"), file = log_file, append = TRUE)
        return(NULL)
      }
      
      Sys.sleep(1)
      cat(paste(Sys.time(), "Fetching data for accession", acc, "\n"), file = log_file, append = TRUE)
      tryCatch({
        fetched_data <- entrez_fetch(db = "nuccore", id = acc, rettype = "gb")
        if (!is.null(fetched_data)) {
          cat(paste(Sys.time(), "Fetched data for accession", acc, "\n"), file = log_file, append = TRUE)
          dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
          file_conn <- file(file_path, open = "wt")
          writeLines(paste("Accession:", acc), file_conn)
          writeLines(as.character(fetched_data), file_conn)
          close(file_conn)
          print(paste("Data saved to file:", file_path))
          cat(paste(Sys.time(), "Data saved to file:", file_path, "\n"), file = log_file, append = TRUE)
        } else {
          cat(paste(Sys.time(), "No data fetched for accession", acc, "\n"), file = log_file, append = TRUE)
        }
      }, error = function(e) {
        cat(paste(Sys.time(), "Error fetching data for accession", acc, ":", e$message, "\n"), file = log_file, append = TRUE)
      })
    })
    
    progress("Data fetch complete.")
    
    file_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "NCBI_Data")
    print("Data saved to files in:")
    print(file_path)
    cat(paste(Sys.time(), "Data saved to files in:", file_path, "\n"), file = log_file, append = TRUE)
  })
  
  
  output$progress <- renderText({
    progress()
  })
  
  observeEvent(input$refresh, {
    session$reload()
  })
}

# Run the Shiny app
shinyApp(ui, server)

