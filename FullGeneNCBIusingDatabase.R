library(shiny)
library(dplyr)
library(readr)
library(DBI)
library(odbc)
library(rentrez)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Database Query by Week/Month and Year"),
  sidebarLayout(
    sidebarPanel(
      actionButton("Extract_Accession", "Extract Accession"),      
      actionButton("Extract_Data", "Extract Data"),
      actionButton("refresh", "Refresh") 
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      verbatimTextOutput("progress")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  results <- reactiveVal(NULL)
  ncbi_data <- reactiveVal(NULL)
  progress <- reactiveVal("")
  
  observeEvent(input$Extract_Accession, {
    print("Connecting to the database...")
    con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};server=WIN-KFO8UF7HKKM;database=NCBI_ViralAgents;trusted_connection=true;", timeout = 10)
    print("Constructing the query...")    
   # query <- "SELECT [ACCESSION] FROM [NCBI].[dbo].[Pasteurella] "
    query <- "SELECT 
                [Accession] as ACCESSION,
                [Locus],
                SUBSTRING([Locus], PATINDEX('%[0-9][0-9]-%[A-Z][A-Z][A-Z]-%[0-9][0-9][0-9][0-9]%', [Locus]), 11) AS ExtractedDate,
                [Definition]
              FROM 
                [NCBI].[dbo].[Pasteurella]
              ORDER BY ExtractedDate desc
              OFFSET 2999 ROWS
              FETCH NEXT 10000 ROWS ONLY "

    print("Executing the query...")
    results(dbGetQuery(con, query))
    print("Disconnecting from the database...")    
    dbDisconnect(con)
    print("Query results:")
    print(results())
  })
  
  output$summary <- renderText({
    req(results())
    paste(results()$ACCESSION, collapse = "\n\n")
  })
  
  observeEvent(input$Extract_Data, {
    accessions <- results()$ACCESSION
    print("Accessions fetched:")
    print(accessions)
    
    print("Fetching data from NCBI...")
    progress("Starting data fetch...")
    
    lapply(accessions, function(acc) {
      # Set the file path to the specific folder
      file_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "NCBI_Data", paste0(acc, ".txt"))
      
      # Check if the file already exists
      if (file.exists(file_path)) {
        print(paste("File already exists for accession", acc, "- skipping download"))
        return(NULL)
      }
      
      Sys.sleep(0.75)  # Introduce a delay between requests to avoid rate limiting
      fetched_data <- entrez_fetch(db = "nuccore", id = acc, rettype = "gb")
      
      # Check if data is fetched correctly
      if (is.null(fetched_data)) {
        print(paste("No data fetched for accession", acc))
      } else {
        print(paste("Fetched data for accession", acc, ":", fetched_data))
        
        dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
        
        # Write the fetched data to the file
        tryCatch({
          file_conn <- file(file_path, open = "wt")
          writeLines(paste("Accession:", acc), file_conn)
          writeLines(as.character(fetched_data), file_conn)
          #writeLines("//", file_conn)
          #writeLines("\n", file_conn)
          close(file_conn)
          print(paste("Data saved to file:", file_path))
        }, error = function(e) {
          print(paste("Error writing to file:", file_path, ":", e$message))
        })
      }
    })
    
    progress("Data fetch complete.")
    
    file_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "NCBI_Data")
    print("Data saved to files in:")
    print(file_path)
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
