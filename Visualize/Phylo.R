#decade, year, accession, map, explanation and phylo 
#```{r}
library(shiny)
library(DBI)
library(odbc)
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(DT)
library(ggtree)
library(ape)
library(Biostrings)
library(shiny)
library(dplyr)
library(readr)
library(DBI)
library(odbc)
library(openssl)
library(seqinr)
library(msa)
#library(shinyRenderWidget)
library(htmlwidgets)
#_______________________________________________________________________________
#----------------------------------Interface------------------------------------
#_______________________________________________________________________________

ui <- fluidPage(
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
      tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
      tags$style(type = "text/css", ".link-spacing {margin-right: 20px; /* Adjust the value as needed */ }")
    ),
    
    div(class = "top-right",
        tags$style(type = "text/css", ".top-right {position: absolute; top: 10px; right: 10px; z-index: 500; text-align: right;}"),
        tags$a("About this tool", href="https://xxxx", class = "link-spacing"),
        tags$a("Analysis", href="https://xxxx")
    ),
    
    tags$style(HTML("
      #.main-panel { height: 100vh; display: flex; flex-direction: column; } 
      #.visualization-panel { overflow-y: auto; flex-grow: 1; }
      .main-panel { overflow-y: auto; height: 100vh; /* Remove overflow-y and height properties */ } 
      .visualization-panel { overflow-y: auto; height: 100vh; }
      .sidebar {position: fixed;width: 20%;}
      .content {margin-left: 20%;}
    ")
  ),
  titlePanel("Welcome to the Tracing Novel Vaccine Dashboard"),
  sidebarLayout(
    sidebarPanel(width = 1, class = "sidebar", # Adjust the width as needed (default is 4)
                 tags$img(src = "https://rvsri.ac.ir/Content/AssetsRazi/image/razilogo-abi2.png", class = "logo", style = "max-width: 100%; height: auto;"),
                 
                 selectInput("agent", "Choose Viral Agent:", 
                             choices = c("h9n2", "agents2", "agents3", "agents4"), selected = "h9n2"),
                 selectInput("database", "Choose Database:", 
                             choices = c("NCBI_ViralAgents", "database2", "database3", "database4"), selected = "NCBI_ViralAgents"),
                 selectInput("period", "Choose Period:", 
                             choices = c("three decades before", "two decades before", "decade before", "last 5 years", "last 3 years"), selected = "last 3 years"),
                 selectInput("year", "Select Year:", choices = 1900:2100, selected = as.numeric(format(Sys.Date(), "%Y"))),
                 actionButton("createTree", "Create Phylogenetic Tree"),br(),br(),
                 actionButton("refresh", "Refresh Page"),
    ),
    mainPanel(class = "main-panel content",
              fluidRow(
                  p("This dashboard provides insights into the tracing of novel vaccines for various viral organisms. You can explore national and international data, visualize timelines, and analyze phylogenetic trees."),
              ),
              fluidRow(
                br(), verbatimTextOutput("para1"), br(),
                column(6, tabsetPanel(
                  tabPanel("Timeline",plotOutput("lineChart"))
                )),
                column(6, tabsetPanel(
                  tabPanel("Normal", leafletOutput("mapNormal")),
                  tabPanel("Topographic", leafletOutput("mapTopographic")),
                  tabPanel("Metro", leafletOutput("mapMetro"))
                ))
              ), 
              fluidRow(
                br(), verbatimTextOutput("para2"),br(),
                column(6, tabsetPanel(
                  tabPanel("Year", 
                           selectInput("xAxis", "Select X-Axis:", choices = c("Month", "Week"), selected = "Month"),
                           plotOutput("detailYear"))
                )),
                column(6, tabsetPanel(
                  tabPanel("Normal", leafletOutput("detailMapNormal")),
                  tabPanel("Topographic", leafletOutput("detailMapTopographic")),
                  tabPanel("Metro", leafletOutput("detailMapMetro"))
                ))
              ),
              fluidRow(
                br(), verbatimTextOutput("para3"),br(),
                tabsetPanel(
                  tabPanel("Month", 
                           selectInput("month", "Select Month:", choices = month.name, selected = month.name[as.numeric(format(Sys.Date(), "%m"))]),
                           DT::dataTableOutput("monthTable")),
                  tabPanel("Week", 
                           selectInput("week", "Select Week:", choices = 1:52, selected = as.numeric(format(Sys.Date(), "%U"))),
                           DT::dataTableOutput("weekTable")),
                  tabPanel("Day", plotOutput("detailDay"),
                           tableOutput("dayTable"))
                ),
                tabsetPanel(
                  tabPanel("ClustalW Tree", plotOutput("tree_dCPlot")),
                  tabPanel("Muscle Tree", plotOutput("tree_dMPlot")),
                  tabPanel("Tree", plotOutput("treePlot"))
                )
              )      
    )
  )
)


server <- function(input, output, session) {#-----------------------------------
  
  df <- NULL
  Tree <- reactiveVal(NULL) # Create a reactive value to store the tree
  Tree_dC <- reactiveVal(NULL) # Create a reactive value to store the tree
  Tree_dM <- reactiveVal(NULL) # Create a reactive value to store the tree
  
  # Check if the directory exists before creating it
  if (!dir.exists("C:/Users/Administrator/Documents/save")) {
    dir.create("C:/Users/Administrator/Documents/save", mode = "0777")
    print(file.info("C:/Users/Administrator/Documents/save"))
    print(Sys.chmod("C:/Users/Administrator/Documents/save", mode = "0755"))
    
    data_frame <- data.frame()  # Create an empty data frame
    saveRDS(data_frame, "C:/Users/Administrator/Documents/save/data_frame.rds")
    fasta_file <- "C:/Users/Administrator/Documents/save/required.fasta"
    writeLines(">sequence_name\n", fasta_file)
  }
  
  # Handle data_frame file
  data_frame_path <- "C:/Users/Administrator/Documents/save/data_frame.rds"
  if (file.exists(data_frame_path)) {
    file.remove(data_frame_path)
    data_frame <- data.frame()  # Create an empty data frame
    saveRDS(data_frame, data_frame_path)
    message("Data_frame file is created! : ", data_frame_path)
  } else {
    stop("File creation failed. The data_frame file was not created.")
  }
  
  # Handle fasta_file
  fasta_file_path <- "C:/Users/Administrator/Documents/save/required.fasta"
  if (file.exists(fasta_file_path)) {
    file.remove(fasta_file_path)
    writeLines(">sequence_name\n", fasta_file_path)
    message("Fasta file is created! : ", fasta_file_path)
  } else {
    stop("File creation failed. The FASTA file was not created.")
  }
  
  
  output$para1 <- renderPrint({#------------------------------------------------
    cat(" It shows pathogen data at three-year intervals, up to three decades, 
    and the data is displayed simultaneously on a map. 
    Required Parameters:
")})
  
  output$para1 <- renderPrint({#------------------------------------------------
    cat(" It shows pathogen data at three-year intervals, up to three decades, 
    and the data is displayed simultaneously on a map. 
    Required Parameters:
")})
  
  output$para2 <- renderPrint({#------------------------------------------------
    cat(" It shows pathogen data in selected annual, monthly and weekly intervals 
    and the data is simultaneously displayed on a map.    
    Required Parameters:
"
    )})
  
  output$para3 <- renderPrint({#------------------------------------------------
    cat(" It displayed the relevant data and visualized the corresponding phylogenetic tree 
    Required Parameters:
"
    )})
  
  fetch_data_for_period <- function(agent, db_name, start_year, end_year) {
    con <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={SQL Server};server=WIN-KFO8UF7HKKM;database=", db_name, ";trusted_connection=true;"), timeout = 10)
    main_table <- paste0(agent, "V_Main")
    fsource_table <- paste0(agent, "V_Fsource")
    query <- paste0(
      "SELECT 
          [dbo].", main_table, ".[DATE], 
          [dbo].", fsource_table, ".[ORGANISM], 
          [dbo].[GEO].[Latitude], 
          [dbo].[GEO].[Longitude], 
          COUNT(*) AS OccurrenceCount
       FROM [dbo].", fsource_table, "
          INNER JOIN 
            [dbo].[GEO] 
          ON 
            SUBSTRING([dbo].", fsource_table, ".[GEO_LOC_NAME], 1, CHARINDEX(':', [dbo].", fsource_table, ".[GEO_LOC_NAME]) - 1) = [dbo].[GEO].[Country]
          INNER JOIN 
            [dbo].", main_table, " 
          ON 
            [dbo].", main_table, ".[ACCESSION] = [dbo].", fsource_table, ".[ACCESSION]
          WHERE 
            CHARINDEX(':', [dbo].", fsource_table, ".[GEO_LOC_NAME]) > 0
          AND 
            YEAR([dbo].", main_table, ".[DATE]) BETWEEN ", start_year, " AND ", end_year, "
          GROUP BY 
            [dbo].", main_table, ".[DATE], [dbo].", fsource_table, ".[ORGANISM], [dbo].[GEO].[Latitude], [dbo].[GEO].[Longitude]"
    )
    data <- dbGetQuery(con, query)
    dbDisconnect(con)
    if (nrow(data) > 0) {
      data$DATE <- as.Date(data$DATE)
    }
    data <- data %>% filter(!is.na(DATE))
    return(data)
  }
  
  fetch_data_for_year <- function(agent, db_name, year) {
    con <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={SQL Server};server=WIN-KFO8UF7HKKM;database=", db_name, ";trusted_connection=true;"), timeout = 10)
    main_table <- paste0(agent, "V_Main")
    fsource_table <- paste0(agent, "V_Fsource")
    query <- paste0(
      "SELECT   
        [dbo].", main_table, ".[DATE], 
        [dbo].", main_table, ".[ACCESSION], 
        [dbo].", fsource_table, ".[ORGANISM], 
        [dbo].[GEO].[Latitude], [dbo].[GEO].[Longitude], 
        COUNT(*) AS OccurrenceCount
     FROM 
        [dbo].", fsource_table, "
     INNER JOIN 
        [dbo].[GEO] 
     ON 
        SUBSTRING([dbo].", fsource_table, ".[GEO_LOC_NAME], 1, CHARINDEX(':', [dbo].", fsource_table, ".[GEO_LOC_NAME]) - 1) = [dbo].[GEO].[Country]
     INNER JOIN
        [dbo].", main_table, " 
     ON 
        [dbo].", main_table, ".[ACCESSION] = [dbo].", fsource_table, ".[ACCESSION]
     WHERE 
        CHARINDEX(':', [dbo].", fsource_table, ".[GEO_LOC_NAME]) > 0
     AND
        YEAR([dbo].", main_table, ".[DATE]) = ", year, "
     GROUP BY 
        [dbo].", main_table, ".[DATE], [dbo].", main_table, ".[ACCESSION], [dbo].", fsource_table, ".[ORGANISM], [dbo].[GEO].[Latitude], [dbo].[GEO].[Longitude]"
    )
    data <- dbGetQuery(con, query)
    dbDisconnect(con)
    if (nrow(data) > 0) {
      data$DATE <- as.Date(data$DATE)
    }
    data <- data %>% filter(!is.na(DATE))
    return(data)
  }
  
  fetch_data_for_phylo <- function(agent, db_name, year) {  
    con <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={SQL Server};server=WIN-KFO8UF7HKKM;database=", db_name, ";trusted_connection=true;"), timeout = 10)
    main_table <- paste0(agent, "V_Main")
    fsource_table <- paste0(agent, "V_Fsource")
    query <- paste0(
      "SELECT   
      [dbo].", main_table, ".[DATE], 
      [dbo].", main_table, ".[ACCESSION], 
      [dbo].", fsource_table, ".[ORGANISM], 
      [dbo].", fsource_table, ".[MOL_TYPE],
      [dbo].", fsource_table, ".[STRAIN],
      [dbo].", fsource_table, ".[ISOLATION_SOURCE],
      [dbo].", fsource_table, ".[HOST],
      [dbo].", fsource_table, ".[GEO_LOC_NAME]
    FROM 
      [dbo].", fsource_table, "
    INNER JOIN
      [dbo].", main_table, " 
    ON 
      [dbo].", main_table, ".[ACCESSION] = [dbo].", fsource_table, ".[ACCESSION]
    WHERE 
      CHARINDEX(':', [dbo].", fsource_table, ".[GEO_LOC_NAME]) > 0
    AND
      YEAR([dbo].", main_table, ".[DATE]) = ", year
    )
    data <- tryCatch({
      dbGetQuery(con, query)
    }, finally = {
      dbDisconnect(con)
    })
    
    if (nrow(data) > 0) {
      data$DATE <- as.Date(data$DATE)
    }
    data <- data %>% filter(!is.na(DATE))
    return(data)
  }
 
  # Define the function to convert the text file to a FASTA file
  convert_to_fasta <- function(input_file, output_file, accession_numbers) {
    # Read the entire file
    lines <- readLines(input_file)
    
    # Initialize variables
    fasta_content <- ""
    sequence <- ""
    header <- ""
    in_origin <- FALSE
    current_accession <- ""
    
    # Process each line
    for (line in lines) {
      if (grepl("^LOCUS", line)) {
        if (sequence != "" && current_accession %in% accession_numbers) {
          fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
        }
        sequence <- ""
        header <- paste0(">", gsub("\\s+", "|", sub("^LOCUS\\s+", "", line)))
        current_accession <- sub("^LOCUS\\s+([^\\s]+).*", "\\1", line)
      } else if (grepl("^ACCESSION", line)) {
        current_accession <- sub("^ACCESSION\\s+([^\\s]+).*", "\\1", line)
      } else if (grepl("^ORIGIN", line)) {
        in_origin <- TRUE
      } else if (grepl("^//", line)) {
        in_origin <- FALSE
        if (sequence != "" && current_accession %in% accession_numbers) {
          fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
        }
        sequence <- ""
      } else if (in_origin && grepl("^\\s*[0-9]+", line)) {
        sequence <- paste0(sequence, gsub("[^a-zA-Z]", "", line))
      }
    }
    
    # Add the last sequence if it matches an accession number
    if (sequence != "" && current_accession %in% accession_numbers) {
      fasta_content <- paste0(fasta_content, header, "\n", sequence, "\n")
    }
    
    # Write to output file
    writeLines(fasta_content, output_file)
  }  

  output$lineChart <- renderPlot({#---------------------------------------------
    period <- switch(input$period,
                     "three decades before" = 30,
                     "two decades before" = 20,
                     "decade before" = 10,
                     "last 5 years" = 5,
                     "last 3 years" = 3)
    start_year <- as.numeric(format(Sys.Date(), "%Y")) - period
    end_year <- as.numeric(format(Sys.Date(), "%Y"))
    data <- fetch_data_for_period(input$agent, input$database, start_year, end_year)
    
    # Create a complete sequence of dates
    #complete_dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day")
    #complete_data <- data.frame(DATE = complete_dates)
    
    # Merge the complete sequence of dates with your data
    #data <- merge(complete_data, data, by = "DATE", all.x = TRUE)    
    
    ggplot(data, aes(x = DATE, y = OccurrenceCount)) +
      geom_line() +
      labs(title = "Occurrence of Viral Agents Over Time", x = "Date", y = "Occurrence Count")})

  output$mapTopographic <- renderLeaflet({
    period <- switch(input$period,
                     "three decades before" = 30,
                     "two decades before" = 20,
                     "decade before" = 10,
                     "last 5 years" = 5,
                     "last 3 years" = 3)
    start_year <- as.numeric(format(Sys.Date(), "%Y")) - period
    end_year <- as.numeric(format(Sys.Date(), "%Y"))
    data <- fetch_data_for_period(input$agent, input$database, start_year, end_year)
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(OccurrenceCount), popup = ~paste("Organism:", ORGANISM))
  })
  
  output$mapNormal <- renderLeaflet({
    period <- switch(input$period,
                     "three decades before" = 30,
                     "two decades before" = 20,
                     "decade before" = 10,
                     "last 5 years" = 5,
                     "last 3 years" = 3)
    start_year <- as.numeric(format(Sys.Date(), "%Y")) - period
    end_year <- as.numeric(format(Sys.Date(), "%Y"))
    data <- fetch_data_for_period(input$agent, input$database, start_year, end_year)
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(OccurrenceCount), popup = ~paste("Organism:", ORGANISM))
  })
  
  output$mapMetro <- renderLeaflet({
    period <- switch(input$period,
                     "three decades before" = 30,
                     "two decades before" = 20,
                     "decade before" = 10,
                     "last 5 years" = 5,
                     "last 3 years" = 3)
    start_year <- as.numeric(format(Sys.Date(), "%Y")) - period
    end_year <- as.numeric(format(Sys.Date(), "%Y"))
    data <- fetch_data_for_period(input$agent, input$database, start_year, end_year)
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(OccurrenceCount), popup = ~paste("Organism:", ORGANISM))
  })
  
  output$detailYear <- renderPlot({#--------------------------------------------
    req(input$year)
    year <- input$year
    xAxis <- input$xAxis
    data <- fetch_data_for_year(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_year(input$agent, input$database, year)
    }
    if (xAxis == "Month") {
      data <- data %>% 
        mutate(Month = floor_date(DATE, "month")) %>% 
        group_by(Month) %>% 
        summarise(OccurrenceCount = sum(OccurrenceCount))
      ggplot(data, aes(x = Month, y = OccurrenceCount)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "red", size = 3) +
        theme_minimal() +
        labs(title = paste("Yearly Occurrence of Viral Agents in", year, "by Month"), x = "Month", y = "Occurrence Count") +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12, angle = 45, hjust = 1))
    } else if (xAxis == "Week") {
      data <- data %>% 
        mutate(Week = floor_date(DATE, "week")) %>% 
        group_by(Week) %>% 
        summarise(OccurrenceCount = sum(OccurrenceCount))
      ggplot(data, aes(x = Week, y = OccurrenceCount)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "red", size = 3) +
        theme_minimal() +
        labs(title = paste("Yearly Occurrence of Viral Agents in", year, "by Week"), x = "Week", y = "Occurrence Count") +
        scale_x_date(date_labels = "%W %Y", date_breaks = "1 week") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12, angle = 45, hjust = 1))}})
  
  output$detailMapNormal <- renderLeaflet({
    year <- input$year
    data <- fetch_data_for_year(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_year(input$agent, input$database, year)
    }
    data <- data %>%
      group_by(Latitude, Longitude, ORGANISM) %>%
      summarise(OccurrenceCount = sum(OccurrenceCount), .groups = 'drop')
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(OccurrenceCount), popup = ~paste("Organism:", ORGANISM, "<br>Count:", OccurrenceCount))
  })
  
  output$detailMapTopographic <- renderLeaflet({
    year <- input$year
    data <- fetch_data_for_year(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_year(input$agent, input$database, year)
    }
    data <- data %>%
      group_by(Latitude, Longitude, ORGANISM) %>%
      summarise(OccurrenceCount = sum(OccurrenceCount), .groups = 'drop')
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(OccurrenceCount), popup = ~paste("Organism:", ORGANISM, "<br>Count:", OccurrenceCount))
  })
  
  output$detailMapMetro <- renderLeaflet({
    year <- input$year
    data <- fetch_data_for_chart(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_chart(input$agent, input$database, year)
    }
    data <- data %>%
      group_by(Latitude, Longitude, ORGANISM) %>%
      summarise(OccurrenceCount = sum(OccurrenceCount), .groups = 'drop')
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(OccurrenceCount), popup = ~paste("Organism:", ORGANISM, "<br>Count:", OccurrenceCount))
  })
  
  output$monthTable <- DT::renderDataTable({
    year <- NULL; month <- NULL; data <- NULL;  
    req(input$year)
    req(input$month, month.name)
    year <- input$year
    month <- match(input$month, month.name)
    data <- fetch_data_for_phylo(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_phylo(input$agent, input$database, year)
    }
    data <- data %>% filter(month(DATE) == month)
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data is registered for this month"))
    } else {
      # Save the data as a dataframe
      df <- data.frame(Accession = data$ACCESSION, 
                       Strain = data$STRAIN,
                       date = data$DATE,
                       Host = data$HOST, 
                       Sample = data$ISOLATION_SOURCE,
                       Country = data$ GEO_LOC_NAME
                       )
      print(df)

      file_path <- "C:/Users/Administrator/Documents/save/data_frame.rds"
      Sys.chmod(file_path, mode = "0777")
      
      # Check write permission
      write_permission <- file.access(dirname(file_path), 2)
      
      if (write_permission == 0) {
        print("You have write permission for the directory.")
        write_rds(df, file_path)
      } else {
        print("You do NOT have write permission for the directory.")
      }
      return(df)
    }
  })
  
  output$weekTable <- DT::renderDataTable({
    year <- NULL; week <- NULL; data <- NULL;    
    req(input$year)
    req(input$week)
    year <- input$year
    week <- input$week
    data <- fetch_data_for_phylo(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_phylo(input$agent, input$database, year)
    }
    data <- data %>% filter(week(DATE) == week)
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data is registered for this week"))
    } else {
      # Save the data as a dataframe
      df <- data.frame(Accession = data$ACCESSION, 
                       Strain = data$STRAIN,
                       date = data$DATE,
                       Host = data$HOST, 
                       Sample = data$ISOLATION_SOURCE,
                       Country = data$ GEO_LOC_NAME
                       )
      
      file_path <- "C:/Users/Administrator/Documents/save/data_frame.rds"
      Sys.chmod(file_path, mode = "0777")
      
      # Check write permission
      write_permission <- file.access(dirname(file_path), 2)
      
      if (write_permission == 0) {
        print("You have write permission for the directory.")
        write_rds(df, file_path)
      } else {
        print("You do NOT have write permission for the directory.")
      }
      return(df)
      print(df)
    }
  })
  
  output$dayTable <- DT::renderDataTable({
    year <- NULL; day <- NULL; data <- NULL;
    req(input$year)
    day<-Sys.Date()
    year <- input$year
    data <- fetch_data_for_phylo(input$agent, input$database, year)
    if (nrow(data) == 0) {
      year <- year - 1
      data <- fetch_data_for_year(input$agent, input$database, year)
    }
    data <- data %>% filter(DATE == Sys.Date())
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data is registered today"))
    } else {
      return(data.frame(Accession = data$ACCESSION, 
                        Strain = data$STRAIN,
                        date = data$DATE,
                        Host = data$HOST, 
                        Sample = data$ISOLATION_SOURCE,
                        Country = data$ GEO_LOC_NAME
      ))
    }
  }) 
  
  # Define df in the global environment

  observeEvent(input$createTree, {
    dir_path <- "C:/Users/Administrator/Documents/save/data_frame.rds"
    df <<- read_rds(dir_path)
    accession_numbers <- df$Accession
    print(list(accession_numbers))
    
    # Define the path to your text file
    file_path <- "C:/mydata/NCBI/H9N2/NCBI_data_2024-11-27_H9N2.txt"
    
    # Path to the file
    fasta_file <- "C:/Users/Administrator/Documents/save/required.fasta"
    
    # Check write permission for the directory
    dir_path <- dirname(fasta_file)
    write_permission <- file.access(dir_path, mode = 2)

    message("Extraction started! Results saved to: ", fasta_file)

    # Convert the text file to a FASTA file using the accession numbers
    convert_to_fasta(file_path, fasta_file, accession_numbers)
    
    # Print a message indicating the extraction is complete
    if (file.exists(fasta_file)) {
      message("Extraction complete! Results saved to: ", fasta_file)
    } else {
      stop("Error: Extraction failed. The FASTA file was not created.")
    }
    
    # Read the FASTA file and visualize the sequences
    #sequences <- readDNAStringSet(fasta_file)
    
    # Replace 'path/to/your/sequences.fasta' with the actual path to your FASTA file
    fasta_file <- "C:/Users/Administrator/Documents/save/required.fasta"
    
    # Read the FASTA file into a DNAStringSet object
    dna_sequences <- "";
    dna_sequences <- readAAStringSet(fasta_file);
    print(dna_sequences);
    
    aligned_sequences <- "";
    aligned_sequences <- msa(dna_sequences);
    print(aligned_sequences);

    ClustalWAlignment <- "";
    ClustalWAlignment <- msa(dna_sequences, "ClustalW");
    print(ClustalWAlignment); 

    MuscleAlignment <- "";
    MuscleAlignment <- msa(dna_sequences, "Muscle");
    print(MuscleAlignment);

    aligned_sequences2 <- ""  ;ClustalWAlignment2 <- ""; ClustalOmegaAlignment2 <- ""; MuscleAlignment2 <- "";
    
    aligned_sequences2 <- msaConvert(aligned_sequences, type="seqinr::alignment");
    print(aligned_sequences2);
    
    ClustalWAlignment2 <- msaConvert(ClustalWAlignment, type="seqinr::alignment");
    print(ClustalWAlignment2);

    MuscleAlignment2 <- msaConvert(MuscleAlignment, type="seqinr::alignment");
    print(MuscleAlignment2);

    d <- ""; dC <- ""; dM <- "";

    d <- dist.alignment(aligned_sequences2, "identity"); #identity
    d[is.na(d)] <- -1
    print(d);
    dC <- dist.alignment(ClustalWAlignment2, "identity");
    dC[is.na(dC)] <- -1
    print(dC);
    dM <- dist.alignment(MuscleAlignment2, "identity");
    dM[is.na(dM)] <- -1
    print(dM);
    
    library(ape)
    tree <- nj(d); Tree(tree); print(tree)
    tree_dC <- nj(dC); Tree_dC(tree_dC); print(tree_dC)
    tree_dM <- nj(dM); Tree_dM(tree_dM); print(tree_dM)
  })
  
  output$treePlot <- renderPlot({
    # Source: https://yulab-smu.top/treedata-book/chapter4.html
    req(Tree())  # Use the reactive value
    ggtree(Tree(), branch.length='branch') + geom_tiplab(as_ylab = TRUE, color = 'firebrick')
  }) 
  #'none': All branches are of equal length.  
  #'branch': Branch lengths are used as provided in the tree object.
  #log': Log-transformed branch lengths are used.
  #'sqrt': Square root-transformed branch lengths are used.
  
  output$tree_dCPlot <- renderPlot({
    req(Tree_dC())  # Use the reactive value
    ggtree(Tree_dC(), branch.length='branch') + geom_tiplab(as_ylab = TRUE, color = 'firebrick')
  }) 
  output$tree_dMPlot <- renderPlot({
    req(Tree_dM())  # Use the reactive value
    ggtree(Tree_dM(), branch.length='branch') + geom_tiplab(as_ylab = TRUE, color = 'firebrick')
  }) 

  observeEvent(input$refresh, {
    # Your code to refresh the app goes here
    # For example, you can re-run the file handling code
    if (!dir.exists("C:/Users/Administrator/Documents/save")) {
      dir.create("C:/Users/Administrator/Documents/save", mode = "2")
    }
    
    # Handle data_frame file
    data_frame_path <- "C:/Users/Administrator/Documents/save/data_frame.rds"
    if (file.exists(data_frame_path)) {
      file.remove(data_frame_path)
      data_frame <- data.frame()  # Create an empty data frame
      saveRDS(data_frame, data_frame_path)
      message("Data_frame file is created! : ", data_frame_path)
    } else {
      stop("File creation failed. The data_frame file was not created.")
    }
    
    # Handle fasta_file
    fasta_file_path <- "C:/Users/Administrator/Documents/save/required.fasta"
    if (file.exists(fasta_file_path)) {
      file.remove(fasta_file_path)
      writeLines(">sequence_name\n", fasta_file_path)
      message("Fasta file is created! : ", fasta_file_path)
    } else {
      stop("File creation failed. The FASTA file was not created.")
    }
    
    # Reload the session
    session$reload()
    
    output$message <- renderText("App refreshed!")
  })
  
}

shinyApp(ui = ui, server = server)

#```
