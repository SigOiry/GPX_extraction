# Install required packages if not already installed
library(shiny)
library(rstudioapi)
library(fs)
library(lubridate)

# UI
ui <- fluidPage(
    titlePanel("GPX File Extractor"),
    sidebarLayout(
        sidebarPanel(
            textInput("drive_letter", "Enter Drive Letter:", value = "E"),
            actionButton("select_output_folder", "Select Output Folder"),
            verbatimTextOutput("output_folder_display"),
            textInput("data_folder_name", "Data Folder Name:", value = "data"),
            checkboxInput("use_date_filter", "Extract files from specific date", value = TRUE),
            conditionalPanel(
                condition = "input.use_date_filter == true",
                dateInput("selected_date", "Select Date:", value = Sys.Date())
            ),
            checkboxInput("delete_existing", "Delete existing .gpx files from GPS after copying", value = FALSE),
            actionButton("extract_button", "Extract")
        ),
        mainPanel(
            verbatimTextOutput("status")
        )
    )
)

# Server
server <- function(input, output, session) {
    
    # Reactive value to store output folder path
    output_folder <- reactiveVal(NULL)
    
    # Update output folder when button is clicked
    observeEvent(input$select_output_folder, {
        # Use rstudioapi::selectDirectory()
        folder <- rstudioapi::selectDirectory(caption = "Select Output Folder")
        if (!is.null(folder)) {
            output_folder(folder)
        }
    })
    
    # Display selected output folder
    output$output_folder_display <- renderText({
        folder <- output_folder()
        if (is.null(folder)) {
            "No folder selected."
        } else {
            paste("Selected Output Folder:", folder)
        }
    })
    
    # Placeholder for status messages
    output$status <- renderText({
        "Awaiting user input..."
    })
    
    observeEvent(input$extract_button, {
        # Reset status
        output$status <- renderText("Processing...")
        
        # Get and validate drive letter
        drive_letter <- toupper(input$drive_letter)
        if (nchar(drive_letter) != 1 || !grepl("^[A-Z]$", drive_letter)) {
            output$status <- renderText("Please enter a valid drive letter (A-Z).")
            return()
        }
        
        # Construct source path
        source_path <- paste0(drive_letter, ":/Garmin/GPX")
        
        # Check if source path exists
        if (!dir.exists(source_path)) {
            output$status <- renderText(paste0("Source path '", source_path, "' does not exist."))
            return()
        }
        
        # Get output folder path
        output_folder_path <- output_folder()
        if (is.null(output_folder_path) || output_folder_path == "") {
            output$status <- renderText("Please select an output folder.")
            return()
        }
        
        # Get data folder name
        data_folder_name <- input$data_folder_name
        if (data_folder_name == "") {
            output$status <- renderText("Please enter a data folder name.")
            return()
        }
        
        # Create data folder inside output folder
        data_folder_path <- file.path(output_folder_path, data_folder_name)
        if (!dir.exists(data_folder_path)) {
            dir.create(data_folder_path, recursive = TRUE)
        }
        
        # Find .gpx files
        gpx_files <- list.files(source_path, pattern = "\\.gpx$", full.names = TRUE)
        if (length(gpx_files) == 0) {
            output$status <- renderText("No .gpx files found in the source path.")
            return()
        }
        
        # If use_date_filter is TRUE, filter files by selected date
        if (input$use_date_filter) {
            # Get the selected date
            selected_date <- as.Date(input$selected_date)
            if (is.na(selected_date)) {
                output$status <- renderText("Please select a valid date.")
                return()
            }
            
            # Get file creation dates
            file_info <- file.info(gpx_files)
            creation_dates <- as.Date(file_info$ctime)
            
            # Filter files by selected date
            files_to_copy <- gpx_files[creation_dates == selected_date]
            
            if (length(files_to_copy) == 0) {
                output$status <- renderText("No .gpx files found for the selected date.")
                return()
            }
        } else {
            # If not using date filter, copy all .gpx files
            files_to_copy <- gpx_files
        }
        
        # Copy files
        file.copy(files_to_copy, data_folder_path, overwrite = TRUE)
        
        # If checkbox is ticked, delete existing .gpx files from GPS after copying
        if (input$delete_existing) {
            # Remove the files we just copied from the GPS
            file.remove(files_to_copy)
            output$status <- renderText(paste0("Files successfully copied and removed from the GPS. ", length(files_to_copy), " .gpx files copied to ", data_folder_path))
        } else {
            output$status <- renderText(paste0("Successfully copied ", length(files_to_copy), " .gpx files to ", data_folder_path))
        }
    })
}

# Run the app
shinyApp(ui = ui, server = server)
