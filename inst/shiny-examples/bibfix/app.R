#

library(shiny)
library(shinydashboard)
library(flexdashboard)
library(ggplot2)
library(dplyr)
library(stringr)
library(synthesisr)
library(shinybusy)
library(openalexR)
library(openalex)

source('scan_file.R')
source('plot_health.R')
source('fill_blanks.R')
source('reconstruct_abstract.R')
source('search_openAlex.R')
source('build_ris.R')

# Set background colour
tags$head(tags$style(
    HTML('
                     #sidebar {
                        background-color: #ffbdbd;
                    }
            
                    body, label, input, button, select { 
                      font-family: "Arial";
                    }')))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("bibfix"),
    
    fluidRow(
        column(10,
               h3('Welcome to bibfix!'),
               br(),
               'bibfix helps you to repair your bibliographic data files!'),
        column(1,
               tags$img(height = 150, src = "https://raw.githubusercontent.com/nealhaddaway/bibfix/master/inst/extdata/bibfix%20hex.png")),
        column(8, align="center", offset = 2,
               br(),
               hr(),
               'Upload an RIS file below to check its health and repair missing fields.',
               br(),
               hr(),
               br())
        ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
            
            # Sidebar panel for inputs ----
            sidebarPanel(
                
                # Input: Select a file ----
                fileInput("file", 
                          "RIS file upload", 
                          multiple = FALSE, 
                          accept = c('.ris', '.txt')),
                uiOutput('fixbutton'),
                br(),
                uiOutput('downloadbutton'),
                add_busy_spinner(spin = "fading-circle", color = "#e43235", margins = c(70, 20), position='bottom-left')
            ),

        # Show a plot of the generated distribution
        mainPanel(
            br(),
            textOutput('report'),
            br(),
            box(flexdashboard::gaugeOutput("plt1"),width=12,background ="green"),
            br(),
            plotOutput('health')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()
    
    #on file upload
    observeEvent(input$file,{
        validate(need(input$file != "", "Select your bibliographic file to upload..."))
        
        #do nothing if file is not uploaded
        if (is.null(input$file)) {
            return(NULL)
        } else {
        
        #upload file
            rv$upload <- read_refs(input$file$datapath)
            rv$n_records <- nrow(rv$upload)
            rv$health <- scan_file(rv$upload)
            
        #create report
            rv$report <- paste0(
                'Your uploaded file contained a total of ',
                rv$n_records,
                ' bibliographic records.'
            )
        }
    })
    
    #repair records
    observeEvent(input$repair,{
        rv$repaired <- repair_refs(rv$upload) 
        rv$upload <- rv$repaired
        rv$n_records <- nrow(rv$upload)
        rv$health <- scan_file(rv$upload)
        #prepare RIS for download
        rv$download_ris <- build_ris(select(rv$repaired, -c(intID)))
    })
    
    #render report
    output$report <- renderText({
        rv$report
    })
    
    #render gauge plot
    output$plt1 <- renderGauge({
        if (is.null(input$file)) {
            return(NULL)
        } else {
            gauge(rv$health$n_complete, min = 0, max = rv$n_records, label = paste("healthy"),
                  gaugeSectors(colors = c("#e43235")
        ))
        }
    })
    
    #render health bar plot
    output$health <- renderPlot({
        if (is.null(input$file)) {
            return(NULL)
        } else {
            plot_health(rv$health)
        }
    })
    
    #render fix button UI
    output$fixbutton <- renderUI({
        if (is.null(input$file)) return(NULL)
        tagList(
            br(),
            'Click the button below to repair your RIS file (this may take a minute)',
            br(),
            actionButton('repair', 'Repair RIS')
        )
    })
    
    #render download button UI
    output$downloadbutton <- renderUI({
        if (is.null(input$repair)) return(NULL)
        tagList(
            downloadButton('download', 'Download RIS')
        )
    })
    
    #download repaired RIS file
    output$download <- downloadHandler(
        filename = function(){
            paste("REPAIRED-", Sys.Date(), ".ris", sep = "")
        },
        content = function(file) {
            write.table(rv$download_ris, file, row.names = FALSE, col.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
