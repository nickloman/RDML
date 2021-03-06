library(shiny)

shinyUI(fluidPage(
  titlePanel("RDML import"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rdml.file", h4("Upload RDML File:"),
                accept=c("application/zip", ".rdml")),
      h4("Input Options:"),
      textInput("pattern", "Name Pattern:", "%NAME%__%TUBE%"),      
      tags$ul("Possible keys in Name Pattern:",              
              tags$li('%NAME% - name of the sample inputted in the qPCR software (ex.: "Sample 1")'),
              tags$li('%ID% - tube ID (ex.: "23")'),
              tags$li('%TUBE% - tube name (ex.: "B11")'),
              tags$li('%TARGET% - PCR target (ex.: "GAPDH")'),
              tags$li('%TYPE% - type of the sample (ex.: "unkn")')),
      conditionalPanel(
        condition = "document.getElementById('rdml.file').value != ''",
        actionButton("update", "Update"),
        tags$ul(h4("Download As:"), 
                tags$li(downloadLink("downloadRdmlObj", "RDML_object")),
                tags$li(downloadLink("downloadCsv", "CSV")),
                tags$li(downloadLink("downloadCsvFiltered", "CSV filtered")))
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "document.getElementById('rdml.file').value == ''",
        h2("Please upload your data first..."),
        img(src="RDML_logo.png", alt = "RDML logo", width="60%", height="60%")
      ),
      conditionalPanel(
        condition = "document.getElementById('rdml.file').value != ''",
        tabsetPanel(
          tabPanel("Plate Overview",
                   h4("Separate colors by tubes:"),
                   fluidRow(
                     column(4, checkboxInput("sep.name.col", "Name", FALSE)),
                     column(4, checkboxInput("sep.type.col", "Type", TRUE)),
                     column(4, checkboxInput("sep.targets.col", "Targets", TRUE))
                   ),
                   h4("Separate notes by tubes:"),
                   fluidRow(
                     column(4, checkboxInput("sep.name.note", "Name", TRUE)),
                     column(4, checkboxInput("sep.type.note", "Type", FALSE)),
                     column(4, checkboxInput("sep.targets.note", "Targets", FALSE))
                   ),
                   fluidRow(
                     column(6, textInput("colors.overview", "Use Colors:", "")),
                     column(6, actionButton("rand.col.overview", "Randomize"))
                   ),
                   plotOutput("overview.plot"),
                   tableOutput("overview.legend")
          ),
          tabPanel("RDML object summary", verbatimTextOutput("rdml.summary.out")),
          tabPanel("Table/Plots",                 
                   h4("Filter by:"),
                   fluidRow(
                     column(4, uiOutput("ui.targets")),
                     column(4, uiOutput("ui.types")),                                     
                     column(4, textInput("names.filter", "Names:", ""))
                   ),
                   fluidRow(
                     column(3, uiOutput("ui.dtypeSelector")),
                     column(3,radioButtons("tablePlotsSelector",
                                           "View as:",
                                           c("Table" = "table",
                                             "Plots" = "plots"))),
                     conditionalPanel(
                       condition = "input.tablePlotsSelector == 'plots'",
                       column(3,radioButtons("plotStyle",
                                             "Plot Style:",
                                             c("Single" = "single",
                                               "Multi" = "multi"))))
                   ),
                   conditionalPanel(
                     condition = "input.tablePlotsSelector == 'table'",
                     tableOutput("fdata.table")
                   ),
                   conditionalPanel(
                     condition = "input.tablePlotsSelector == 'plots'",
                     plotOutput("fdata.plot"))
          )
        )
      )
    )
  )
)
)