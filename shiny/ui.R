ui = function() {
  TITLE = paste0("ICCAT SCRS / Task 1 & 2 data exporter")
  return(
    fluidPage(
      title = TITLE,
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tags$div(
        class = "main-container",
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          tags$div(id = "glasspane",
                   tags$div(class = "loading", "Filtering data and preparing output...")
          )
        ),
        tags$div(
          fluidRow(
            column(
              width = 8,
              h2(
                style = "margin-top: 5px !important",
                img(src = "iccat-logo.jpg", height = "48px"),
                span(TITLE)
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              fluidRow(
                column(width = 12,
                  h3("Statistical correspondent"),
                  h4("Identification"),
                  textInput("name",  label = "Name"),
                  textInput("email", label = "e-mail"),
                  textInput("phone", label = "Phone"),
                  h4("Affiliation"),
                  textInput("institution", label = "Institution"),
                  textInput("department",  label = "Department"),
                  textInput("address",     label = "Address"),
                  virtualSelectInput("country",
                    label = "Country",
                    search = TRUE,
                    autoSelectFirstOption = FALSE,
                    multiple = FALSE,
                    choices = ALL_FLAGS
                  )
                )
              )
            ),
            column(width = 10,
                   h3("Datasets and forms"),
                   fluidRow(
                     column(width = 2,
                            virtualSelectInput("reporting_flag",
                                               label = "Reporting flag",
                                               search = TRUE,
                                               autoSelectFirstOption = FALSE,
                                               multiple = FALSE,
                                               choices = ALL_FLAGS
                            )
                     ),
                     column(width = 2,
                            numericInput("year_from", label = "Year from", value = 2023)
                     ),
                     column(width = 2,
                            numericInput("year_to",   label = "Year to"  , value = 2023)
                     ),
                     column(width = 2,
                            virtualSelectInput("version_reported",
                                               label = "Version reported",
                                               multiple = FALSE,
                                               autoSelectFirstOption = FALSE,
                                               choices = ALL_VERSIONS
                            )
                     ),
                     column(width = 2,
                            virtualSelectInput("content_type",
                                               label = "Content (data)",
                                               multiple = FALSE,
                                               autoSelectFirstOption = FALSE,
                                               choices = ALL_CONTENT_TYPES
                            )
                     )
                   ),
                   tabsetPanel(id = "dataset", selected = "ST02-T1NC",
                               tabPanel("ST02-T1NC",
                                        h3("Task 1 - Nominal Catches"),

                                        actionButton  ("gen_st02", "Generate"),
                                        downloadButton("exp_st02", "Export"),

                                        div(style = "margin-top: .5em; font-size: x-small",
                                            dataTableOutput("ST02", fill = TRUE)
                                        )
                               )
                   )
            )
          )
         )
      )
    )
  )
}
