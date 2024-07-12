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
                                               label = div(icon("filter"), span("Reporting flag")),
                                               search = TRUE,
                                               autoSelectFirstOption = FALSE,
                                               multiple = FALSE,
                                               choices = ALL_FLAGS
                            )
                     ),
                     column(width = 2,
                            numericInput("year_from", label = div(icon("filter"), span("Year from")), value = 2022)
                     ),
                     column(width = 2,
                            numericInput("year_to",   label = div(icon("filter"), span("Year to"))  , value = 2022)
                     ),
                     column(width = 2,
                            virtualSelectInput("version_reported",
                                               label = "Version reported",
                                               search = TRUE,
                                               multiple = FALSE,
                                               autoSelectFirstOption = FALSE,
                                               choices = ALL_VERSIONS
                            )
                     ),
                     column(width = 2,
                            virtualSelectInput("content_type",
                                               label = "Content (data)",
                                               search = TRUE,
                                               multiple = FALSE,
                                               autoSelectFirstOption = FALSE,
                                               choices = ALL_CONTENT_TYPES
                            )
                     )
                   ),
                   tabsetPanel(id = "dataset", selected = "ST01-T1FC",
                               tabPanel("ST01-T1FC",
                                        h3("Task 1 - Fleet characteristics"),

                                        downloadButton("exp_st01", "Export"),
                                        div(style = "margin-top: .5em;",
                                          tabsetPanel(id = "ST01", selected = "ST01A",
                                                      tabPanel("ST01A",
                                                               div(style = "margin-top: .5em; font-size: x-small",
                                                                   dataTableOutput("ST01A", fill = TRUE)
                                                               )
                                                      ),
                                                      tabPanel("ST01B",
                                                               div(style = "margin-top: .5em; font-size: x-small",
                                                                   dataTableOutput("ST01B", fill = TRUE)
                                                               )
                                                      )
                                          )
                                        )
                               ),
                               tabPanel("ST02-T1NC",
                                        h3("Task 1 - Nominal catches"),

                                        downloadButton("exp_st02", "Export"),

                                        div(style = "margin-top: .5em; font-size: x-small",
                                            dataTableOutput("ST02", fill = TRUE)
                                        )
                               ),
                               tabPanel("ST03-T2CE",
                                        h3("Task 2 - Catch and effort"),

                                        fluidRow(
                                          column(width = 2,
                                                 virtualSelectInput("ce_data_source",
                                                                    label = div(icon("filter"), span("Data source")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_DATA_SOURCES)
                                          ),
                                          column(width = 2,
                                                 numericInput("ce_data_coverage", label = "Data coverage (%)",
                                                              min = 0, max = 100, value = 100)
                                          )
                                        ),

                                        downloadButton("exp_st03", "Export"),

                                        div(style = "margin-top: .5em; font-size: x-small",
                                            dataTableOutput("ST03", fill = TRUE)
                                        )
                               ),
                               tabPanel("ST04-T2SZ",
                                        h3("Task 2 - Size sampling"),

                                        fluidRow(
                                          column(width = 2,
                                                 virtualSelectInput("sz_species",
                                                                    #selected = "ALB",
                                                                    label = div(icon("filter"), span("Species")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_SPECIES)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("sz_sampling_location",
                                                                    #selected = "ATS",
                                                                    label = div(icon("filter"), span("Sampling location")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_SAMPLING_LOCATIONS)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("sz_frequency_type",
                                                                    #selected = "TLE",
                                                                    label = div(icon("filter"), span("Frequency type")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_FREQUENCY_TYPES)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("sz_class_limit",
                                                                    #selected = "LL",
                                                                    label = div(icon("filter"), span("Class limit")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_CLASS_LIMITS)
                                          )
                                        ),
                                        fluidRow(
                                          column(width = 2,
                                                 virtualSelectInput("sz_product_type",
                                                                    label = div(icon("filter"), span("Product type")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_PRODUCT_TYPES)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("sz_sampling_unit",
                                                                    #selected = "HLS",
                                                                    label = div(icon("filter"), span("Sampling unit")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_SAMPLING_UNITS)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("sz_raised",
                                                                    #selected = "No",
                                                                    label = div(icon("filter"), span("Raised")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = c("Yes", "No"))
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("sz_size_interval",
                                                                    #selected = 1,
                                                                    label = div(icon("filter"), span("Size interval")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_SIZE_INTERVALS)
                                          )
                                        ),

                                        downloadButton("exp_st04", "Export"),

                                        div(style = "margin-top: .5em; font-size: x-small",
                                           dataTableOutput("ST04", fill = TRUE)
                                        )
                               ),
                               tabPanel("ST05-T2CS",
                                        h3("Task 2 - Catch-at-size"),

                                        fluidRow(
                                          column(width = 2,
                                                 virtualSelectInput("cs_species",
                                                                    label = div(icon("filter"), span("Species")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = CAS_SPECIES)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("cs_frequency_type",
                                                                    label = div(icon("filter"), span("Frequency type")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = CAS_FREQUENCY_TYPES)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("cs_class_limit",
                                                                    label = div(icon("filter"), span("Class limit")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_CLASS_LIMITS)
                                          ),
                                          column(width = 2,
                                                 virtualSelectInput("cs_size_interval",
                                                                    label = div(icon("filter"), span("Size interval")),
                                                                    search = TRUE,
                                                                    multiple = FALSE,
                                                                    autoSelectFirstOption = FALSE,
                                                                    choices = ALL_SIZE_INTERVALS)
                                          )
                                        ),

                                        downloadButton("exp_st05", "Export"),

                                        div(style = "margin-top: .5em; font-size: x-small",
                                            dataTableOutput("ST05", fill = TRUE)
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
