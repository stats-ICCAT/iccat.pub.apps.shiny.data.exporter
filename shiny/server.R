server = function(input, output, session) {
  output$ST02 = renderDataTable({
    ST02_table = generate_ST02()

    colnames(ST02_table) =
      c("Flag of vessel",
        "Fleet suffix",
        "Year",
        "Species",
        "Stock",
        "Sampling",
        "Task I",
        "Gear",
        "Fishing zone",
        "Landings (L)",
        "Discards (DD)",
        "Discards (DL)",
        "Caged BFT (FA)",
        "Target / Bycatch",
        "L",
        "DD / DL",
        "Data source",
        "Corrections",
        "Data source",
        "Corrections")

    DT::datatable(
      ST02_table,
      options = list(
        autoWidth = FALSE,
        scrollX   = TRUE,
        dom       = "ltipr" # To remove the 'search box' - see: https://rstudio.github.io/DT/options.html and https://datatables.net/reference/option/dom
      ),
      filter    = "none",
      selection = "none",
      rownames  = FALSE,
      container = htmltools::withTags(
        table(
          class = "display",
          thead(
            tr(
              th(colspan = 1, ""),
              th(colspan = 1, ""),
              th(colspan = 1, ""),
              th(colspan = 1, ""),
              th(colspan = 3, style = "text-align: center; border-left: 1px solid; border-right: 1px solid;", "Areas"),
              th(colspan = 2, ""),
              th(colspan = 2, style = "text-align: center; border-left: 1px solid;",                          "Dead (kg)"),
              th(colspan = 2, style = "text-align: center; border-left: 1px solid; border-right: 1px solid;", "Alive (kg)"),
              th(colspan = 1, ""),
              th(colspan = 2, style = "text-align: center; border-left: 1px solid;",                          "Conv. factors"),
              th(colspan = 2, style = "text-align: center; border-left: 1px solid;",                          "Landings"),
              th(colspan = 2, style = "text-align: center; border-left: 1px solid; border-right: 1px solid;", "Discards")
            ),
            tr(
              lapply(colnames(ST02_table), th)
            )
          )
        )
      )
    ) %>% DT::formatCurrency(columns = c(10, 11, 12, 13, 15, 16), currency = "")
  })

  generate_ST02 =
    eventReactive(
      input$gen_st02, {
        get_NC(year_from = input$year_from, year_to = input$year_to, reporting_flag = input$reporting_flag)
      }
    )

  output$exp_st02 = downloadHandler(
    filename = function() { paste0("ST02-T1NC_", input$reporting_flag, "_", input$year_from, "_", input$year_to, ".xlsx") },
    content = function(output_filename) {
      temp_file = temp_xlsx()

      export_ST02A(statistical_correspondent = list(name  = input$name,
                                                    email = input$email,
                                                    phone = input$phone,
                                                    institution = input$institution,
                                                    department  = input$department,
                                                    address     = input$address,
                                                    country     = input$country),

                   version_reported = input$version_reported,
                   content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN,

                   reporting_flag   = input$reporting_flag,
                   year_from        = input$year_from,
                   year_to          = input$year_to,
                   template_file    = "../refs/ST02-T1NC.xlsx",
                   destination_file = temp_file)

      file.copy(temp_file, output_filename)
    }
  )
}
