server = function(input, output, session) {
  output$ST01A = renderDataTable({
    ST01_table = ST01A.filter_data(FC, FC_f,
                                   reporting_flag = input$reporting_flag,
                                   year_from = input$year_from,
                                   year_to = input$year_to)

    if(nrow(ST01_table) == 0) stop("No T1FC (ST01A) data identified by current search criteria!")

    colnames(ST01_table) =
      c("ICCAT serial number",
        "National reg. number",
        "IRCS",
        "IMO",
        "Vessel name",
        "Flag of vessel",
        "Fleet suffix",
        "Gear group",
        "LOA (m)",
        "Tonnage",
        "Tonnage type",
        "Fish carrying cap. (mt)",
        "Year",
        "Fishing days (ATL)",
        "Fishing days (MED)",
        "Fishery 1",
        "Fishery 2",
        "Fishery 3",
        "Fishery 4",
        "Fishery 5",
        "Authorised from",
        "Authorised to",
        "Tot. fishing days",
        "Catches in auth. period (kg)",
        "Bycatch outside auth. period (kg)")

    DT::datatable(
      ST01_table,
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
              th(colspan = 12, style = "border-top: 1px solid;", "Vessel attributes"),
              th(colspan = 13, style = "border-left: 1px solid; border-top: 1px solid;", "Vessel activity in ICCAT fisheries"),
            ),
            tr(
              th(colspan = 5, style = "border-top: 1px solid;", "Vessel ID"),
              th(colspan = 2, style = "border-left: 1px solid; border-top: 1px solid;", "Fleet ID"),
              th(colspan = 5, style = "border-left: 1px solid; border-top: 1px solid;", "Other attributes"),
              th(colspan = 1, style = "border-left: 1px solid; border-top: 1px solid;", "Period"),
              th(colspan = 2, style = "border-left: 1px solid; border-top: 1px solid;", "Total effort"),
              th(colspan = 5, style = "border-left: 1px solid; border-top: 1px solid;", "Fisheries (activity, 1 or +)"),
              th(colspan = 5, style = "border-left: 1px solid; border-top: 1px solid;", "BFTE fisheries only (details)")
            ),
            tr(
              lapply(colnames(ST01_table), th)
            )
          )
        )
      )
    ) %>% DT::formatCurrency(columns = c(9, 10, 12, 14, 15, 23, 24, 25), currency = "")
  })

  output$ST01B = renderDataTable({
    ST01_table = ST01B.filter_data(FCG, FCG_f,
                                   reporting_flag = input$reporting_flag,
                                   year_from = input$year_from,
                                   year_to = input$year_to)

    if(nrow(ST01_table) == 0) stop("No T1FC (ST01B) data identified by current search criteria!")

    colnames(ST01_table) =
      c("Flag of vessel",
        "Fleet suffix",
        "Gear group",
        "Fleet description",
        "LOA class",
        "GRT class",
        "Num. vessels",
        "Year",
        "Fishing days (ATL)",
        "Fishing days (MED)",
        "Fishery 1",
        "Fishery 2",
        "Fishery 3",
        "Fishery 4",
        "Fishery 5")

    DT::datatable(
      ST01_table,
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
              th(colspan = 7, style = "border-top: 1px solid;", "Vessel attributes (small-scale vessels only)"),
              th(colspan = 13, style = "border-left: 1px solid; border-top: 1px solid;", "Fleet (small-scale) activity in ICCAT fisheries"),
            ),
            tr(
              th(colspan = 4, style = "border-top: 1px solid;", "Fleet ID"),
              th(colspan = 3, style = "border-left: 1px solid; border-top: 1px solid;", "Other attributes"),
              th(colspan = 1, style = "border-left: 1px solid; border-top: 1px solid;", "Period"),
              th(colspan = 2, style = "border-left: 1px solid; border-top: 1px solid;", "Total effort"),
              th(colspan = 5, style = "border-left: 1px solid; border-top: 1px solid;", "Fisheries (activity, 1 or +)")
            ),
            tr(
              lapply(colnames(ST01_table), th)
            )
          )
        )
      )
    ) %>% DT::formatCurrency(columns = c(9, 10), currency = "")
  })

  output$exp_st01 = downloadHandler(
    filename = function() { paste0("ST01-T1FC_", input$reporting_flag, "_", input$year_from, "-", input$year_to, ".xlsx") },
    content = function(output_filename) {
      temp_file = temp_xlsx()

      ST01.export(FC, FC_f, FCG, FCG_f,
                  statistical_correspondent = list(name  = input$name,
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
                  destination_file = temp_file)

      file.copy(temp_file, output_filename)
    }
  )

  output$ST02 = renderDataTable({
    ST02_table = ST02.filter_data(NC,
                                  reporting_flag = input$reporting_flag,
                                  year_from = input$year_from,
                                  year_to = input$year_to)

    if(nrow(ST02_table) == 0) stop("No T1NC data identified by current search criteria!")

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

  output$exp_st02 = downloadHandler(
    filename = function() { paste0("ST02-T1NC_", input$reporting_flag, "_", input$year_from, "-", input$year_to, ".xlsx") },
    content = function(output_filename) {
      temp_file = temp_xlsx()

      ST02.export(NC,
                  statistical_correspondent = list(name  = input$name,
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
                  destination_file = temp_file)

      file.copy(temp_file, output_filename)
    }
  )

  output$ST03 = renderDataTable({
    ST03_table = ST03.filter_data_CE(EF, CA,
                                     reporting_flag = input$reporting_flag,
                                     year_from = input$year_from,
                                     year_to = input$year_to,
                                     data_source = input$ce_data_source)

    if(nrow(ST03_table) == 0) stop("No T2CE data identified by current search criteria!")

    colnames(ST03_table) =
      c("Flag of vessel",
        "Fleet suffix",
        "Year",
        "Month",
        "Gear",
        "Square type",
        "Quadrant",
        "Lat",
        "Lon",
        "School type",
        "Effort",
        "Effort Type",
        "Effort",
        "Effort Type",
        str_replace_all(colnames(ST03_table)[15:ncol(ST03_table)], "_", " / ")
      )

    variable_columns = ncol(ST03_table) - 14

    DT::datatable(
      ST03_table,
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
              th(colspan = 14, style = "border-top: 1px solid;", "Catch and effort attributes"),
              th(colspan = variable_columns, style = "border-top: 1px solid; border-left: 1px solid;", "Species catch composition (kg)")
            ),
            tr(
              th(colspan = 10, ""),
              th(colspan =  4, style = "border-left: 1px solid;", "Effort data"),
              th(colspan = variable_columns, style = "border-left: 1px solid;", "")
            ),
            tr(
              th(colspan =  2, "Fleet ID"),
              th(colspan =  3, style = "border-left: 1px solid;", "Time series / gear"),
              th(colspan =  4, style = "border-left: 1px solid;", "Geographic area"),
              th(colspan =  1, style = "border-left: 1px solid;", "Fishing mode"),
              th(colspan =  2, style = "text-align: center; border-left: 1px solid;", "Primary"),
              th(colspan =  2, style = "text-align: center; border-left: 1px solid;", "Secondary"),
              th(colspan = variable_columns, style = "border-left: 1px solid;", "Species / Product type / Catch type")
            ),
            tr(
              lapply(colnames(ST03_table), th)
            )
          )
        )
      )
    ) %>% DT::formatCurrency(columns = c(11, 13, 15:ncol(ST03_table)), currency = "")
  })

  output$exp_st03 = downloadHandler(
    filename = function() { paste0("ST03-T2CE_", input$reporting_flag, "_", input$year_from, "-", input$year_to, "_", input$ce_data_source, ".xlsx") },
    content = function(output_filename) {
      temp_file = temp_xlsx()

      ST03.export(EF, CA,
                  statistical_correspondent = list(name  = input$name,
                                                   email = input$email,
                                                   phone = input$phone,
                                                   institution = input$institution,
                                                   department  = input$department,
                                                   address     = input$address,
                                                   country     = input$country),

                  version_reported = input$version_reported,
                  content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN,
                  data_coverage    = input$ce_data_coverage,

                  reporting_flag   = input$reporting_flag,
                  year_from        = input$year_from,
                  year_to          = input$year_to,
                  data_source      = input$ce_data_source,

                  destination_file = temp_file)

      file.copy(temp_file, output_filename)
    }
  )

  output$ST04 = renderDataTable({
    ST04_table = ST04.filter_data(SZ,
                                  reporting_flag = input$reporting_flag, year_from = input$year_from, year_to = input$year_to,
                                  species = input$sz_species, product_type = input$sz_product_type,
                                  sampling_location = input$sz_sampling_location, sampling_unit = input$sz_sampling_unit, raised = is.na(input$sz_raised) | input$sz_raised == "Yes",
                                  frequency_type = input$sz_frequency_type, size_interval = input$sz_size_interval, class_limit = input$sz_class_limit)

    if(nrow(ST04_table) == 0) stop("No T2SZ data identified by current search criteria!")

    colnames(ST04_table) =
      c("Flag of vessel",
        "Fleet suffix",
        "Year",
        "Month",
        "Gear",
        "Catch type",
        "School type",
        "Sampling area",
        "Square type",
        "Quadrant",
        "Lat",
        "Lon",
        "Data source",
        "Total catch (kg)",
        "Sample unit ID",
        "Month (sampling)",
        "Sampled fish (kg)",
        "Sampled fish (no)",
        "Size class",
        "U",
        "M",
        "F",
        "I"
      )

    DT::datatable(
      ST04_table,
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
              th(colspan = 18, style = "border-top: 1px solid;", "Sample attributes"),
              th(colspan =  5, style = "border-left: 1px solid; border-top: 1px solid;", "Size frequency details"),
            ),
            tr(
              th(colspan =  2, "Fleet ID"),
              th(colspan = 12, style = "border-left: 1px solid;", "Catch strata associated to samples"),
              th(colspan =  4, style = "border-left: 1px solid;", "Sample strata"),
              th(colspan =  1, ""),
              th(colspan =  4, style = "border-left: 1px solid;", "Sampled number & gender")
            ),
            tr(
              lapply(colnames(ST04_table), th)
            )
          )
        )
      )
    ) %>%
      DT::formatCurrency(columns = c(14, 17), currency = "") %>%
      DT::formatCurrency(columns = c(18, 20:23), currency = "", interval = 3, mark = ",", digits = 0)
  })

  output$exp_st04 = downloadHandler(
    filename = function() { paste0("ST04-T2SZ_", input$reporting_flag, "_", input$year_from, "-", input$year_to, "_",
                                   input$sz_species, "_", #input$sz_product_type, "_",
                                   input$sz_sampling_location, "_", input$sz_sampling_unit, "_",
                                   ifelse(!is.na(input$sz_raised) & input$sz_raised == "Yes", "RAISED", "NOT_RAISED"), "_",
                                   input$sz_frequency_type, "_", input$sz_class_limit, "_",
                                   input$sz_size_interval, ".xlsx") },
    content = function(output_filename) {
      temp_file = temp_xlsx()

      ST04.export(SZ,
                  statistical_correspondent = list(name  = input$name,
                                                   email = input$email,
                                                   phone = input$phone,
                                                   institution = input$institution,
                                                   department  = input$department,
                                                   address     = input$address,
                                                   country     = input$country),

                  version_reported  = input$version_reported,
                  content_type      = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN,

                  reporting_flag    = input$reporting_flag,
                  year_from         = input$year_from,
                  year_to           = input$year_to,

                  species           = input$sz_species,
                  product_type      = input$sz_product_type,

                  sampling_location = input$sz_sampling_location,
                  sampling_unit     = input$sz_sampling_unit,
                  raised            = is.na(input$sz_raised) | input$sz_raised == "Yes",

                  frequency_type    = input$sz_frequency_type,
                  size_interval     = input$sz_size_interval,
                  class_limit       = input$sz_class_limit,

                  #template_file    = "./refs/ST04-T2SZ.xlsx",
                  destination_file = temp_file)

      file.copy(temp_file, output_filename)
    }
  )

  output$ST05 = renderDataTable({
    ST05_table = ST05.filter_data(CS,
                                  reporting_flag = input$reporting_flag, year_from = input$year_from, year_to = input$year_to,
                                  species = input$cs_species,
                                  frequency_type = input$cs_frequency_type, size_interval = input$cs_size_interval, class_limit = input$cs_class_limit)

    if(nrow(ST05_table) == 0) stop("No T2CS data identified by current search criteria!")

    colnames(ST05_table) =
      c("Flag of vessel",
        "Fleet suffix",
        "Year",
        "Month",
        "Gear",
        "Catch type",
        "School type",
        "Sampling area",
        "Quadrant",
        "Lat",
        "Lon",
        "Total catch (kg)",
        "Total catch (num)",
        "Num. samples",
        "Sampled fish (kg)",
        "Sampled fish (no)",
        "Size class",
        "U",
        "M",
        "F",
        "I"
      )

    DT::datatable(
      ST05_table,
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
              th(colspan = 16, style = "border-top: 1px solid;", "CAS attributes"),
              th(colspan =  5, style = "border-left: 1px solid; border-top: 1px solid;", "CAS details"),
            ),
            tr(
              th(colspan =  2, "Fleet ID"),
              th(colspan = 11, style = "border-left: 1px solid;", "Catch strata associated to samples"),
              th(colspan =  3, style = "border-left: 1px solid;", "Sampling details"),
              th(colspan =  1, ""),
              th(colspan =  4, style = "border-left: 1px solid;", "Sampled number & gender")
            ),
            tr(
              lapply(colnames(ST05_table), th)
            )
          )
        )
      )
    ) %>%
      DT::formatCurrency(columns = c(12, 15), currency = "") %>%
      DT::formatCurrency(columns = c(13, 16, 18:21), currency = "", interval = 3, mark = ",", digits = 0)
  })

  output$exp_st05 = downloadHandler(
    filename = function() { paste0("ST05-T2CS_", input$reporting_flag, "_", input$year_from, "-", input$year_to, "_",
                                   input$cs_species, "_",  input$cs_frequency_type, "_", input$cs_class_limit, "_",
                                   input$cs_size_interval, ".xlsx") },
    content = function(output_filename) {
      temp_file = temp_xlsx()

      ST05.export(CS,
                  statistical_correspondent = list(name  = input$name,
                                                   email = input$email,
                                                   phone = input$phone,
                                                   institution = input$institution,
                                                   department  = input$department,
                                                   address     = input$address,
                                                   country     = input$country),

                  version_reported  = input$version_reported,
                  content_type      = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN,

                  reporting_flag    = input$reporting_flag,
                  year_from         = input$year_from,
                  year_to           = input$year_to,

                  species           = input$cs_species,

                  frequency_type    = input$cs_frequency_type,
                  size_interval     = input$cs_size_interval,
                  class_limit       = input$cs_class_limit,

                  destination_file = temp_file)

      file.copy(temp_file, output_filename)
    }
  )
}
