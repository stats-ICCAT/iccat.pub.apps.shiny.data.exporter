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
              th(colspan = 12, "Vessel attributes",                  style = "border-top: 1px solid;"),
              th(colspan = 13, "Vessel activity in ICCAT fisheries", style = "border-top: 1px solid;border-left: 1px solid;"),
            ),
            tr(
              th(colspan =  5, "Vessel ID",                          style = ""),
              th(colspan =  2, "Fleet ID",                           style = "border-left: 1px solid;"),
              th(colspan =  5, "Other attributes",                   style = "border-left: 1px solid;"),
              th(colspan =  1, "Period",                             style = "border-left: 1px solid;"),
              th(colspan =  2, "Total effort",                       style = "border-left: 1px solid;"),
              th(colspan =  5, "Fisheries (activity, 1 or +)",       style = "border-left: 1px solid;"),
              th(colspan =  5, "BFTE fisheries only (details)",      style = "border-left: 1px solid;")
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
              th(colspan =  7, "Vessel attributes (small-scale vessels only)",    style = "border-top: 1px solid;", ),
              th(colspan = 13, "Fleet (small-scale) activity in ICCAT fisheries", style = "border-left: 1px solid; border-top: 1px solid;"),
            ),
            tr(
              th(colspan =  4, "Fleet ID",                                        style = ""),
              th(colspan =  3, "Other attributes",                                style = "border-left: 1px solid;"),
              th(colspan =  1, "Period",                                          style = "border-left: 1px solid;"),
              th(colspan =  2, "Total effort",                                    style = "border-left: 1px solid;"),
              th(colspan =  5, "Fisheries (activity, 1 or +)",                    style = "border-left: 1px solid;")
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
      withProgress(message = "Exporting data to ST01 form. Please wait...", {
        temp_file = temp_xlsx()

        i_statistical_correspondent = list(name  = input$name,
                                           email = input$email,
                                           phone = input$phone,
                                           institution = input$institution,
                                           department  = input$department,
                                           address     = input$address,
                                           country     = input$country)

        i_version_reported = input$version_reported
        i_content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN

        i_reporting_flag = input$reporting_flag
        i_year_from      = input$year_from
        i_year_to        = input$year_to

        ST01A_data = ST01A.filter_data(FC, FC_f,
                                       i_reporting_flag,
                                       i_year_from, i_year_to)

        ST01B_data = ST01B.filter_data(FCG, FCG_f,
                                       i_reporting_flag,
                                       i_year_from, i_year_to)

        future_promise(seed = NULL, {
          ST01.export(ST01A_data, ST01B_data,
                      statistical_correspondent = i_statistical_correspondent,

                      version_reported = i_version_reported,
                      content_type     = i_content_type,

                      reporting_flag   = i_reporting_flag,
                      year_from        = i_year_from,
                      year_to          = i_year_to,
                      destination_file = temp_file)

          file.copy(temp_file, output_filename)
        })
      })
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
        "Task I area",
        "Gear",
        "Fishing zone",
        "Landings (dead, L)",
        "Discards (dead, DD)",
        "Discards (alive, DL)",
        "Caged BFT (alive, FA)",
        "Target / Bycatch",
        "Landings (L)",
        "Discards (DD / DL)",
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
              th(colspan = 16, "Catch attributes",       style = "text-align: left;   border-top: 1px solid;"),
              th(colspan =  4, "Data",                   style = "text-align: left;   border-left: 1px solid; border-top: 1px solid;")
            ),
            tr(
              th(colspan = 2, "Fleet ID",                style = "text-align: left;"),
              th(colspan = 7, "Catch data",              style = "text-align: left;   border-left: 1px solid;"),
              th(colspan = 5, "Quantities caught (kg)",  style = "text-align: center; border-left: 1px solid;"),
              th(colspan = 2, "Conversion factor used)", style = "text-align: center; border-left: 1px solid;"),
              th(colspan = 2, "Landings",                style = "text-align: left;   border-left: 1px solid;"),
              th(colspan = 2, "Discards",                style = "text-align: left;   border-left: 1px solid;")
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
      withProgress(message = "Exporting data to ST02 form. Please wait...", {
        temp_file = temp_xlsx()

        i_statistical_correspondent = list(name  = input$name,
                                           email = input$email,
                                           phone = input$phone,
                                           institution = input$institution,
                                           department  = input$department,
                                           address     = input$address,
                                           country     = input$country)

        i_version_reported = input$version_reported
        i_content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN

        i_reporting_flag = input$reporting_flag
        i_year_from      = input$year_from
        i_year_to        = input$year_to

        ST02_data = ST02.filter_data(NC,
                                     i_reporting_flag,
                                     i_year_from, i_year_to)

        future_promise(seed = NULL, {
          ST02.export(ST02_data,
                      statistical_correspondent = i_statistical_correspondent,

                      version_reported = i_version_reported,
                      content_type     = i_content_type,

                      reporting_flag   = i_reporting_flag,
                      year_from        = i_year_from,
                      year_to          = i_year_to,
                      destination_file = temp_file)

          file.copy(temp_file, output_filename)
        })
      })
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
        "Effort 1",
        "Effort 1 Type",
        "Effort 2",
        "Effort 2 Type",
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
              th(colspan = 14,               "Catch and effort attributes",    style = "border-top: 1px solid;"),
              th(colspan = variable_columns, "Species catch composition (kg)", style = "border-top: 1px solid; border-left: 1px solid;")
            ),
            tr(
              th(colspan =  2,               "Fleet ID"),
              th(colspan =  3,               "Time series / gear",                  style = "border-left: 1px solid;"),
              th(colspan =  4,               "Geographic area",                     style = "border-left: 1px solid;"),
              th(colspan =  5,               "Fishing mode and effort data",        style = "border-left: 1px solid;"),
              th(colspan = variable_columns, "Species / Product type / Catch type for tuna, tuna-like species, and sharks (targeted and bycatch)", style = "border-left: 1px solid;", )
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

      withProgress(message = "Exporting data to ST03 form. Please wait...", {
        temp_file = temp_xlsx()

        i_statistical_correspondent = list(name  = input$name,
                                           email = input$email,
                                           phone = input$phone,
                                           institution = input$institution,
                                           department  = input$department,
                                           address     = input$address,
                                           country     = input$country)

        i_version_reported = input$version_reported
        i_content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN
        i_data_coverage    = input$ce_data_coverage

        i_reporting_flag = input$reporting_flag
        i_year_from      = input$year_from
        i_year_to        = input$year_to
        i_data_source    = input$ce_data_source

        ST03_data = ST03.filter_data_CE(EF, CA,
                                        reporting_flag = input$reporting_flag,
                                        year_from = input$year_from,
                                        year_to = input$year_to,
                                        data_source = input$ce_data_source)

        future_promise(seed = NULL, {
          ST03.export(ST03_data,
                      statistical_correspondent = i_statistical_correspondent,

                      version_reported = i_version_reported,
                      content_type     = i_content_type,
                      data_coverage    = i_data_coverage,

                      reporting_flag   = i_reporting_flag,
                      year_from        = i_year_from,
                      year_to          = i_year_to,
                      data_source      = i_data_source,

                      destination_file = temp_file)

          file.copy(temp_file, output_filename)
        })
      })
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
      withProgress(message = "Exporting data to ST04 form. Please wait...", {
        temp_file = temp_xlsx()

        i_statistical_correspondent = list(name  = input$name,
                                           email = input$email,
                                           phone = input$phone,
                                           institution = input$institution,
                                           department  = input$department,
                                           address     = input$address,
                                           country     = input$country)

        i_version_reported = input$version_reported
        i_content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN

        i_reporting_flag = input$reporting_flag
        i_year_from      = input$year_from
        i_year_to        = input$year_to

        i_species           = input$sz_species
        i_product_type      = input$sz_product_type

        i_sampling_location = input$sz_sampling_location
        i_sampling_unit     = input$sz_sampling_unit
        i_raised            = is.na(input$sz_raised) | input$sz_raised == "Yes"

        i_frequency_type    = input$sz_frequency_type
        i_size_interval     = input$sz_size_interval
        i_class_limit       = input$sz_class_limit

        ST04_data = ST04.filter_data(SZ,
                                     reporting_flag    = i_reporting_flag,
                                     year_from         = i_year_from,
                                     year_to           = i_year_to,

                                     species           = i_species,
                                     product_type      = i_product_type,

                                     sampling_location = i_sampling_location,
                                     sampling_unit     = i_sampling_unit,

                                     raised            = i_raised,

                                     frequency_type    = i_frequency_type,

                                     size_interval     = i_size_interval,
                                     class_limit       = i_class_limit)

        future_promise(seed = NULL, {
          ST04.export(ST04_data,
                      statistical_correspondent = i_statistical_correspondent,

                      version_reported  = i_version_reported,
                      content_type      = i_content_type,

                      reporting_flag    = i_reporting_flag,
                      year_from         = i_year_from,
                      year_to           = i_year_to,

                      species           = i_species,
                      product_type      = i_product_type,

                      sampling_location = i_sampling_location,
                      sampling_unit     = i_sampling_unit,

                      raised            = i_raised,

                      frequency_type    = i_frequency_type,

                      size_interval     = i_size_interval,
                      class_limit       = i_class_limit,

                      destination_file  = temp_file)

          file.copy(temp_file, output_filename)
        })
      })
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
      i_year_from      = input$year_from
      i_year_to        = input$year_to

      withProgress(message = "Exporting data to ST05 form. Please wait...", {
        temp_file = temp_xlsx()

        i_statistical_correspondent = list(name  = input$name,
                                           email = input$email,
                                           phone = input$phone,
                                           institution = input$institution,
                                           department  = input$department,
                                           address     = input$address,
                                           country     = input$country)

        i_version_reported = input$version_reported
        i_content_type     = REF_CONTENT_TYPES[CODE == input$content_type]$NAME_EN

        i_reporting_flag = input$reporting_flag
        i_year_from      = input$year_from
        i_year_to        = input$year_to

        i_species           = input$cs_species

        i_frequency_type    = input$cs_frequency_type
        i_size_interval     = input$cs_size_interval
        i_class_limit       = input$cs_class_limit

        ST05_data = ST05.filter_data(CS,
                                     reporting_flag    = i_reporting_flag,
                                     year_from         = i_year_from,
                                     year_to           = i_year_to,

                                     species           = i_species,

                                     frequency_type    = i_frequency_type,

                                     size_interval     = i_size_interval,
                                     class_limit       = i_class_limit)

        future_promise(seed = NULL, {
          ST05.export(ST05_data,
                      statistical_correspondent = i_statistical_correspondent,

                      version_reported  = i_version_reported,
                      content_type      = i_content_type,

                      reporting_flag    = i_reporting_flag,
                      year_from         = i_year_from,
                      year_to           = i_year_to,

                      species           = i_species,

                      frequency_type    = i_frequency_type,

                      size_interval     = i_size_interval,
                      class_limit       = i_class_limit,

                      destination_file  = temp_file)

          file.copy(temp_file, output_filename)
        })
      })
    }
  )
}
