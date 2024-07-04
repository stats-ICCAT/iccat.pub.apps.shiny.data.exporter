load_EF = function(reporting_flag, year_from, year_to, data_source) {
  SQL_EF =
    paste0("
  	SELECT
  	  fg.FlagCode,
  	  ds.DataSourceCode,
    	ft.FlagOfVesselCode as FlagVessel,
      ft.FleetSuffix,
    	ceP.YearC AS Year,
    	tp.TimePeriodID AS Month,
    	gr.GearCode AS Gear,
    	st.SquareType,
    	CASE
    	  WHEN ceS.QuadID = 1 THEN 'NE'
    	  WHEN ceS.QuadID = 2 THEN 'SE'
    	  WHEN ceS.QuadID = 3 THEN 'SW'
    	  WHEN ceS.QuadID = 4 THEN 'NW'
    	END AS Quadrant,
    	ceS.Lat,
    	ceS.Lon,
    	fm.SchoolTypeCode AS SchoolType,
    	CASE
    		WHEN ceE.PriorityID = 1 THEN 'PRIMARY'
    		WHEN ceE.PriorityID = 2 THEN 'SECONDARY'
    		ELSE 'UNKNOWN'
    	END As EffortTypePriority,
    	et.EffortTypeCode AS EffortType,
    	ceE.Effort AS Effort
    FROM
    	t2ceProcesses ceP
    INNER JOIN
		  DataSources ds
	  ON
		  ceP.DataSourceID = ds.DataSourceID
    INNER JOIN
    	t2ceStrata ceS
    ON
    	ceS.InProcID = ceP.InProcID
    INNER JOIN
    	t2ceEfforts ceE
    ON
    	ceE.StrataID = ceS.StrataID
    INNER JOIN
    	SquareTypes st
    ON
    	st.SquareTypeID = ceS.SquareTypeID
    INNER JOIN
    	TimePeriods tp
    ON
    	tp.TimePeriodID = ceS.TimePeriodID
    INNER JOIN
    	Gears gr
    ON
    	gr.GearID = ceP.GearID
    INNER JOIN
    	GearGroups gg
    ON
    	gg.GearGrpID = gr.GearGrpID
    INNER JOIN
    	Fleets ft
    ON
    	ft.FleetID = ceP.FleetID
    INNER JOIN
    	Flags fg
    ON
    	fg.FlagID = ft.RepFlagID
    INNER JOIN
    	FileTypes ftt
    ON
    	ftt.FileTypeID = ceP.FileTypeID
    INNER JOIN
    	EffortTypes et
    ON
    	et.EffortTypeID = ceE.EffortTypeID
    INNER JOIN
    	SchoolTypes fm
    ON
    	fm.SchoolTypeID = ceS.SchoolTypeID
    WHERE
    	--fg.FlagCode = '", reporting_flag, "' AND
      --ceP.YearC BETWEEN ", year_from, " AND ", year_to, " AND
    	--ds.DataSourceCode = '", data_source, "' AND
    	ceE.PriorityID IN (1, 2)
    "
    )

  return(
    tabular_query(
      statement = SQL_EF,
      connection = DB_STAT(server = "ICARO\\SQL16")
    )
  )
}

load_CA = function(reporting_flag, year_from, year_to, data_source) {
  SQL_CA =
    paste0("
  	SELECT
    	fg.FlagCode,
  	  ds.DataSourceCode,
    	ft.FlagOfVesselCode as FlagVessel,
      ft.FleetSuffix,
    	ceP.YearC AS Year,
    	tp.TimePeriodID AS Month,
    	gr.GearCode AS Gear,
    	st.SquareType,
    	CASE
    	  WHEN ceS.QuadID = 1 THEN 'NE'
    	  WHEN ceS.QuadID = 2 THEN 'SE'
    	  WHEN ceS.QuadID = 3 THEN 'SW'
    	  WHEN ceS.QuadID = 4 THEN 'NW'
    	END AS Quadrant,
    	ceS.Lat,
    	ceS.Lon,
    	fm.SchoolTypeCode AS SchoolType,
    	sp.SpeciesCode AS Species,
	    ptt.ProductTypeCode AS ProductType,
    	ct.CatchTypeCode AS CatchType,
    	ROUND(ceC.CatchStdCE, 3) AS CatchStdCE
    FROM
    	t2ceProcesses ceP
    INNER JOIN
		  DataSources ds
	  ON
		  ceP.DataSourceID = ds.DataSourceID
    INNER JOIN
    	t2ceStrata ceS
    ON
    	ceS.InProcID = ceP.InProcID
    INNER JOIN
    	t2ceCatches ceC
    ON
    	ceC.StrataID = ceS.StrataID
    INNER JOIN
    	SquareTypes st
    ON
    	st.SquareTypeID = ceS.SquareTypeID
    INNER JOIN
    	TimePeriods tp
    ON
    	tp.TimePeriodID = ceS.TimePeriodID
    INNER JOIN
    	Gears gr
    ON
    	gr.GearID = ceP.GearID
    INNER JOIN
    	GearGroups gg
    ON
    	gg.GearGrpID = gr.GearGrpID
    INNER JOIN
    	Fleets ft
    ON
    	ft.FleetID = ceP.FleetID
    INNER JOIN
    	Flags fg
    ON
    	fg.FlagID = ft.RepFlagID
    INNER JOIN
    	Species sp
    ON
    	sp.SpeciesID = ceC.SpeciesID
    INNER JOIN
    	FileTypes ftt
    ON
    	ftt.FileTypeID = ceP.FileTypeID
    INNER JOIN
    	CatchTypes ct
    ON
    	ct.CatchTypeID = ceC.CatchTypeID
    INNER JOIN
    	ProductTypes ptt
    ON
    	ptt.ProductTypeID = ceC.ProductTypeID
    INNER JOIN
    	SchoolTypes fm
    ON
    	fm.SchoolTypeID = ceS.SchoolTypeID
    WHERE
    	--ceP.YearC BETWEEN ", year_from, " AND ", year_to, " AND
    	--fg.FlagCode = '", reporting_flag, "' AND
    	--ds.DataSourceCode = '", data_source, "' AND
    	1 = 1
    "
    )

  return(
    tabular_query(
      statement = SQL_CA,
      connection = DB_STAT(server = "ICARO\\SQL16")
    )
  )
}

filter_CE = function(EF_all, CA_all,
                     reporting_flag, year_from, year_to, data_source) {

  EF_filtered = EF_all[FlagCode == reporting_flag &
                       Year >= year_from &
                       Year <= year_to &
                       DataSourceCode == data_source]

  EF_filtered$FlagCode       = NULL
  EF_filtered$DataSourceCode = NULL

  CA_filtered = CA_all[FlagCode == reporting_flag &
                         Year >= year_from &
                         Year <= year_to &
                         DataSourceCode == data_source]

  CA_filtered$FlagCode       = NULL
  CA_filtered$DataSourceCode = NULL

  EF_filtered$EffortTypePriority =
    factor(
      EF_filtered$EffortTypePriority,
      levels = c("PRIMARY", "SECONDARY", "UNKNOWN"),
      labels = c("PRIMARY", "SECONDARY", "UNKNOWN"),
      ordered = TRUE
    )

  EF_filtered = EF_filtered[, .(Effort = round(sum(Effort, na.rm = TRUE), 2)),
                                keyby = .(FlagVessel, FleetSuffix, Year, Month, Gear, SquareType, Quadrant, Lat, Lon, SchoolType, EffortTypePriority, EffortType)]

  EF_data_primary   = unique(EF_filtered[EffortTypePriority == "PRIMARY"])
  EF_data_primary[, PrimaryEffortType     := EffortType]
  EF_data_primary[, PrimaryEffort         := Effort]

  EF_data_primary$EffortTypePriority = NULL
  EF_data_primary$EffortType         = NULL
  EF_data_primary$Effort             = NULL

  EF_data_secondary = unique(EF_filtered[EffortTypePriority == "SECONDARY"])
  EF_data_secondary[, SecondaryEffortType     := EffortType]
  EF_data_secondary[, SecondaryEffort         := Effort]

  EF_data_secondary$EffortTypePriority = NULL
  EF_data_secondary$EffortType         = NULL
  EF_data_secondary$Effort             = NULL

  EF_data = unique(rbind(EF_data_primary[, 1:10], EF_data_secondary[, 1:10]))
  EF_data[, NROW := .I]

  EF_data_cols = colnames(EF_data)[1:10]

  EF_data =
    merge(
      EF_data, EF_data_primary,
      by = EF_data_cols,
      all.x = TRUE, all.y = FALSE,
      sort = FALSE
    )

  EF_data =
    merge(
      EF_data, EF_data_secondary,
      by = EF_data_cols,
      all.x = TRUE, all.y = FALSE,
      sort = FALSE
    )

  EF_data =
    EF_data[, .(FlagVessel, FleetSuffix, Year, Month, Gear, SquareType, Quadrant, Lat, Lon, SchoolType, PrimaryEffort, PrimaryEffortType, SecondaryEffort, SecondaryEffortType)]

  CA_w =
    dcast.data.table(
      CA_filtered,
      formula = FlagVessel + FleetSuffix + Year + Month + Gear + SquareType + Quadrant + Lat + Lon + SchoolType ~ Species + ProductType + CatchType,
      fun.aggregate = function(v) { return(sum(v, na.rm = TRUE)) },
      fill = NA,
      #drop = c(TRUE, FALSE),
      value.var = "CatchStdCE"
    )

  CE_data =
    merge(
      EF_data, CA_w,
      by = colnames(EF_data)[1:10],
      all.x = TRUE
    )

  return(
    CE_data[order(Year, Month, FleetSuffix, Gear, SquareType, Quadrant, Lat, Lon, SchoolType, PrimaryEffortType, SecondaryEffortType)]
  )
}

export_ST03 = function(EF_all, CA_all,
                       statistical_correspondent = NULL,
                       version_reported = "Final",
                       content_type = "Revision (FULL)",
                       data_coverage = NA,
                       reporting_flag, year_from, year_to,
                       data_source = NA,
                       template_file = "./refs/ST03-T2CE.xlsx",
                       destination_file = NA) {

  if(is.null(statistical_correspondent))
    statistical_correspondent =
      list(
        name  = NA,
        email = NA,
        phone = NA,
        institution = NA,
        department  = NA,
        address     = NA,
        country     = NA
      )

  main_sheet = "ST03-T2CE"

  # DEPENDENCIES FROM REF_FLAGS (which is exported by iccat.pub.data)

  country           = REF_FLAGS[CODE == statistical_correspondent$country]$NAME_EN
  reporting_country = REF_FLAGS[CODE == reporting_flag]$NAME_EN

  filtered_CE = filter_CE(EF_all, CA_all,
                          reporting_flag, year_from, year_to, data_source) #[1:30, 1:51]

  if(nrow(filtered_CE) == 0)
    stop(paste0("No T2CE data found for ", reporting_flag, " (", year_from, "-", year_to, ")"))

  xls_template = wb_load(template_file)

  xls_template$add_data(sheet = main_sheet, paste0("Automatically generated from ICCAT T2CE current data on ", Sys.time()), start_row = 12, start_col = 11, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, statistical_correspondent$name,        start_row =  5, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$email,       start_row =  6, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$phone,       start_row =  6, start_col = 7, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$institution, start_row =  7, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$department,  start_row =  8, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$address,     start_row =  9, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, country,                               start_row =  9, start_col = 7, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, reporting_country, start_row = 12, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, year_from,         start_row = 13, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, year_to,           start_row = 13, start_col = 5, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, data_coverage,     start_row = 14, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, data_source,       start_row = 15, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, version_reported,  start_row = 17, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, content_type,      start_row = 18, start_col = 3, col_names = FALSE)

  num_rows = nrow(filtered_CE)
  num_cols = ncol(filtered_CE)

  LETTERS_SPACE = append(" ", letters)

  ALL_COLUMNS = CJ(LETTERS_SPACE, letters)
  ALL_COLUMNS = toupper(paste0(str_trim(ALL_COLUMNS[[1]]), ALL_COLUMNS[[2]]))

  max_row = max(23, num_rows)
  max_col = max(39, num_cols)

  COLS = ALL_COLUMNS[15:max_col]

  # Handles the "header" of the table, where species / product type / catch type are stored

  # First updates the styles...
  styles = xls_template$get_cell_style(sheet = main_sheet, dims = "O22:O24")
  xls_template$set_cell_style(sheet = main_sheet, dims = paste0("O22:", tail(COLS, 1), 24), style = styles)

  #...then writes the actual data
  SPECIES_PRODUCT_CATCH_TYPE = colnames(filtered_CE)[15:ncol(filtered_CE)]

  counter = 1

  for(item in SPECIES_PRODUCT_CATCH_TYPE) {
    species    = str_replace_all(item, "([A-Z]+)\\_[A-Z]+\\_[A-Z]+",  "\\1")
    product    = str_replace_all(item,  "[A-Z]+\\_([A-Z]+)\\_[A-Z]+", "\\1")
    catch_type = str_replace_all(item,  "[A-Z]+\\_[A-Z]+\\_([A-Z]+)", "\\1")

    xls_template$add_data(sheet = main_sheet, c(species, product, catch_type), start_row = 22, start_col = COLS[counter], col_names = FALSE, na.strings = "")

    counter = counter + 1
  }

  #xls_template$remove_comment(dims = "AN20:AO22")

  if(num_rows > 23 | num_cols > 39) {
    # Copies the styles of the first 14 columns over all available data rows (dataset-dependent)
    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "A28:N28")
    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("A51:N", 27 + num_rows), style = styles)

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "O21")
    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("O21:", tail(COLS, 1), 21), style = styles)

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "O25")
    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("O25:", tail(COLS, 1), 25), style = styles)

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "O27")
    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("O27:", tail(COLS, 1), 27), style = styles)

    xls_template$unmerge_cells(sheet = main_sheet, dims = "O21:AM21")
    xls_template$merge_cells(sheet = main_sheet, dims = paste0("O21:", tail(COLS, 1), 21))

    xls_template$unmerge_cells(sheet = main_sheet, dims = "O25:AM25")
    xls_template$merge_cells(sheet = main_sheet, dims = paste0("O25:", tail(COLS, 1), 25))

    # Copies the styles of the second two cells (Q28 and R28) in the "data" part of the table to every other two columns on the right
    # If copying the first two, the left border on P28 will be copied every other cell...
    EVEN_COLUMNS = COLS[c(TRUE, FALSE)]
    EVEN_COLUMNS = EVEN_COLUMNS[2:length(EVEN_COLUMNS)]

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "Q28")

    for(column in EVEN_COLUMNS) {
      xls_template$set_cell_style(sheet = main_sheet, dims = paste0(column, "28:", column, 28), style = styles)
    }

    ODD_COLUMNS = COLS[c(FALSE, TRUE)]
    ODD_COLUMNS = ODD_COLUMNS[2:length(ODD_COLUMNS)]

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "R28")

    for(column in ODD_COLUMNS) {
      xls_template$set_cell_style(sheet = main_sheet, dims = paste0(column, "28:", column, 28), style = styles)
    }

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = paste0("O28:", tail(ALL_COLUMNS, 1), 28))

    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("O28:", tail(ALL_COLUMNS, 1), 27 + num_rows), style = styles)

    # Enlarges the table to cover for all data rows / columns (dataset-dependent)
    existing_table = xls_template$get_tables()

    extended_table = paste0("A27:", ALL_COLUMNS[max_col], 27 + max_row)

    print(paste0("Ëxtending table ", existing_table$tab_name, " from ", existing_table$tab_ref, " to ", extended_table))

    xls_template$update_table(tabname = existing_table$tab_name, dims = extended_table)
  }

  xls_template$add_data(sheet = main_sheet, filtered_CE, start_row = 28, col_names = FALSE, with_filter = FALSE, na.strings = "")

  if(is.na(destination_file))
    destination_file = paste0("./out/ST03-T2CE_", reporting_flag, "_", year_from, "_", year_to, "_", data_source, ".xlsx")

  wb_save(xls_template, destination_file, overwrite = TRUE)
}
