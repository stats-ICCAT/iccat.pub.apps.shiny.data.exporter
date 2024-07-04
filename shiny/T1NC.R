load_NC = function(reporting_flag, year_from, year_to) {
  SQL =
    paste0("
  	SELECT
    	fg.FlagCode,
    	ft.FlagOfVesselCode as FlagVessel,
    	ft.FleetSuffix,
    	t1.Year4 AS Year,
    	sp.Alfa3FAO as Species,
    	ss.Stock,
		  sta.SAreaName AS StockOld,
    	spa.SampAreaCode as SamplingArea,
    	sa.AreaCode AS Area,
    	gr.GearCode AS Gear,
    	fz.CatchCodeDescrip AS FishingZone,
    	dt.CatchTypeCode,
    	t1.CatchMT * 1000 AS CatchKg,
    	tb.TargetByCatchCode AS TargetBycatch,
    	t1.CnvFactor AS ConversionFactor,
    	t1n.DSourceCode AS DataSource,
	    t1n.CorrectionsCode AS Corrections
    FROM
    	T1CatchCur t1
    INNER JOIN
    	Species sp
    ON
    	t1.SpeciesID = sp.SpeciesID
    INNER JOIN
    	Gears gr
    ON
    	t1.GearID = gr.GearID
    LEFT JOIN
    	StatAreas sa
    ON
    	t1.AreaID = sa.AreaID
    LEFT JOIN
    	StocksStAreasDetails sad
    ON
    	sa.AreaID = sad.AreaID
    INNER JOIN
    	StocksAreas sta
    ON
    	sad.StockAreaID = sta.StockAreaID AND sp.StockBoundID = sta.StockBoundID
    INNER JOIN
    	CatchTypes dt
    ON
    	t1.CatchTypeID = dt.CatchTypeID
    INNER JOIN
    	Fleets ft
    ON
    	t1.FleetID = ft.FleetID
    INNER JOIN
    	Flags fg
    ON
    	ft.RepFlagID = fg.FlagID
    INNER JOIN
    	SamplingAreas spa
    ON
    	spa.SampAreaID = t1.SampAreaID
    LEFT JOIN
    	SamplStock ss
    ON
    	spa.SampAreaCode = ss.SampAreaCode AND
	  ((sp.Alfa3FAO NOT IN ('ALB', 'BET', 'BFT', 'BUM', 'SAI', 'SKJ', 'SPF', 'SWO', 'WHM', 'YFT') AND ss.SpeciesCode = 'REST' ) OR sp.Alfa3FAO = ss.SpeciesCode )
    INNER JOIN
    	CatchCodes fz
    ON
    	fz.CatchCodeID = t1.FishZoneID
    LEFT JOIN
    	TargetByCatch tb
    ON
    	t1.TargetByCatchID = tb.TargetByCatchID
    LEFT JOIN
    	t1ncNominalCatches t1n
    ON
    	t1.InProcID = t1n.InProcID AND
    	t1.Year4 = t1n.YearC AND
    	t1.FleetID = t1n.FleetID AND
    	t1.SpeciesID = t1n.SpeciesID AND
    	t1.SampAreaID = t1n.SampAreaID AND
    	t1.AreaID = t1n.AreaID AND
    	t1.GearID = t1n.GearID AND
    	t1.FishZoneID = t1n.FishZoneID AND
    	t1.CatchTypeID = t1n.CatchTypeID AND
    	t1.TargetByCatchID = t1n.TargetByCatchID
    WHERE
    	  --fg.FlagCode = '", reporting_flag, "' AND
        --t1.Year4 BETWEEN ", year_from, " AND ", year_to, " AND
        t1.CatchMT > 0
    "
    )

  return(
    tabular_query(
      statement = SQL,
      connection = DB_T1(server = "ICARO\\SQL16")
    )
  )
}

filter_NC = function(NC_all,
                     reporting_flag, year_from, year_to) {

  filtered_NC = NC_all[FlagCode == reporting_flag &
                       Year >= year_from &
                       Year <= year_to]

  filtered_NC$FlagCode = NULL

  filtered_NC$CatchTypeCode =
    factor(
      filtered_NC$CatchTypeCode,
      levels = c("L", "DD", "DL", "FA"),
      labels = c("L", "DD", "DL", "FA"),
      ordered = TRUE
    )

  filtered_NC[CatchTypeCode == "L",             `:=`(ConversionGroup = "CONV_L", DataSourceGroup = "DS_L", CorrectionsGroup = "CORR_L")]
  filtered_NC[CatchTypeCode == "FA",            `:=`(ConversionGroup = "CONV_F", DataSourceGroup = "DS_F", CorrectionsGroup = "CORR_F")]
  filtered_NC[CatchTypeCode %in% c("DD", "DL"), `:=`(ConversionGroup = "CONV_D", DataSourceGroup = "DS_D", CorrectionsGroup = "CORR_D")]
  filtered_NC[is.na(CatchTypeCode),             `:=`(ConversionGroup = "CONV_U", DataSourceGroup = "DS_U", CorrectionsGroup = "CORR_U")]

  filtered_NC$ConversionGroup =
    factor(
      filtered_NC$ConversionGroup,
      levels = c("CONV_L", "CONV_F", "CONV_D", "CONV_U"),
      labels = c("CONV_L", "CONV_F", "CONV_D", "CONV_U"),
      ordered = TRUE
    )

  filtered_NC$DataSourceGroup =
    factor(
      filtered_NC$DataSourceGroup,
      levels = c("DS_L", "DS_F", "DS_D", "DS_U"),
      labels = c("DS_L", "DS_F", "DS_D", "DS_U"),
      ordered = TRUE
    )

  filtered_NC$CorrectionsGroup =
    factor(
      filtered_NC$CorrectionsGroup,
      levels = c("CORR_L", "CORR_F", "CORR_D", "CORR_U"),
      labels = c("CORR_L", "CORR_F", "CORR_D", "CORR_U"),
      ordered = TRUE
    )

  filtered_NC_w =
    dcast.data.table(
      filtered_NC[CatchTypeCode != "LF"],
      formula = FlagVessel + FleetSuffix + Year + Species + Stock + SamplingArea + Area + Gear + FishingZone + TargetBycatch ~ CatchTypeCode,
      fun.aggregate = function(v) { return(sum(v, na.rm = TRUE)) },
      fill = NA,
      drop = c(TRUE, FALSE),
      value.var = "CatchKg"
    )

  NC_conversion_factors_w =
    dcast.data.table(
      filtered_NC[CatchTypeCode != "LF"],
      formula = FlagVessel + FleetSuffix + Year + Species + Stock + SamplingArea + Area + Gear + FishingZone + TargetBycatch ~ ConversionGroup,
      fun.aggregate = function(v) { return(min(v, na.rm = TRUE)) },
      fill = NA,
      drop = c(TRUE, FALSE),
      value.var = "ConversionFactor"
    )

  NC_conversion_factors_w[is.na(CONV_L) & !is.na(CONV_F), CONV_L := CONV_F]

  NC_data_sources_w =
    dcast.data.table(
      filtered_NC[CatchTypeCode != "LF"],
      formula = FlagVessel + FleetSuffix + Year + Species + Stock + SamplingArea + Area + Gear + FishingZone + TargetBycatch ~ DataSourceGroup,
      fun.aggregate = function(v) { return(min(v, na.rm = TRUE)) },
      fill = NA,
      drop = c(TRUE, FALSE),
      value.var = "DataSource"
    )

  NC_data_sources_w[is.na(DS_L) & !is.na(DS_F), DS_L := DS_F]

  NC_corrections_w =
    dcast.data.table(
      filtered_NC[CatchTypeCode != "LF"],
      formula = FlagVessel + FleetSuffix + Year + Species + Stock + SamplingArea + Area + Gear + FishingZone + TargetBycatch ~ CorrectionsGroup,
      fun.aggregate = function(v) { return(min(v, na.rm = TRUE)) },
      fill = NA,
      drop = c(TRUE, FALSE),
      value.var = "Corrections"
    )

  NC_corrections_w[is.na(CORR_L) & !is.na(CORR_F), CORR_L := CORR_F]

  NC_final_w =
    merge(
      filtered_NC_w,
      NC_conversion_factors_w,
      all.x = TRUE,
      all.y = TRUE
    )

  NC_final_w =
    merge(
      NC_final_w,
      NC_data_sources_w,
      all.x = TRUE,
      all.y = TRUE
    )

  NC_final_w =
    merge(
      NC_final_w,
      NC_corrections_w,
      all.x = TRUE,
      all.y = TRUE
    )

  return(
    NC_final_w
    [, .(FlagVessel, FleetSuffix, Year, Species, Stock, SamplingArea, Area, Gear, FishingZone, L, DD, DL, FA, TargetBycatch, CONV_L, CONV_D, DS_L, CORR_L, DS_D, CORR_D)]
    [order(Year, FleetSuffix, Species, Stock, SamplingArea, Area, Gear, FishingZone)]
  )
}

export_ST02A = function(NC_all,
                        statistical_correspondent = NULL,
                        version_reported = "Final",
                        content_type = "Revision (FULL)",
                        reporting_flag, year_from, year_to,
                        template_file = "./refs/ST02-T1NC.xlsx",
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

  main_sheet = "ST02A"

  # DEPENDENCIES FROM REF_FLAGS (which is exported by iccat.pub.data)

  country           = REF_FLAGS[CODE == statistical_correspondent$country]$NAME_EN
  reporting_country = REF_FLAGS[CODE == reporting_flag]$NAME_EN

  filtered_NC = filter_NC(NC_all, reporting_flag, year_from, year_to)

  if(nrow(filtered_NC) == 0)
    stop(paste0("No T1NC data found for ", reporting_flag, " (", year_from, "-", year_to, ")"))

  xls_template = wb_load(template_file)

  xls_template$add_data(sheet = main_sheet, paste0("Automatically generated from ICCAT T1NC current data on ", Sys.time()), start_row = 12, start_col = 11, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, statistical_correspondent$name,        start_row =  5, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$email,       start_row =  6, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$phone,       start_row =  6, start_col = 7, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$institution, start_row =  7, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$department,  start_row =  8, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, statistical_correspondent$address,     start_row =  9, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, country,                               start_row =  9, start_col = 7, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, reporting_country, start_row = 12, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, year_from, start_row = 13, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, year_to,   start_row = 13, start_col = 5, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, version_reported, start_row = 17, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, content_type,     start_row = 18, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, filtered_NC, start_row = 26, col_names = FALSE, na.strings = "")

  num_rows = nrow(filtered_NC)

  if(num_rows > 25) {
    xls_template$update_table(sheet = main_sheet, tabname = "tblST02A", dims = paste0("A25:T", 25 + num_rows))

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "A26:T26")

    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("A51:T", 25 + num_rows), style = styles)
  }

  if(is.na(destination_file))
    destination_file = paste0("./out/ST02-T1NC_", reporting_flag, "_", year_from, "_", year_to, ".xlsx")

  wb_save(xls_template, destination_file, overwrite = TRUE)
}
