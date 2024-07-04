load_SZ = function(type = 'siz',
                   reporting_flag, year_from, year_to,
                   species, product_type,
                   sampling_location, sampling_unit, raised,
                   frequency_type, size_interval, class_limit) {
  SQL_SZ = paste0("
  	SELECT
  	  fg.FlagCode,
  	  sp.SpeciesCode,
  	  sl.SampLocationCode,
  	  su.SampUnitTypeCode,
  	  frqt.FreqTypeCode,
  	  cl.SzClassLimitCode,
  	  szP.SzInterval,
  	  szP.Raised_SUnit AS Raised,
      ft.FlagOfVesselCode as FlagVessel,
      ft.FleetSuffix,
      szP.YearC AS Year,
      tpc.TimePeriodID AS MonthCatch,
      gr.GearCode AS Gear,
      CASE
        WHEN szP.GearID IN (33, 35, 37, 38, 39, 40, 80, 81, 82, 83, 84) THEN 'DD'
        ELSE 'L'
      END AS CatchType,
      st.SchoolTypeCode AS SchoolType,
      sa.SampAreaCode AS SamplingArea,
      sqt.SquareTypeCode AS SquareType,
      CASE
        WHEN szS.QuadID = 1 THEN 'NE'
        WHEN szS.QuadID = 2 THEN 'SE'
        WHEN szS.QuadID = 3 THEN 'SW'
        WHEN szS.QuadID = 4 THEN 'NW'
      END AS Quadrant,
      FLOOR(szS.Lat) AS Lat,
      FLOOR(szS.Lon) AS Lon,
      ds.DataSourceCode AS DataSource, -- Data source
      szS.CatchWGT AS TotalCatch, -- Total catch
      szS.StrataID AS SampleUnitID, -- Needs to be adjusted as it should start from 1...
      tps.TimePeriodID AS MonthSamp, -- Month sample # TO CHECK!
      szS.FishSampWGT AS SampledWeightKg,
      szS.FishSampNUM AS SampledNumbers,
      szF.SizeClass,
      sx.SexCode AS Sex,
      ROUND(szF.Nr, 0) AS Nr
    FROM
      t2szProcesses szP
    INNER JOIN
      SizeInfo si
    ON
      si.SizeInfoID = szP.SizeInfoID
    INNER JOIN
      t2szStrata szS
    ON
      szP.InProcID = szS.InProcID
    INNER JOIN
      DataSources ds
    ON
      szS.DataSourceID = ds.DataSourceID
    INNER JOIN
      t2szFreqs szF
    ON
      szF.StrataID = szS.StrataID
    INNER JOIN
      Species sp
    ON
      sp.SpeciesID = szP.SpeciesID
    INNER JOIN
      Fleets ft
    ON
      ft.FleetID = szP.FleetID
    INNER JOIN
      Flags fg
    ON
      fg.FlagID = ft.RepFlagID
    INNER JOIN
      Gears gr
    ON
      gr.GearID = szP.GearID
    INNER JOIN
      FreqTypes frqt
    ON
      frqt.FreqTypeID = szP.FreqTypeID
    INNER JOIN
      SizeClassLimits cl
    ON
      cl.SzClassLimitID = szP.SzClassLimit
    INNER JOIN
      TimePeriods tpc
    ON
      tpc.TimePeriodID = szS.TimePeriodCatch
    INNER JOIN
      TimePeriods tps
    ON
      tps.TimePeriodID = szS.TimePeriodSamp
    INNER JOIN
      SchoolTypes st
    ON
      st.SchoolTypeID = szS.SchoolTypeID
    INNER JOIN
      SamplingAreas sa
    ON
      sa.SampAreaID = szS.SampAreaID
    INNER JOIN
      SquareTypes sqt
    ON
      sqt.SquareTypeID = szS.SquareTypeID
    INNER JOIN
      SexInfo sx
    ON
      sx.SexID = szF.SexID
    INNER JOIN
  		SampLocations sl
  	ON
  		szP.SampLocationID = sl.SampLocationID
  	INNER JOIN
  		[dbSTATpre].[dbo].SampUnitTypes su -- This might be an issue!!!
  	ON
  		szP.SampleUnitID = su.SampUnitTypeID
    WHERE
      si.SizeInfoCode = '", type, "' AND
    	--fg.FlagCode = '", reporting_flag, "' AND
      --szP.YearC BETWEEN ", year_from, " AND ", year_to, " AND
    	--sp.SpeciesCode = '" , species, "' AND
    	-- Here we shall have the 'product type' filter, but this information doesn't seem to be available...
    	--sl.SampLocationCode = '", sampling_location, "' AND
    	--su.SampUnitTypeCode = '", sampling_unit, "' AND
    	--frqt.FreqTypeCode = '", frequency_type, "' AND
    	--cl.SzClassLimitCode = '", class_limit, "' AND
    	--szP.SzInterval = '", size_interval, "' AND
    	--szP.Raised_SUnit = ", ifelse(raised, 1, 0), " AND
    	1 = 1
    "
  )

  return(
    tabular_query(
      statement = SQL_SZ,
      connection = DB_STAT(server = "ICARO\\SQL16")
    )
  )
}

filter_SZ = function(SZ_all,
                     reporting_flag, year_from, year_to,
                     species, product_type,
                     sampling_location, sampling_unit, raised,
                     frequency_type, size_interval, class_limit) {
  SZ_filtered =
    SZ_all[FlagCode == reporting_flag &
             Year >= year_from &
             Year <= year_to &
             SpeciesCode == species &
             # No product type in the DB...
             SampLocationCode == sampling_location &
             SampUnitTypeCode == sampling_unit &
             Raised == ifelse(raised, 1, 0) &
             FreqTypeCode == frequency_type &
             SzInterval == size_interval &
             SzClassLimitCode == class_limit]

  SZ_filtered$FlagCode         = NULL
  SZ_filtered$SpeciesCode      = NULL
  SZ_filtered$SampLocationCode = NULL
  SZ_filtered$SampUnitTypeCode = NULL
  SZ_filtered$Raised           = NULL
  SZ_filtered$FreqTypeCode     = NULL
  SZ_filtered$SzInterval       = NULL
  SZ_filtered$SzClassLimitCode = NULL

  SZ_filtered$Sex = factor(
    SZ_filtered$Sex,
    labels = c("U", "M", "F", "I"),
    levels = c("U", "M", "F", "I"),
    ordered = TRUE
  )

  SZ_filtered = SZ_filtered[order(Year, MonthCatch, FleetSuffix, Gear, SquareType, Quadrant, Lat, Lon, SchoolType, CatchType, DataSource, SizeClass)]

  strata = unique(SZ_filtered$SampleUnitID)

  strata_mapping = data.table(STRATA_ID_ORIG = strata, STRATA_ID = 1:length(strata))

  SZ_data =
    dcast.data.table(
      SZ_filtered,
      FlagVessel + FleetSuffix + Year + MonthCatch + Gear + CatchType + SchoolType + SamplingArea + SquareType + Quadrant + Lat + Lon + DataSource + TotalCatch + SampleUnitID + MonthSamp + SampledWeightKg + SampledNumbers + SizeClass ~ Sex,
      fun.aggregate = function(v) { return(sum(v, na.rm = TRUE)) },
      fill = NA,
      drop = c(TRUE, FALSE),
      value.var = "Nr"
    )

  SZ_data =
    merge(
      SZ_data, strata_mapping,
      by.x = "SampleUnitID", by.y = "STRATA_ID_ORIG",
      all.x = TRUE
    )[order(STRATA_ID, FlagVessel, FleetSuffix, Year, MonthCatch, Gear, CatchType, SchoolType, SamplingArea, SquareType, Quadrant, Lat, Lon, DataSource, MonthSamp, SampledWeightKg, SampledNumbers, SizeClass)]

  return(
    SZ_data[, .(FlagVessel, FleetSuffix, Year, MonthCatch, Gear, CatchType, SchoolType, SamplingArea, SquareType, Quadrant, Lat, Lon, DataSource, TotalCatch, SampleUnitID = paste0("Strata#", STRATA_ID), MonthSamp, SampledWeightKg, SampledNumbers, SizeClass, U, M, `F`, I)]
  )
}

export_ST04 = function(SZ_all,
                       statistical_correspondent = NULL,
                       version_reported = "Final", content_type = "Revision (FULL)",
                       reporting_flag, year_from, year_to,
                       species, product_type,
                       sampling_location, sampling_unit, raised,
                       frequency_type, size_interval, class_limit,
                       template_file = "./refs/ST04-T2SZ.xlsx",
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

  main_sheet = "ST04-T2SZ"

  # DEPENDENCIES FROM REF_FLAGS (which is exported by iccat.pub.data)

  country           = REF_FLAGS[CODE == statistical_correspondent$country]$NAME_EN
  reporting_country = REF_FLAGS[CODE == reporting_flag]$NAME_EN

  SZ_filtered = filter_SZ(SZ_all,
                          reporting_flag, year_from, year_to,
                          species, product_type,
                          sampling_location, sampling_unit, raised == "Yes",
                          frequency_type, size_interval, class_limit)

  if(nrow(SZ_filtered) == 0)
    stop(paste0("No T2SZ data found for ", reporting_flag, " (", year_from, "-", year_to, ")"))

  xls_template = wb_load(template_file)

  xls_template$add_data(sheet = main_sheet, paste0("Automatically generated from ICCAT T2SZ current data on ", Sys.time()), start_row = 12, start_col = 11, col_names = FALSE)

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

  xls_template$add_data(sheet = main_sheet, species,           start_row = 14, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, product_type,      start_row = 15, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, sampling_location,           start_row = 13, start_col = 8, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, sampling_unit,               start_row = 14, start_col = 8, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, ifelse(raised, "Yes", "No"), start_row = 15, start_col = 8, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, version_reported,  start_row = 17, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, content_type,      start_row = 18, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, frequency_type,    start_row = 17, start_col = 8, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, size_interval,     start_row = 18, start_col = 8, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, class_limit,       start_row = 19, start_col = 8, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, SZ_filtered, start_row = 27, col_names = FALSE, with_filter = FALSE, na.strings = "")

  num_rows = nrow(SZ_filtered)
  max_row = max(24, num_rows)

  if(max_row > 24) {
    xls_template$update_table(sheet = main_sheet, tabname = "tblT2SZ", dims = paste0("A26:W", 26 + max_row))

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "A27:W27")

    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("A51:W", 26 + num_rows), style = styles)
  }


  if(is.na(destination_file))
    destination_file = paste0("./out/ST04-T2SZ_", reporting_flag, "_", year_from, "_", year_to, "_", data_source, ".xlsx")

  wb_save(xls_template, destination_file, overwrite = TRUE)
}
