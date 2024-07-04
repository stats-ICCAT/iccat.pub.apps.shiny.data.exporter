load_CS = function(type = 'cas',
                   reporting_flag, year_from, year_to,
                   species,
                   frequency_type, size_interval, class_limit) {
  SQL_CS = paste0("
  	SELECT
  	  fg.FlagCode,
  	  sp.SpeciesCode,
  	  frqt.FreqTypeCode,
  	  cl.SzClassLimitCode,
  	  szP.SzInterval,
      ft.FlagOfVesselCode as FlagVessel,
      ft.FleetSuffix,
      szP.YearC AS Year,
      tpc.TimePeriodID AS Month,
      gr.GearCode AS Gear,
      CASE
        WHEN szP.GearID IN (33, 35, 37, 38, 39, 40, 80, 81, 82, 83, 84) THEN 'DD'
        ELSE 'L'
      END AS CatchType,
      st.SchoolTypeCode AS SchoolType,
      sa.SampAreaCode AS SamplingArea,
      CASE
        WHEN szS.QuadID = 1 THEN 'NE'
        WHEN szS.QuadID = 2 THEN 'SE'
        WHEN szS.QuadID = 3 THEN 'SW'
        WHEN szS.QuadID = 4 THEN 'NW'
      END AS Quadrant,
      FLOOR(szS.Lat) AS Lat,
      FLOOR(szS.Lon) AS Lon,
      szS.CatchWGT AS TotalCatchWgt, -- Total catch
      szS.CatchNUM AS TotalCatchNum,
      szS.NrSamples AS NumSamples,
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
      SexInfo sx
    ON
      sx.SexID = szF.SexID
    WHERE
      si.SizeInfoCode = '", type, "' AND
    	--fg.FlagCode = '", reporting_flag, "' AND
      --szP.YearC BETWEEN ", year_from, " AND ", year_to, " AND
    	--sp.SpeciesCode = '" , species, "' AND
    	--frqt.FreqTypeCode = '", frequency_type, "' AND
    	--cl.SzClassLimitCode = '", class_limit, "' AND
    	--szP.SzInterval = '", size_interval, "' AND
    	1 = 1
    "
  )

  return(
    tabular_query(
      statement = SQL_CS,
      connection = DB_STAT(server = "ICARO\\SQL16")
    )
  )
}

filter_CS = function(CS_all,
                     reporting_flag, year_from, year_to,
                     species,
                     frequency_type, size_interval, class_limit) {
  CS_filtered =
    CS_all[FlagCode == reporting_flag &
             Year >= year_from &
             Year <= year_to &
             SpeciesCode == species &
             # No product type in the DB...
             FreqTypeCode == frequency_type &
             SzInterval == size_interval &
             SzClassLimitCode == class_limit]

  CS_filtered$FlagCode         = NULL
  CS_filtered$SpeciesCode      = NULL
  CS_filtered$FreqTypeCode     = NULL
  CS_filtered$SzInterval       = NULL
  CS_filtered$SzClassLimitCode = NULL

  CS_filtered$Sex = factor(
    CS_filtered$Sex,
    labels = c("U", "M", "F", "I"),
    levels = c("U", "M", "F", "I"),
    ordered = TRUE
  )

  CS_filtered = CS_filtered[order(Year, Month, FleetSuffix, Gear, Quadrant, Lat, Lon, SchoolType, CatchType, SizeClass)]

  CS_data =
    dcast.data.table(
      CS_filtered,
      FlagVessel + FleetSuffix + Year + Month + Gear + CatchType + SchoolType + SamplingArea + Quadrant + Lat + Lon + TotalCatchWgt + TotalCatchNum + NumSamples + SampledWeightKg + SampledNumbers + SizeClass ~ Sex,
      fun.aggregate = function(v) { return(sum(v, na.rm = TRUE)) },
      fill = NA,
      drop = c(TRUE, FALSE),
      value.var = "Nr"
    )[order(FlagVessel, FleetSuffix, Year, Month, Gear, CatchType, SchoolType, SamplingArea, Quadrant, Lat, Lon, TotalCatchWgt, TotalCatchNum, NumSamples, SampledWeightKg, SampledNumbers, SizeClass)]

  return(
    CS_data[, .(FlagVessel, FleetSuffix, Year, Month, Gear, CatchType, SchoolType, SamplingArea, Quadrant, Lat, Lon, TotalCatchWgt, TotalCatchNum, NumSamples, SampledWeightKg, SampledNumbers, SizeClass, U, M, `F`, I)]
  )
}

export_ST05 = function(CS_all,
                       statistical_correspondent = NULL,
                       version_reported = "Final", content_type = "Revision (FULL)",
                       reporting_flag, year_from, year_to,
                       species,
                       frequency_type, size_interval, class_limit,
                       template_file = "./refs/ST05-T2CS.xlsx",
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

  main_sheet = "ST05-T2CS"

  # DEPENDENCIES FROM REF_FLAGS (which is exported by iccat.pub.data)

  country           = REF_FLAGS[CODE == statistical_correspondent$country]$NAME_EN
  reporting_country = REF_FLAGS[CODE == reporting_flag]$NAME_EN

  CS_filtered = filter_CS(CS_all,
                          reporting_flag, year_from, year_to,
                          species,
                          frequency_type, size_interval, class_limit)

  if(nrow(CS_filtered) == 0)
    stop(paste0("No T2CS data found for ", reporting_flag, " (", year_from, "-", year_to, ")"))

  xls_template = wb_load(template_file)

  xls_template$add_data(sheet = main_sheet, paste0("Automatically generated from ICCAT T2CS current data on ", Sys.time()), start_row = 12, start_col = 11, col_names = FALSE)

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
  #xls_template$add_data(sheet = main_sheet, product_type,      start_row = 15, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, version_reported,  start_row = 17, start_col = 3, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, content_type,      start_row = 18, start_col = 3, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, frequency_type,    start_row = 16, start_col = 8, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, size_interval,     start_row = 17, start_col = 8, col_names = FALSE)
  xls_template$add_data(sheet = main_sheet, class_limit,       start_row = 18, start_col = 8, col_names = FALSE)

  xls_template$add_data(sheet = main_sheet, CS_filtered, start_row = 27, col_names = FALSE, with_filter = FALSE, na.strings = "")

  num_rows = nrow(CS_filtered)
  max_row = max(24, num_rows)

  if(max_row > 24) {
    xls_template$update_table(sheet = main_sheet, tabname = "tblST05", dims = paste0("A26:U", 26 + max_row))

    styles = xls_template$get_cell_style(sheet = main_sheet, dims = "A27:U27")

    xls_template$set_cell_style(sheet = main_sheet, dims = paste0("A51:U", 26 + num_rows), style = styles)
  }


  if(is.na(destination_file))
    destination_file = paste0("./out/ST05-T2CS_", reporting_flag, "_", year_from, "_", year_to, "_", data_source, ".xlsx")

  wb_save(xls_template, destination_file, overwrite = TRUE)
}
