library(iccat.dev.data)
library(iccat.pub.data)

library(stringr)
library(openxlsx2)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

options(scipen = 9999)

build_dropdown_data = function(reference_codes) {
  return(
    setNames(
      as.character(reference_codes$CODE),
      paste0(reference_codes$CODE, " - ", reference_codes$NAME_EN)
    )
  )
}

ALL_VERSIONS      = c("Final", "Preliminary")

REF_CONTENT_TYPES = data.table(CODE    = c("NF",         "NP",            "RF",              "RP"),
                               NAME_EN = c("New (FULL)", "New (PARTIAL)", "Revision (FULL)", "Revision (PARTIAL)"))
ALL_CONTENT_TYPES = build_dropdown_data(REF_CONTENT_TYPES)

ALL_FLAGS = build_dropdown_data(REF_FLAGS)

ALL_DATA_SOURCES = build_dropdown_data(REF_DATA_SOURCES)

SP_TEMPERATE    = REF_SPECIES[SPECIES_GROUP       == "Temperate tunas"]
SP_TROPICAL     = REF_SPECIES[SPECIES_GROUP       == "Tropical tunas"]
SP_SMALL_TUNAS  = REF_SPECIES[SPECIES_GROUP_ICCAT == "Tunas (small)"]
SP_BILLFISH     = REF_SPECIES[SPECIES_GROUP       == "Billfishes"]
SP_MAJOR_SHARKS = REF_SPECIES[SPECIES_GROUP_ICCAT == "Sharks (major)"]
SP_OTHER_SHARKS = REF_SPECIES[SPECIES_GROUP_ICCAT == "Sharks (other)"]

SP_BILLFISH_SWO = REF_SPECIES[CODE == "SWO"]

ALL_SPECIES = list(
  "Temperate tunas" = setNames(as.character(SP_TEMPERATE$CODE),    paste0(SP_TEMPERATE$CODE,    " - ", SP_TEMPERATE$NAME_EN)),
  "Tropical tunas"  = setNames(as.character(SP_TROPICAL$CODE),     paste0(SP_TROPICAL$CODE,     " - ", SP_TROPICAL$NAME_EN)),
  "Small tunas"     = setNames(as.character(SP_SMALL_TUNAS$CODE),  paste0(SP_SMALL_TUNAS$CODE,  " - ", SP_SMALL_TUNAS$NAME_EN)),
  "Billfish"        = setNames(as.character(SP_BILLFISH$CODE),     paste0(SP_BILLFISH$CODE,     " - ", SP_BILLFISH$NAME_EN)),
  "Sharks (major)"  = setNames(as.character(SP_MAJOR_SHARKS$CODE), paste0(SP_MAJOR_SHARKS$CODE, " - ", SP_MAJOR_SHARKS$NAME_EN)),
  "Sharks (other)"  = setNames(as.character(SP_OTHER_SHARKS$CODE), paste0(SP_OTHER_SHARKS$CODE, " - ", SP_OTHER_SHARKS$NAME_EN))
)

CAS_SPECIES = list(
  "Temperate tunas" = setNames(as.character(SP_TEMPERATE$CODE),    paste0(SP_TEMPERATE$CODE,    " - ", SP_TEMPERATE$NAME_EN)),
  "Tropical tunas"  = setNames(as.character(SP_TROPICAL$CODE),     paste0(SP_TROPICAL$CODE,     " - ", SP_TROPICAL$NAME_EN)),
  "Billfish"        = setNames(as.character(SP_BILLFISH_SWO$CODE), paste0(SP_BILLFISH_SWO$CODE, " - ", SP_BILLFISH_SWO$NAME_EN))
)

ALL_PRODUCT_TYPES = build_dropdown_data(REF_PRODUCT_TYPES[CODE != "UN"])

ALL_SAMPLING_UNITS = build_dropdown_data(REF_SAMPLING_UNITS[CODE != "UNK"])
ALL_SAMPLING_LOCATIONS = build_dropdown_data(REF_SAMPLING_LOCATIONS[CODE != "UNK"])

ALL_FREQUENCY_TYPES    = build_dropdown_data(REF_FREQUENCY_TYPES[CODE %in% c("SFL",
                                                                             "CFL",
                                                                             "LD1",
                                                                             "SLJFL",
                                                                             "CLJFL",
                                                                             "EYF",
                                                                             "TLE",
                                                                             "WGT",
                                                                             "HGTW",
                                                                             "UNK")])

CAS_FREQUENCY_TYPES    = build_dropdown_data(REF_FREQUENCY_TYPES[CODE %in% c("SFL",
                                                                             "SLJFL")])
ALL_SIZE_INTERVALS = c(1, 2, 5)
ALL_CLASS_LIMITS   = build_dropdown_data(REF_SIZE_CLASS_LIMITS[CODE != "UN"])

load("./data/FC.RData")
load("./data/FC_f.RData")

load("./data/FCG.RData")
load("./data/FCG_f.RData")

load("./data/NC.RData")

load("./data/EF.RData")
load("./data/CA.RData")

load("./data/SZ.RData")

load("./data/CS.RData")
