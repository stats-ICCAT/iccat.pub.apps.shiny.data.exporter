library(iccat.dev.data)
library(iccat.pub.data)

library(openxlsx2)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

options(scipen = 9999)

REF_CONTENT_TYPES = data.table(CODE    = c("NF",         "NP",            "RF",              "RP"),
                               NAME_EN = c("New (FULL)", "New (PARTIAL)", "Revision (FULL)", "Revision (PARTIAL)"))

ALL_FLAGS = setNames(as.character(REF_FLAGS$CODE), paste0(REF_FLAGS$CODE, " - ", REF_FLAGS$NAME_EN))

ALL_VERSIONS      = c("Final", "Preliminary")
ALL_CONTENT_TYPES = setNames(as.character(REF_CONTENT_TYPES$CODE), paste0(REF_CONTENT_TYPES$CODE, " - ", REF_CONTENT_TYPES$NAME_EN))

source("./T1NC.R")
#source("./T2CE.R")
#source("./T2SZ.R")
#source("./T2CS.R")
