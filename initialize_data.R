library(iccat.dev.data)

source("./shiny/T1NC.R")
source("./shiny/T2CE.R")
source("./shiny/T2SZ.R")
source("./shiny/T2CS.R")

NC = load_NC(NA, NA, NA)
save("NC", file = "./shiny/data/NC.RData", compress = "gzip")

EF = load_EF(NA, NA, NA, NA)
save("EF", file = "./shiny/data/EF.RData", compress = "gzip")

CA = load_CA(NA, NA, NA, NA)
save("CA", file = "./shiny/data/CA.RData", compress = "gzip")

SZ = load_SZ("siz", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
save("SZ", file = "./shiny/data/SZ.RData", compress = "gzip")

CS = load_CS("cas", NA, NA, NA, NA, NA, NA, NA)
save("CS", file = "./shiny/data/CS.RData", compress = "gzip")

META = list(LAST_UPDATE = "2024-07-04")
save("META", file = "./shiny/data/META.RData", compress = "gzip")
