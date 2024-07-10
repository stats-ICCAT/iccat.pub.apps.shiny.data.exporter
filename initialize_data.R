library(iccat.dev.data)

NC = ST02.load_data(NA, NA, NA)
save("NC", file = "./shiny/data/NC.RData", compress = "gzip")

EF = ST03.load_data_EF(NA, NA, NA, NA)
save("EF", file = "./shiny/data/EF.RData", compress = "gzip")

CA = ST03.load_data_CA(NA, NA, NA, NA)
save("CA", file = "./shiny/data/CA.RData", compress = "gzip")

SZ = ST04.load_data("siz", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
save("SZ", file = "./shiny/data/SZ.RData", compress = "gzip")

CS = ST05.load_data("cas", NA, NA, NA, NA, NA, NA, NA)
save("CS", file = "./shiny/data/CS.RData", compress = "gzip")

META = list(LAST_UPDATE = "2024-07-04")
save("META", file = "./shiny/data/META.RData", compress = "gzip")
