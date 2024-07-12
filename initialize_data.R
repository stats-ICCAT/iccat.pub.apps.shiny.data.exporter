library(iccat.dev.data)

FC   = ST01A.load_data_vessels()
FC_f = ST01A.load_data_fisheries()

save("FC",   file = "./shiny/data/FC.RData", compress = "gzip")
save("FC_f", file = "./shiny/data/FC_f.RData", compress = "gzip")

FCG   = ST01B.load_data_vessels()
FCG_f = ST01B.load_data_fisheries()

save("FCG",   file = "./shiny/data/FCG.RData", compress = "gzip")
save("FCG_f", file = "./shiny/data/FCG_f.RData", compress = "gzip")

NC = ST02.load_data()
save("NC", file = "./shiny/data/NC.RData", compress = "gzip")

EF = ST03.load_data_EF()
save("EF", file = "./shiny/data/EF.RData", compress = "gzip")

CA = ST03.load_data_CA()
save("CA", file = "./shiny/data/CA.RData", compress = "gzip")

SZ = ST04.load_data("siz")
save("SZ", file = "./shiny/data/SZ.RData", compress = "gzip")

CS = ST05.load_data("cas")
save("CS", file = "./shiny/data/CS.RData", compress = "gzip")

META = list(LAST_UPDATE = "2024-07-04")
save("META", file = "./shiny/data/META.RData", compress = "gzip")
