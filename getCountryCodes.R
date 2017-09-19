
library(rvest)

pg = read_html("http://www.nationsonline.org/oneworld/country_code_list.htm")

pg_tbls = pg %>% html_table()
cntryCodes = pg_tbls[[3]]
cntryCodes = cntryCodes[cntryCodes[["Country or Area Name"]] != "",]
cntryCodes = cntryCodes[, c(-1)]

head(cntryCodes)

names(cntryCodes) = c("country", "iso_alpha_2_code", "iso_alpha_3_code", "iso_numeric_code")

write.csv(cntryCodes, "Data/cntryCodes.csv", row.names = FALSE)
