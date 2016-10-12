# Internal Data for tax functions

library(data.table)
library(devtools)

lito_tbl <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/ysi package data/lito-info.tsv", header = T)

sapto_tbl <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/ysi package data/sapto_tbl.tsv", header = T)

medicare_tbl <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/ysi package data/medicare_tbl.tsv", header = T)

income_tax_rates_tbl <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/ysi package data/tax-brackets-and-marginal-rates-by-fy.tsv", header = T)

write.table(lito_tbl, "./data/lito_tbl.tsv", row.names = F)
write.table(sapto_tbl, "./data/sapto_tbl.tsv", row.names = F)
write.table(medicare_tbl, "./data/medicare_tbl.tsv", row.names = F)
write.table(income_tax_rates_tbl, "./data/income_tax_rates_tbl.tsv", row.names = F)

#temp <- fread("./data/lito_tbl.tsv", header = T)


# package data into ysi
use_data(lito_tbl, sapto_tbl, medicare_tbl, income_tax_rates_tbl,

         internal = T, overwrite =  T) # If changing internal to FALSE then back to TRUE delete the rda files

income_tax_rates_tbl <- read.csv("./data/income_tax_rates_tbl.csv", header = T)


write.csv(income_tax_rates_tbl, "./data/income_tax_rates_tbl.csv", row.names = F)
