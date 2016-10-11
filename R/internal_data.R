

# // Internal data tables for YSI tax functions //


# make sure that you

library(data.table)
library(devtools)

lito_tbl <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/ysi package data/lito-info")

sapto_tbl <- data.table::fread("./data/sapto-tbl.tsv")

medicare_tbl <- data.table::fread("./data/medicare-tbl.tsv")

income_tax_rates_tbl <- data.table::fread("./data/income-tax-rates-tbl.tsv")

# Writes all data sets to package
use_data(lito_tbl, sapto_tbl, medicare_tbl, income_tax_rates_tbl,

         internal = T)


