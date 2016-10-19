# Internal Data for tax functions
#
# library(data.table)
# library(devtools)
#
# package data into ysi
# use_data(lito_tbl, sapto_tbl, medicare_tbl, income_tax_rates_tbl, APEnergySuppRates,APMaxBasicRate,APMeansTest,APPensionSuppRates,DeemingRates,
#
#          internal = F, overwrite =  T) # If changing internal to FALSE then back to TRUE delete the rda files

#
# write.table(lito_tbl, "./data/lito_tbl.tsv", row.names = F)
# write.table(sapto_tbl, "./data/sapto_tbl.tsv", row.names = F)
# write.table(medicare_tbl, "./data/medicare_tbl.tsv", row.names = F)
# write.table(income_tax_rates_tbl, "C:/Users/User/Documents/ysi/data/income_tax_rates_tbl_old.tsv", row.names = F)
#
#
# lito_tbl <- fread("C:/Users/User/Documents/ysi/data/lito_tbl.tsv", header = T)
#
# sapto_tbl <- fread("C:/Users/User/Documents/ysi/data/sapto_tbl.tsv", header = T)
#
# medicare_tbl <- fread("C:/Users/User/Documents/ysi/data/medicare_tbl.tsv", header = T)
#
# income_tax_rates_tbl <- fread("C:/Users/User/Documents/ysi/data/income_tax_rates_tbl.tsv", header = T)


# income_tax_rates_tbl <- read.csv("C:/Users/User/Documents/ysi/data/income_tax_rates_tbl.csv", header = T)
#
#
# write.csv(income_tax_rates_tbl, "./data/income_tax_rates_tbl.csv", row.names = F)
# medicare_tbl <- filter(medicare_tbl, sato == F)
# income_tax_rates_tbl <- filter(income_tax_rates_tbl, sato == F)
# sapto_tbl <- filter(sapto_tbl, family_status == "single")

# Jacksons Age pensions tables
# APEnergySuppRates <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/HILDA Housing Share Files/Government Benefits/Age Pension/Age Pension function/APEnergySuppRates.csv", header = T)
# APMaxBasicRate <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/HILDA Housing Share Files/Government Benefits/Age Pension/Age Pension function/APMaxBasicRate.csv", header = T)
# APMeansTest <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/HILDA Housing Share Files/Government Benefits/Age Pension/Age Pension function/APMeansTest.csv", header = T)
# APPensionSuppRates <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/HILDA Housing Share Files/Government Benefits/Age Pension/Age Pension function/APPensionSuppRates.csv", header = T)
# DeemingRates <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/HILDA Housing Share Files/Government Benefits/Age Pension/Age Pension function/DeemingRates.csv", header = T)
# RARates <- fread("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Debt_housing/HILDA Housing Share Files/Government Benefits/Age Pension/Age Pension function/RARates.csv", header = T)
#
# write.table(APEnergySuppRates, "C:/Users/User/Documents/ysi/data/APEnergySuppRates.tsv", row.names = F)
# write.table(APMaxBasicRate, "C:/Users/User/Documents/ysi/data/APMaxBasicRate.tsv", row.names = F)
# write.table(APMeansTest, "C:/Users/User/Documents/ysi/data/APMeansTest.tsv", row.names = F)
# write.table(APPensionSuppRates, "C:/Users/User/Documents/ysi/data/APPensionSuppRates.tsv", row.names = F)
# write.table(DeemingRates, "C:/Users/User/Documents/ysi/data/DeemingRates.tsv", row.names = F)
# write.table(RARates, "C:/Users/User/Documents/ysi/data/RARates.tsv", row.names = F)


