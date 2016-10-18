#' YSI Super Tax Assesment Function
#'
#' This function combines all the nessesary Superannuation tax functions and data set mutations to be an all-in-one function for calculating Super Tax
#' (Requires Tax_Matrix set)

#' @name ysi_super_tax
#' @param df Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @return A data frame
#' @export

# ysi_super_tax <- function( ){
#   temp <- temp %>% mutate_all(
#     Sapto = ifelse(is.na(Sapto), 0, Sapto))
#
#
#
#
#
# }
#
#
#
# Sapto = ifelse(is.na(Sapto), 0, Sapto))
#
# # Convert NAs to 0
# HILDA_Super[is.na(HILDA_Super)] <- 0
#
# # Convert variables to numeric
# HILDA_Super <- as.data.frame(mapply(as.numeric, HILDA_Super))
#
# # Super data frame
# SuperTbl <- select(HILDA_Super, HouseID, PersonID, PersonNo, IntDate, Age, Sex, NoInHouse, MarketInc_Imp_p, MarketInc_Imp_n, RegIncImp_p, RegIncImp_n, WageIncImp_SalSac, SalSacImp, SalSac_MainImp, SalSac_OtherImp, SalSac_Super_Received, SalSac_Super_Amount, SalSac_Super_Perct, PrivTran, PubTransImp, ScholarshipsImp, Super_Inc, SuperRet, Super, HSuperannuation, pSuperannuation_Retirees, pSuperannuation_Non_Retirees, Retired, SuperCapital, SuperCapital_ValueBracket, SuperCapital_Value, EmployeeStatus, CthPublicServant, CCS_member, PSSAP_member, PSS_member, EmployerBusinessSuperContributions,  EmployerContribution_PercentWage, EmployerContribution_Dollars, EmployerContribution, FreqSuperEmployerContribution, PersonalSuperContributionTypes, PersonalSuperContribution_PerctWage, PersonalSuperContribution_Dollars, PersonalSuperContribution, FreqSuperPersonalContribution, PersonalContribution_PrivateSuperFund, PersonalContribution_SuperFund, PersonallContributionPrivateSuper_Amount, FreqPersonalPrivateSuperContribution, PerctIncPrivateSuper, Partner, PartnerSuperContributions, PartnerSuperContributions_Amount, FreqPartnerSuperContributions,SuperFundValue,  EstSuperFundValue, LargestSuperFund)
# SuperTbl[is.na(SuperTbl)] <- 0
#
# # Determine age of everyone at 30 June 2013
# SuperTbl <- mutate(SuperTbl, Age_30June13 = Age - 1)
#
# # Wage and Salary excluding salary sacrifice
# SuperTbl <- mutate(SuperTbl, WageInc_ExSalSac = WageIncImp_SalSac - SalSacImp)
# SuperTbl <- mutate(SuperTbl, RegInc = RegIncImp_p - RegIncImp_n)
