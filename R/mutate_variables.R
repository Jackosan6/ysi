#' ysi Mutate_combine Function
#'
#' This function mutates multiple useful and commonly used variables for YSI analysis.
#'
#' It generates the following, which are mostly a sum of the posative and negative HILDA variables:
#'
#' House Income (HouseInc)
#' Total Individual Income (TotalIncome)
#' Regular Individual Income (RegInc)
#' HouseHold level combined Assets (Assets)
#' Idividual Net Rent (NetRent)
#'
#' @param df Argument is intended to be a fresh HILDA data frame
#' @return A data frame
#' @export

ysi_mutate_combine <- function(df= NULL) {

  mutate(df, HouseInc = HHTotInc_p - HHTotInc_n,
               TotalIncome = TotalInc_p-TotalInc_n,
               RegInc=RegIncImp_p-RegIncImp_n,
               Assets= HHAsset_p- HHAsset_n,
               NetRent = Rent_p - Rent_n)
}

#' YSI Taxable Income Mutate Function
#'
#' This function mutates Taxable Income (TaxInc), a commonly used variable for YSI analysis as part of Income Tax Assesment
#'
#' It generates taxable income as a variable. Dependent on having used:
#'
#' ysi_mutate_combine()
#' DEDfn()
#'
#' @param df Argument is intended to be an edited HILDA data frame
#' @return A data frame
#' @export

ysi_tax_inc_mut <- function(df = NULL) {

  mutate(df, TaxInc=RegInc-Super_Inc-PrivTran-PubTransImp-ScholarshipsImp-SalSac_MainImp*52-SalSac_OtherImp*52-Deduct)

}
