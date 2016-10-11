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


#' YSI Taxable Income Standalone Function
#'
#' This function just finds taxable income.
#'
#' @param df Argument is intended to be a fresh HILDA data frame
#' @return A data frame
#' @export

ysi_tax_inc <- function(df = NULL) {
  temp <- ysi_mutate_combine(df)
  temp$Deduct <- sapply(temp$RegInc, DEDfn)
  temp <- ysi_tax_inc_mut(temp)

  return(temp)
}






ysi_tax_inc_mut <- function(df = NULL) {

  mutate(df, TaxInc=RegInc-Super_Inc-PrivTran-PubTransImp-ScholarshipsImp-SalSac_MainImp*52-SalSac_OtherImp*52-Deduct)

}

#' YSI test func
#'
#' This function just finds taxable income.
#'
#' @param x and integer
#' @return An element
#' @export


new_func <- function(x = x) {
  y  <- x^2
  return(y)
}
