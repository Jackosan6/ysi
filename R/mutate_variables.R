#' ysi Mutate_combine Function
#'
#' This function is a standalone function that mutates multiple useful and commonly used variable for YSI analysis.
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

ysi_mutate_combine <- function(df) {

  mutate(df, HouseInc = HHTotInc_p - HHTotInc_n,
               TotalIncome = TotalInc_p-TotalInc_n,
               RegInc=RegIncImp_p-RegIncImp_n,
               Assets= HHAsset_p- HHAsset_n,
               NetRent = Rent_p - Rent_n)
}
