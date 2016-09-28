#' Super Tax part of Income Tax
#'
#' Super Tax Based on Super income and age, for apply to HILDA for Income tax calculation
#'
#' @param Super_Inc Whole dollar value of super income
#' @param Age An interger of the age of the tax payer
#' @return A list
#' @export



#Calculating tax on Super payments
SUPERTAXfn <- function(Super_Inc, Age) {
  if (Super_Inc <  100697) {
    Supertax <- 0
  } else if(Super_Inc > 100696) {
    if (Age > 59){
      Supertax <- 0
    } else if (Age < 55) {
      if (Super_Inc %in% 80001:180000){
        Supertax <- 0.37*Super_Inc
      } else if (Super_Inc > 180000) {
        Supertax <- 0.47*Super_Inc
      }
    } else if (Age %in% 55:58) {
      if (Super_Inc %in% 80001:180000){
        Supertax <- 0.22*Super_Inc
      } else if (Super_Inc > 180000) {
        Supertax <- 0.30*Super_Inc
      }
    }
  }
}
