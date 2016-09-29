#' Super Tax Function
#'
#' Super Tax function for use on HIDLA, as part of the Income Tax Assesment
#' For use with mapply()
#'
#' @param Super_Inc Whole dollar value of super income
#' @param Age The age of the tax payer
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
