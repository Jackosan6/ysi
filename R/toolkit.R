# toolkit

#' Sample view of a interesing cohort in set
#'
#' @name inspect_ysi
#' @param df A dataframe of interest
#' @return A data frame
#' @export

inspect_ysi <- function(df = NULL){
  select(df, PersonID, HouseID,PersonNo, Sex, Age, State, GCC,TotalInc_p, WageIncImp, MortgageRepay, HouseValTodayImp, HomeEquity, InvestInc_p,BusinessInc_p, Super, NoInHouse)
}
