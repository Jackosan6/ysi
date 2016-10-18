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



ysi_mutate_combine <- function(df = NULL) {


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

#' Family Status Variables
#'
#' This function mutates variables for coupled, Non dependents over 25, Number coupled, Partnered and Single.
#' (Coupled,NonDepOver25, NoCoupled, Partnered, Single)
#'
#' @param df Argument is intended to be a fresh HILDA data frame
#' @return A data frame
#' @export

ysi_family_mut <- function(df = NULL) {

  df <- df %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0), #These should be checked by someone that they're appropriate for income tax
                            NonDepOver25=(Age>=25 & Family!=9))
  df <- df %>% group_by(HouseID) %>% mutate(NonDepOver25s=sum(as.numeric(Age>=25 & Family!=9)))
  df <- df %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(NoCoupled=sum(Coupled))
  df <- df %>% mutate(Partnered=(Coupled==1 & NoCoupled>1),
                            Single=(Partnered==FALSE))
  return(df)
}

