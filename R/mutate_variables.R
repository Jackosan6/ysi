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

# HHtbl <- HHtbl %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0), #These should be checked by someone that they're appropriate for income tax
#                           NonDepOver25=(Age>=25 & Family!=9))
# HHtbl <- HHtbl %>% group_by(HouseID) %>% mutate(NonDepOver25s=sum(as.numeric(Age>=25 & Family!=9)))
# HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(NoCoupled=sum(Coupled))
# HHtbl <- HHtbl %>% mutate(Partnered=(Coupled==1 & NoCoupled>1),
#                           Single=(Partnered==FALSE))

ysi_mutate_combine <- function(df = NULL) {

  # df <- df %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0), #These should be checked by someone that they're appropriate for income tax
  #                           NonDepOver25=(Age>=25 & Family!=9))
  # HHtbl <- HHtbl %>% group_by(HouseID) %>% mutate(NonDepOver25s=sum(as.numeric(Age>=25 & Family!=9)))
  # HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(NoCoupled=sum(Coupled))
  # HHtbl <- HHtbl %>% mutate(Partnered=(Coupled==1 & NoCoupled>1),
  #                           Single=(Partnered==FALSE))

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

