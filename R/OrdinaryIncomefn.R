#' Age Pension Assessable income function
#'
#' (Jackson to complete...)
#'
#' @name OrdinaryIncomefn
#' @param df Argument is HHtbl from the master age pension function
#' @return A data frame
#' @export
#'


OrdinaryIncomefn <- function(HHtbl=NULL){
  ## Dummy the individual as partnered if in a couple and there is another person in the family in the same situation
  HHtbl <- HHtbl %>% group_by(PersonID) %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0))
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(NoCoupled=sum(Coupled))
  HHtbl <- HHtbl %>% mutate(Partnered=(Coupled==1 & NoCoupled>1),
                            Single=(Partnered==FALSE))
  ## Adjust wages and salaries income for income concession
  #HHtbl <- HHtbl %>% group_by(PersonID) %>%
  #  mutate(AdjWagesFt=ifelse(RepWagesSalaryWkly*2>250,RepWagesSalaryWkly*2-250,RepWagesSalaryWkly*2))
  # Or using imputed
  HHtbl <- HHtbl %>% mutate(AdjWagesFt=ifelse(WageIncImp/26>250,WageIncImp/26-250,WageIncImp/26))

  HHtbl$UnincBusinessInc[which(is.na(HHtbl$UnincBusinessInc))] <- 0
  HHtbl$ForeignPen[which(is.na(HHtbl$ForeignPen))] <- 0
  HHtbl$OtherIrregPay[which(is.na(HHtbl$OtherIrregPay))] <- 0
  HHtbl$Rent[which(is.na(HHtbl$Rent))] <- 0

  # Not living in a nursing home
  HHtbl$DwellingType[which(is.na(HHtbl$DwellingType))] <- 99
  HHtbl <- HHtbl %>% mutate(NotNursingHome=as.numeric(DwellingType!=1))

  HHtbl <- HHtbl %>% group_by(PersonID) %>%
    mutate(Personal_Income=max(AdjWagesFt*26 + UnincBusinessInc + WorkCompImp + ForeignPen  + OtherIrregPay + Rent*NotNursingHome,0))

  ## Generate a variable for the income of members of a couple where both are over 65 and in the same "family",
  ## then create an income variable conditional on this being true
  HHtbl <- HHtbl %>% mutate(CoupleMemberIncome=Coupled*Personal_Income)
  ## then sum together income of those in a couple
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>%
    mutate(Ordinary_Income=ifelse(NoCoupled>1,sum(CoupleMemberIncome),Personal_Income))
  return(HHtbl)
}
