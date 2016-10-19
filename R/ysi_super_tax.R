#' YSI Super Tax Assesment Function (Working...)
#'
#' This function combines all the nessesary Superannuation tax functions and data set mutations to be an all-in-one function for calculating Super Tax
#' (Requires Tax_Matrix set)
#' (Currently only usable in Wave14)

#' @name ysi_super_tax
#' @param df Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @return A data frame
#' #' @export
#
# ysi_super_tax <- function(df = NULL){
#   temp <- df
#   all_cols <- colnames(temp) # Should check changing all na's to zeros... is that a dangerous change?
#   temp <- temp %>%
#     mutate_each_(funs(na_to_zero), all_cols) %>%
#     mutate_each_(funs(as.numeric), all_cols) %>%
#     mutate(
#       Age_30June13 = Age - 1,
#       WageInc_ExSalSac = WageIncImp_SalSac - SalSacImp,
#       TotalIndIncTax = IndTax + IndMedLevy,   # to calculate how much tax each person should theoretically pay on taxable income  <- unnecessary??
#
#       EmpCont = mapply(EmpSuperCont_fn, EmployerBusinessSuperContributions,
#                        EmployerContribution,
#                        FreqSuperEmployerContribution,
#                        WageInc_ExSalSac,
#                        EmployerContribution_PercentWage)
#
#       SuperTbl$EmpCont <- mapply(EmpSuperCont_fn,
#                                  SuperTbl$EmployerBusinessSuperContributions,
#                                  SuperTbl$EmployerContribution,
#                                  SuperTbl$FreqSuperEmployerContribution,
#                                  SuperTbl$WageInc_ExSalSac,
#                                  SuperTbl$EmployerContribution_PercentWage)
#
#
#       )
#
# }
#

#==============================================================================================================================

# Superannuation broken into:

# 1. Contributions to super
# 1.1. Concessional Contributions
# 1.2. Non-Concessional Contributions
# 1.3. Govt Contributions

# 2. Earnings in super

# 3. Payouts from super

#==============================================================================================================================

## 1.1. Concessional contributions to super

#======================================================

# 1.1.1. Employer contributions to super fund
# applied amount stated in survey; if NA
# applied rate of employer contribution stated in survey; if NA
# applied 9.25% rate, noting employers dont have to pay super on income above $48,040 per quarter

EmpSuperCont_fn_2 <- function(DoesEmpMakeCont = NULL,
                              ContMadeByEmp = NULL,
                              FreqEmpCont = NULL,
                              Wage = NULL,
                              EmpContPerWage = NULL) {

  if (DoesEmpMakeCont == 1 & FreqEmpCont > 0) {
  if (FreqEmpCont == 1) {
    EmpCont <- ContMadeByEmp * 52
    return(EmpCont)
  } else if (FreqEmpCont == 2) {
    EmpCont <- ContMadeByEmp * 26
    return(EmpCont)
  } else if (FreqEmpCont == 3) {
    EmpCont <- ContMadeByEmp * 6.5
    return(EmpCont)
  } else if (FreqEmpCont == 4) {
    EmpCont <- ContMadeByEmp
    return(EmpCont)
  } else {
    return(0)
  }
} else if (DoesEmpMakeCont == 1 & FreqEmpCont == 0) {
  if (EmpContPerWage > 0) {
    if (Wage < 48040 * 4) {
      EmpCont <- Wage * (EmpContPerWage / 100)
      return(EmpCont)
    } else if (Wage >= 48040 * 4) {
      EmpCont <- (48040 * 4) * (EmpContPerWage / 100)
      return(EmpCont)
    }
  } else if (EmpContPerWage <= 0) {
    if (Wage < 48040 * 4) {
      EmpCont <- Wage * 0.0925
      return(EmpCont)
    } else if (Wage >= 48040 * 4) {
      EmpCont <- (48040 * 4) * 0.0925
      return(EmpCont)
    }}} else {
      return(0)
    }
  }

EmpSuperCont_fn <- function(DoesEmpMakeCont = NULL,
                            ContMadeByEmp = NULL,
                            FreqEmpCont = NULL,
                            Wage = NULL,
                            EmpContPerWage = NULL) {

  if (DoesEmpMakeCont == 1 & FreqEmpCont > 0) {
  if (Wage < 48040 * 4) {
    EmpCont <- Wage * 0.0925
    return(EmpCont)
  } else if (Wage >= 48040 * 4) {
    EmpCont <- (48040 * 4) * 0.0925
    return(EmpCont)
  }} else {
    return(0)
  }
 }


#======================================================











