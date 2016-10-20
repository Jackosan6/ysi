#' YSI Super Tax Assesment Function (Working...)
#'
#' This function combines all the nessesary Superannuation tax functions and data set mutations to be an all-in-one function for calculating Super Tax
#' (Grace to fill in soon)
#' (Requires Tax_Matrix set)
#' (Currently only usable in Wave14)

#' @name ysi_super_tax
#' @param df Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @return A data frame
#' @export
#
ysi_super_tax <- function(df = NULL){

  all_cols <- colnames(df) # Should check changing all na's to zeros... is that a dangerous change?
  print("Determining Super Variables")
  temp <- df %>%
    mutate_each_(funs(na_to_zero), all_cols) %>%
    mutate_each_(funs(as.numeric), all_cols) %>%


    mutate(
      Age_30June13 = Age - 1,
      WageInc_ExSalSac = WageIncImp_SalSac - SalSacImp,
      TotalIndIncTax = IndTax + IndMedLevy,   # to calculate how much tax each person should theoretically pay on taxable income  <- obselete, does inocm tax make this??

    # run function of each bit

      EmpCont = mapply(EmpSuperCont_fn, EmployerBusinessSuperContributions, # is it this function on the second (2) function?
                       EmployerContribution,
                       FreqSuperEmployerContribution,
                       WageInc_ExSalSac,
                       EmployerContribution_PercentWage),

      SalSac_Super = mapply(SalSac_SuperCont_fn, SalSac_Super_Received, SalSac_Super_Amount, SalSac_Super_Perct, WageInc_ExSalSac),
      ConcessionalCont = EmpCont + SalSac_Super,
      PersonalCont = mapply(PersonalCont_Fn, PersonalSuperContributionTypes, PersonalSuperContribution, FreqSuperPersonalContribution, WageInc_ExSalSac, PersonalSuperContribution_PerctWage),

      PersonalPrivateCont = mapply(PrivateSuper_fn,
                                   PersonalContribution_PrivateSuperFund,
                                   PersonalContribution_SuperFund,
                                   PersonallContributionPrivateSuper_Amount,
                                   FreqPersonalPrivateSuperContribution,
                                   TaxableInc,
                                   PerctIncPrivateSuper),

      PartnerCont = mapply(PartnerCont_fn, PartnerSuperContributions, PartnerSuperContributions_Amount, FreqPartnerSuperContributions),

      AfterTaxCont = PersonalCont + PersonalPrivateCont + PartnerCont,
      Govt_CoCont_Inc = TaxableInc + ConcessionalCont,

      # need to check Wage variable
      Govt_CoCont = mapply(Govt_Co_Cont_fn, AfterTaxCont, Govt_CoCont_Inc, WageIncImp_SalSac, Age_30June13),
      # need to add more varibles
      LISC_Inc = TaxableInc,
      # to check wage variable
      LISC = mapply(LISC_fn, ConcessionalCont, LISC_Inc, WageIncImp_SalSac),

      # Govt Contributions to super
      GovtCont = Govt_CoCont + LISC,

      # Total Contributions to Super
      TotalSuperCont = EmpCont + PersonalCont + PersonalPrivateCont + PartnerCont + SalSac_Super + GovtCont
    )

    ## 1.4. Tax paid on contributions to super
    print("Calculating tax paid on contributions to super")
    # taxable income plus contributions to super above 25000
    temp <- temp %>% mutate(
      EmpContAbove25k = ConcessionalCont - 25000,
      EmpContECC_25k = EmpContAbove25k * 0.0566,
      EmpCont_Inc_25k = TaxableInc + EmpContAbove25k,
      EmpCont_Inc_25k_IncTax = mapply(income_tax_rates_func, EmpCont_Inc_25k, year = "2013-14"),
      EmpCont_Inc_25k_ML = mapply(MLfn, EmpCont_Inc_25k,fy_year = "2013-14"),
      EmpCont_Inc_25k_Tax = EmpCont_Inc_25k_IncTax + EmpCont_Inc_25k_ML,
      EmpCont_Inc_25k_TaxExcess = EmpCont_Inc_25k_Tax - TotalIndIncTax + EmpContECC_25k,

      #taxable income plus contributions to super above 35000
      EmpContAbove35k = ConcessionalCont - 35000,
      EmpContECC_35k = EmpContAbove35k * 0.0566,
      EmpCont_Inc_35k = TaxableInc + EmpContAbove35k,
      EmpCont_Inc_35k_IncTax = mapply(income_tax_rates_func, EmpCont_Inc_35k, year = "2013-14"),
      EmpCont_Inc_35k_ML = mapply(MLfn, EmpCont_Inc_35k, fy_year = "2013-14"),
      EmpCont_Inc_35k_Tax = EmpCont_Inc_35k_IncTax + EmpCont_Inc_35k_ML,
      EmpCont_Inc_35k_TaxExcess = EmpCont_Inc_35k_Tax - TotalIndIncTax + EmpContECC_35k,

      ConcessionalContTax = mapply(ContTax_fn, Age_30June13, ConcessionalCont, EmpCont_Inc_25k_TaxExcess, EmpCont_Inc_35k_TaxExcess)
    )

    #1.4.2
    print("Cont...")
    temp <- temp %>% mutate(
      SpouseInc = TaxableInc + ConcessionalCont,
      Partner_TaxOffset = as.numeric(mapply(PartnerTaxOffset_fn, SpouseInc, PartnerCont)),

      ## 2.1. Earnings in Super
      FundValue = as.numeric(Super + SuperRet),
      FundEarnings = FundValue * 0.05,      # As no earnings figure provided, applied rate of return to get earnings in the fund
      ## 2.2
      EarningsTax = mapply(EarningsTax_fn, Age_30June13, FundEarnings, Super_Inc),
      ##3.2
      TaxInc_SuperInc = TaxableInc + Super_Inc,
      TaxInc_SuperInc_Tax = mapply(income_tax_rates_func, TaxInc_SuperInc,year = "2013-14"),
      TaxInc_SuperInc_MDLevy = mapply(MLfn, TaxInc_SuperInc ,fy_year = "2013-14"),

      SuperPayoutTax = TaxInc_SuperInc_Tax + TaxInc_SuperInc_MDLevy - TotalIndIncTax,        # Tax paid on super, assuming
      PayoutTax = mapply(SuperPayoutTax_fn, Age_30June13, SuperPayoutTax, Super_Inc),
      #Div 293
      Div293Inc = TaxableInc + ConcessionalCont,
      # Income and Contributions above $300,000
      AmtAbv300k =  Div293Inc - 300000,
      Div293Tax = mapply(Div293Tax_fn, Div293Inc, ConcessionalCont, AmtAbv300k),

      #==============================================================================================================================

      # Total Tax
      TotalSuperTax = ConcessionalContTax - Partner_TaxOffset + EarningsTax + PayoutTax + Div293Tax - GovtCont,
      TotalTax = TotalIndIncTax + TotalSuperTax





    )
    return(temp)
    print("Done, see variable -> $TotalSuperTax")

}

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

EmpSuperCont_fn <- function(DoesEmpMakeCont, ContMadeByEmp, FreqEmpCont, Wage, EmpContPerWage) { if (DoesEmpMakeCont == 1 & FreqEmpCont > 0) {
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
    }}


#======================================================

# 1.1.2. Salary sacrifice contributions to super
# applied amount stated in survey; if NA
# applied rate of employer contribution stated in survey

SalSac_SuperCont_fn <- function(Received, Amount, Perct, Wage) { if (Received == 1) {
  if (Amount > 0) {
    SuperCont <- Amount
    return(Amount)
  } else if (Amount <= 0) {
    if (Perct > 0) {
      SuperCont <- Wage * (Perct / 100)
      return(SuperCont)
    } else {
      return(0)
    }
  }
} else {
  return(0)
}}




#======================================================

# 1.1.3. Work test for those aged 65-74 who want to make super contributions
#

#======================================================

# Concessional Contributions to Super
#SuperTbl <- mutate(SuperTbl, ConcessionalCont = EmpCont + SalSac_Super)

#======================================================

## 1.2. Non-concessional contributions

#======================================================

# 1.2.1. Personal Contribution to Super Fund
# applied amount stated in survey; if NA
# applied rate of employer contribution stated in survey

PersonalCont_Fn <- function(DoesMakeCont, ContAmount, FreqCont, Wage, ContPerctWage) {if (DoesMakeCont == 1 | DoesMakeCont == 2) {
  if (FreqCont == 1) {
    (PerCont <- ContAmount * 52)
    return(PerCont)
  } else if (FreqCont == 2) {
    (PerCont <- ContAmount * 26)
    return(PerCont)
  } else if (FreqCont == 3) {
    (PerCont <- ContAmount * 6.5)
    return(PerCont)
  } else if (FreqCont == 4) {
    (PerCont <- ContAmount * 1)
    return(PerCont)
  } else if (FreqCont == 0) {
    if (ContPerctWage > 0) {
      (PerCont <- Wage * (ContPerctWage / 100))
      return(PerCont)
    } else {
      return(0)
    }
  }
} else {
  return(0)
}}


#======================================================

# 1.2.2. Personal contributions to private super fund
# applied amount stated in survey; if NA
# applied rate of employer contribution stated in survey

PrivateSuper_fn <- function(MakesPrivateCont, MakesPersonalCont, ContAmount, FreqCont, Inc, PerctInc) { if ((MakesPrivateCont == 1 | MakesPersonalCont == 1) & PerctInc <= 0) {
  if (FreqCont == 1) {
    SuperCont <- ContAmount * 52
    return(SuperCont)
  } else if (FreqCont == 2) {
    SuperCont <- ContAmount * 26
    return(SuperCont)
  } else if (FreqCont == 3) {
    SuperCont <- ContAmount * 6.5
    return(SuperCont)
  } else if (FreqCont == 4) {
    SuperCont <- ContAmount * 1
    return(SuperCont)
  } else if (FreqCont == 0) {
    return(0)
  }} else if ((MakesPrivateCont == 1 | MakesPersonalCont == 1) & PerctInc > 0) {
    SuperCont <- Inc * (PerctInc / 100)
    return(SuperCont)
  } else {
    return(0)
  }}


#======================================================

# 1.2.3. Partner contributions to super fund
# applied amount stated in survey

PartnerCont_fn <- function(DoesPartnerCont, ContAmount, FreqCont) { if (DoesPartnerCont == 1) {
  if (FreqCont == 1) {
    SuperCont <- ContAmount * 52
    return(SuperCont)
  } else if (FreqCont == 2) {
    SuperCont <- ContAmount * 26
    return(SuperCont)
  } else if (FreqCont == 3) {
    SuperCont <- ContAmount * 6.5
    return(SuperCont)
  } else if (FreqCont == 4) {
    SuperCont <- ContAmount * 1
    return(SuperCont)
  } else if (FreqCont == 0) {
    return(0)
  }
} else {
  return(0)
}}


#======================================================

# Non-Concessional contributions to super

#======================================================

## 1.3. Government Contiburions

#======================================================

# 1.3.1. Government contributions to super for low income earners
# 1. Super co-contributions
# 2. Low income super contribution

# 1.3.1.1. Super Co-contributions
# Eligible if:
# made at least 1 personal (after-tax) contribution to super
# Income threshold test: Taxable income + reportable fringe benefits total + employer super contributions - business deductions
# Lower income threshold - $33,516
# Higher income threshold - $48,516
# If income <= lower threshold - Govt will match personal super contributions at rate of 50%
# Max entitlement - $500
# If income below higher threshold, max contribution reduces
# 10% eligible income test - 10% or more of income must come from employment-related activities and/or carrying on a business
# less than 71yrs old at end financial year
# didn't hold temporary visa at any time during financial year
# lodged tax return
# Min amount paid is $20

# Income for purpose of super co-contributions test
# to add more variables to this

Govt_Co_Cont_fn <- function(NonConcCont, Inc, Wage, Age) { if (isTRUE((NonConcCont > 0) & (Wage / Inc >= 0.1) & (Age < 71))) {
  if (Inc <= 33516) {
    if (NonConcCont >= 1000) {
      GovtCont <- 1000 * 0.5
      return(GovtCont)
    } else if (NonConcCont < 1000 & NonConcCont > 40) {
      GovtCont <- NonConcCont * 0.5
      return(GovtCont)
    } else if (NonConcCont <= 40) {
      GovtCont <- 20
      return(GovtCont)
    }
  } else if (Inc > 33516 & Inc < 48516) {
    if (NonConcCont >= 1000) {
      GovtCont <- (1000 * 0.5) - ((Inc - 33516) * 0.03333)
      return(GovtCont)
    } else if (NonConcCont < 1000 & NonConcCont > 40) {
      GovtCont <- NonConcCont * 0.5 - ((Inc - 33516) * 0.03333)
      if (GovtCont > 0) {
        return(GovtCont)
      } else {
        return(0)
      }
    } else if (NonConcCont <= 40) {
      GovtCont <- 20
      return(GovtCont)
    }
  } else {
    return(0)
  }
} else {
  return(0)
}}

# need to check Wage variable

# 1.3.1.2. Low income super contribution
# for taxable incomes up to $37,000
# Govt contributes 15% concessional contributions to super
# up to max of $500; min of $10
# Eligibility:
# Concessional contributions are made
# Income <= $37000
# have not held temporary resident visa
# 10% or more income comes from business and or employment

# LISC Income
# need to add more varibles

LISC_fn <- function(ConcCont, Inc, Wage) { if (isTRUE((Wage / Inc >= 0.1) & (Inc < 37000))) {
  Cont <- ConcCont * 0.15
  if (Cont > 500) {
    LISC <- 500
    return(LISC)
  } else if (Cont < 500 & Cont > 10) {
    LISC <- Cont
    return(LISC)
  } else if (Cont <= 10) {
    LISC <- 10
    return(LISC)
  }
} else {
  return(0)
}}

# # to check wage variable
# SuperTbl$LISC <- mapply(LISC_fn, SuperTbl$ConcessionalCont, SuperTbl$LISC_Inc, SuperTbl$WageIncImp_SalSac)
#
# #======================================================
#
# # Govt Contributions to super
# SuperTbl <- mutate(SuperTbl, GovtCont = Govt_CoCont + LISC)
#
# # Total Contributions to Super
# SuperTbl <- mutate(SuperTbl, TotalSuperCont = EmpCont + PersonalCont + PersonalPrivateCont + PartnerCont + SalSac_Super + GovtCont)

#======================================================

## 1.4. Tax paid on contributions to super

#======================================================

# 1.4.1. Tax paid on pre-tax contributions to super
# employee/business contributions are taxed at 15% in fund
# if contributions above cap, excess is included as taxable income and taxed at marginal rate
# Excess Contributions Charge applies to the excess (5.66% is average of 2013-14yr)
# if under 59, contributions cap of $25,000
# if 59+, contributions cap of $35,000
#
# # taxable income plus contributions to super above 25000
# SuperTbl <- SuperTbl %>% mutate(EmpContAbove25k = ConcessionalCont - 25000) %>%
#   mutate(EmpContECC_25k = EmpContAbove25k * 0.0566) %>%
#   mutate(EmpCont_Inc_25k = TaxableInc + EmpContAbove25k)
# SuperTbl$EmpCont_Inc_25k_IncTax <- mapply(RegIncTAXfn, SuperTbl$EmpCont_Inc_25k)
# SuperTbl$EmpCont_Inc_25k_ML <- mapply(MLfn, SuperTbl$EmpCont_Inc_25k)
# SuperTbl <- SuperTbl %>% mutate(EmpCont_Inc_25k_Tax = EmpCont_Inc_25k_IncTax + EmpCont_Inc_25k_ML) %>%
#   mutate(EmpCont_Inc_25k_TaxExcess = EmpCont_Inc_25k_Tax - TotalIndIncTax + EmpContECC_25k)
#
# #taxable income plus contributions to super above 35000
# SuperTbl <- SuperTbl %>% mutate(EmpContAbove35k = ConcessionalCont - 35000) %>%
#   mutate(EmpContECC_35k = EmpContAbove35k * 0.0566) %>%
#   mutate(EmpCont_Inc_35k = TaxableInc + EmpContAbove35k)
# SuperTbl$EmpCont_Inc_35k_IncTax <- mapply(RegIncTAXfn, SuperTbl$EmpCont_Inc_35k)
# SuperTbl$EmpCont_Inc_35k_ML <- mapply(MLfn, SuperTbl$EmpCont_Inc_35k)
# SuperTbl <- SuperTbl %>% mutate(EmpCont_Inc_35k_Tax = EmpCont_Inc_35k_IncTax + EmpCont_Inc_35k_ML) %>%
#  mutate(EmpCont_Inc_35k_TaxExcess = EmpCont_Inc_35k_Tax - TotalIndIncTax + EmpContECC_35k)

# function to determine tax on contributions
ContTax_fn <- function(Age, ConcessionalCont, EmpCont_Inc_25k_TaxExcess, EmpCont_Inc_35k_TaxExcess) { if (Age < 59) {
  if (ConcessionalCont <= 25000) {
    EmpContTax <- ConcessionalCont * 0.15
    return(EmpContTax)
  } else if (ConcessionalCont > 25000) {
    EmpContTax <- 25000 * 0.15 + EmpCont_Inc_25k_TaxExcess
    return(EmpContTax)
  } else {
    return(0)
  }
} else if (Age >= 59) {
  if (ConcessionalCont <= 35000) {
    EmpContTax <- ConcessionalCont * 0.15
    return(EmpContTax)
  } else if (ConcessionalCont > 35000) {
    EmpContTax <- 35000 * 0.15 + EmpCont_Inc_35k_TaxExcess
    return(EmpContTax)
  } else {
    return(0)
  }
} else {
  return(0)
}}


#======================================================

# 1.4.2. Tax savings through spouse contributions to super (for low income or nor working spouse)
# eligible if spouse's assessable income less than $13,800
# max offset of $540 a year
# tax offset is 18% of the lesser of:
# $3000 reduced by $1 for every $1 that the sum of spouse's income was more than $10,800
# total of contributions for spouse for the year


PartnerTaxOffset_fn <- function(Inc, PartnerCont) { if (PartnerCont > 0 & Inc <= 10800) {
  TaxOffset_1 <- 0.18 * 3000
  TaxOffset_2 <- 0.18 * PartnerCont
  if (TaxOffset_1 <= TaxOffset_2) {
    return(TaxOffset_1)
  } else if (TaxOffset_1 > TaxOffset_2) {
    TaxOffset <- TaxOffset_2
    return(TaxOffset_2)
  } else {
    return(0)
  }} else if (PartnerCont > 0 & Inc < 13800) {
    TaxOffset_3 <- 0.18 * 3000 - (0.18 * (Inc - 10800))
    TaxOffset_4 <- 0.18 * PartnerCont
    if (TaxOffset_3 <= TaxOffset_4) {
      return (TaxOffset_3)
    } else if (TaxOffset_3 > TaxOffset_4) {
      return(TaxOffset_4)
    } else {
      return(0)
    }
  } else {
    return(0)
  }}


#==============================================================================================================================

## 2.1. Earnings in Super

# Super Fund Value
# SuperTbl <- mutate(SuperTbl, FundValue = Super + SuperRet)
# SuperTbl$FundValue <- as.numeric(SuperTbl$FundValue)
#
# # As no earnings figure provided, applied rate of return to get earnings in the fund
# SuperTbl <- mutate(SuperTbl, FundEarnings = FundValue * 0.05)

#======================================================

## 2.2. Tax paid on earnings in super
# if under 60, earnings tax at 15%
# if 60+, no tax on earnings
# if at preservation age, may start transition to retirement pension --> continue to work, continue making super payments, and take payout from super fund
# earnings in these super funds are tax exempt

EarningsTax_fn <- function(Age, FundEarnings, Super_Inc) { if (Age < 55) {
  EarningsTax <- FundEarnings * 0.15
  return (EarningsTax)
} else if (Age >= 55 & Age < 60 & Super_Inc > 0) {
  EarningsTax <- 0
  return(0)
} else if (Age >= 55 & Age < 60 & Super_Inc <= 0) {
  EarningsTax <- FundEarnings * 0.15
  return(EarningsTax)
} else if (Age >= 60) {
  EarningsTax <- 0
  return(0)
}}


#==============================================================================================================================

## 3.1. Payouts from Super --> Super_Inc Variable

#======================================================

## 3.2. Tax paid on payouts from super
# at 30 June 2013, individual at preservation age if at least 55yrs old
# if under preservation age, payouts form part of regular income tax and are taxed at marginal rate
# if at preservation age, payouts taxed at marginal tax rates, with 15% tax offset available
# if 60+, no tax on payouts
#
# SuperTbl <- mutate(SuperTbl, TaxInc_SuperInc = TaxableInc + Super_Inc)
# SuperTbl$TaxInc_SuperInc_Tax <- mapply(RegIncTAXfn, SuperTbl$TaxInc_SuperInc)
# SuperTbl$TaxInc_SuperInc_MDLevy <- mapply(MLfn, SuperTbl$TaxInc_SuperInc)
# # Tax paid on super, assuming
# SuperTbl <- mutate(SuperTbl, SuperPayoutTax = TaxInc_SuperInc_Tax + TaxInc_SuperInc_MDLevy - TotalIndIncTax)

SuperPayoutTax_fn_2 <- function(Age, SuperPayoutTax) {if (Age < 55) {
  PayoutTax <- SuperPayoutTax
  return(PayoutTax)
} else if (Age >= 55 & Age < 60) {
  PayoutTax <- SuperPayoutTax - (0.15 * SuperPayoutTax)
  return(PayoutTax)
} else if (Age >= 60) {
  PayoutTax <- 0
  return(PayoutTax)
}}

# no tax paid on payouts if in transition to retirement pension??
SuperPayoutTax_fn <- function(Age, SuperPayoutTax, Super_Inc) {if (Age < 55) {
  PayoutTax <- SuperPayoutTax
  return(PayoutTax)
} else if ((Age >= 55 & Age < 60) & Super_Inc <= 0) {
  PayoutTax <- SuperPayoutTax - (0.15 * SuperPayoutTax)
  return(PayoutTax)
} else if (Age <= 55 & Age < 60 & Super_Inc > 0) {
  PayoutTax <- 0
  return(PayoutTax)
} else if (Age >= 60) {
  PayoutTax <- 0
  return(PayoutTax)
} else {
  return(0)
}}


#==============================================================================================================================

## 4. Division 293 Tax
# 15% tax paid on contributions to super when combing income and super contributions above $300,000

# Income as defined under Div 293 - need to add more variables to this
# SuperTbl <- mutate(SuperTbl, Div293Inc = TaxableInc + ConcessionalCont)
# # Income and Contributions above $300,000
# SuperTbl <- mutate(SuperTbl, AmtAbv300k =  Div293Inc - 300000)

Div293Tax_fn <- function(Inc, EmpCont, AmtAbv300k) { if (Inc > 300000) {
  if (AmtAbv300k < EmpCont) {
    Tax <- AmtAbv300k * 0.15
    return(Tax)
  } else if (AmtAbv300k >= EmpCont) {
    Tax <- EmpCont * 0.15
    return(Tax)
  } else {
    return(0)
  }} else {
    return(0)
  }}


