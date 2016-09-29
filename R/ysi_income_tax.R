#' Medicare Levy Function
#'
#' Medicare Levy  for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc variable to have been calulated previously
#'
#' @param Super_Inc Whole dollar value of super income
#' @return A list
#' @export



MLfn<-function(Taxinc = NULL)  {if(Taxinc < 20542)
{ML <- 0
return(ML)}
  else if(Taxinc < 24167)
  {ML <-  0.1*(Taxinc - 20542)
  return(ML)}
  else
  {ML <- 0.015*Taxinc
  return(ML)}
}

#' Low Income Tax Offset (LITO) Function
#'
#' Low Income Tax Offset (LITO) for use on HIDLA, as part of the Income Tax Assesment
#' For use with sapply()
#'
#' Requires TaxInc variable to have been calulated previously
#'
#' @param Super_Inc Whole dollar value of super income
#' @return A list
#' @export

LITOfn<-function(Taxinc= NULL)  {if(Taxinc < 37000)
{LITO <- 445
return(LITO)}
  else if(Taxinc < 66667)
  {LITO <- 445 - 0.015*(Taxinc - 37000)
  return(LITO)}
  else
  {LITO <- 0
  return(LITO)}
}

#' Mature Age Tax Offset (MATO) Function
#'
#' Mature Age Tax Offset (MATO) for use on HIDLA, as part of the Income Tax Assesment
#' For use with mapply()
#'
#'
#' @param WageIncImp_SalSac Whole dollar value of Wage income with salary sacrifice
#' @param YoB The year of birth

#' @return A list
#' @export


MATOfn<-function(WageIncImp_SalSac= NULL, YoB= NULL)  {if(WageIncImp_SalSac < 10000)
{MATO <- 0.05*WageIncImp_SalSac
return(MATO)}
  else if(WageIncImp_SalSac < 53000)
  {MATO <- 500
  return(MATO)}
  else if(WageIncImp_SalSac < 63000)
  {MATO <- 500-0.05*(WageIncImp_SalSac-53000)
  return(MATO)}
  else
  {MATO <- 0
  return(MATO)}
}
