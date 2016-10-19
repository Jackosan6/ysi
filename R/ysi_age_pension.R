#' Age Pension Master Function
#'
#' (Jackson to complete...)
#'
#' @name ysi_age_pension
#' @param df Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @return A data frame
#' @export

ysi_age_pension <- function(df = NULL){
  #HHtbl <- HHtbl %>% filter(Age >= 18)

  HHtbl <- AssessableAssetsfn(HHtbl)

  HHtbl <- OrdinaryIncomefn(HHtbl)

  HHtbl <- DeemedAssetsfn(HHtbl)

  HHtbl <- OwnHomeAPfn(HHtbl)

  HHtbl <- AgePensionRatefn(HHtbl)

  ## deemed assets with home
  HHtbl <- HHtbl %>% mutate(AssetsAdjHome=AssetShare+SuperAdj+BankBal+UnitOPEquity+UnitHomeEquityAP,
                            DeemedAssetsHome=ifelse(ComplicatedAssetSplit==1,SharedDeemedAssets*AssetProp+SuperAdj+BankBal+UnitHomeEquityAP,
                                                    SharedDeemedAssets+SuperAdj+BankBal+UnitHomeEquityAP))
  HHtbl <- RentAssistfn(HHtbl)

  ### NOW FOR THE OUTPUTS


  ## Dummy for if currently receiving a pension payment
  HHtbl$AP_per_Fortnight[which(is.na(HHtbl$AP_per_Fortnight))] <- 0
  HHtbl <- HHtbl %>% mutate(AnnualisedAP=AP_per_Fortnight*26)


  HHtbl <- HHtbl %>% group_by(PersonID) %>%
    mutate(ra=ifelse(Rent_Assist==1,ifelse(Single==1,min(-min((Fortnightly_Rent-RAMaxRentSingle)*0.75,0),RAMaxRatesSingle)*26,
                                           min(-min((Fortnightly_Rent-RAMaxRentCoupled)*0.75,0),RAMaxRatesCoupled)*26/2),0),
           mpr=ifelse(Single==1,APMBRSingle+APPSSingle+APESSingle+ra,APMBRCoupled+APPSCoupled+APESCoupled+ra),
           di=ifelse(Single==1,sum(diff(c(0,pmin(DeemedAssets,c(DeemTASingle,Inf))))*c(DeemBTR,DeemATR)),
                     sum(diff(c(0,pmin(DeemedAssets,c(DeemTACoupled,Inf))))*c(DeemBTR,DeemATR))),
           oi=ifelse(Single==1,Ordinary_Income+di,(Ordinary_Income+di)/2),
           ir=ifelse(Single==1,max(oi-APIncTestSingle,0)*APIncomeRedRate,max(oi-APIncTestCoupled,0)*APIncomeRedRate),
           voa=ifelse(Single==1,AssetsAdj,AssetsAdj/2),
           avl=ifelse(Single==1,ifelse(UnitOwnHomeAP==TRUE,as.numeric(APAssetsSingleOwnHome),as.numeric(APAssetsSingle)),
                      ifelse(UnitOwnHomeAP==TRUE,as.numeric(APAssetsCoupledOwnHome)/2,as.numeric(APAssetsCoupled)/2)),
           ar=max(floor((voa-avl)/250)*9.75,0),
           ReducedRate=(ir+ar>0),
           HowReduced=ifelse(ReducedRate==TRUE,ifelse(ir>ar,"Income","Assets"),"NotReduced"),
           age_pensionEST=ifelse(Age>=65,max(min(mpr-ir,mpr-ar),0),0),
           age_pension=ifelse(AnnualisedAP>0,age_pensionEST,0))
  HHtbl$age_pension[which(is.na(HHtbl$age_pension))] <- 0

  return(HHtbl)
}
