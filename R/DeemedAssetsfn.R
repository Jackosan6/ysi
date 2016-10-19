#' Age Pension Deemed assets function
#'
#' (Jackson to complete...)
#'
#' @name AssessableAssetsfn
#' @param df Argument is HHtbl from the master age pension function
#' @return A data frame
#' @export
#'

##
DeemedAssetsfn <- function(HHtbl=NULL){
  ## Dummy the individual as partnered if in a couple and there is another person in the family in the same situation
  HHtbl <- HHtbl %>% group_by() %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0),
                            NonDepOver25=(Age>=25 & Family!=9))
  HHtbl <- HHtbl %>% group_by(HouseID) %>% mutate(NonDepOver25s=sum(as.numeric(Age>=25 & Family!=9)))
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(NoCoupled=sum(Coupled))
  HHtbl <- HHtbl %>% mutate(Partnered=(Coupled==1 & NoCoupled>1),
                            Single=(Partnered==FALSE))

  ## Deriving household type to identify issues in splitting household assets
  # HHType: 1= only couple, 4= couple with children <15, 7= couple with dependents
  # HHType: 13= lone parent with children <15, 16= lone parent with dependents, 24= lone person
  # HHType: ComplicatedAssetSplit is for households with 2 or more asset holding income units
  HHtbl <- HHtbl %>% mutate(OnlyCouple=(HHType==1|HHType==4|HHType==7),
                            LonePerson=(HHType==13|HHType==16|HHType==24),
                            OtherRelationsInHouse=(HHType==2|HHType==5|HHType==8|HHType==10|HHType==11|HHType==14|HHType==17|HHType==19|HHType==20|HHType==22),
                            OtherNonRelationsInHouse=(HHType==3|HHType==6|HHType==9|HHType==12|HHType==15|HHType==18|HHType==21|HHType==23|HHType==25|HHType==26),
                            ComplicatedAssetSplit=(OtherRelationsInHouse==TRUE|OtherNonRelationsInHouse==TRUE))
  # Personal asset adjustments
  HHtbl <- HHtbl %>% mutate(SuperOver65=(Super+SuperRet)*as.numeric(Age>=65))
  # Couple Asset pooling
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(SuperAdj=ifelse(Coupled==1,sum(SuperOver65),SuperOver65),
                                                                     SuperRetired=ifelse(Coupled==1,sum(SuperRet),SuperRet),
                                                                     BankBal=ifelse(Coupled==1,sum(BankAcct+JointBankAcct),BankAcct+JointBankAcct))

  # Derive a asset weighting variable in order to allocate assets if there several income units in a household
  HHtbl <- HHtbl %>% mutate(AssetsWeighting=ifelse(NonDepOver25==TRUE,
                                                   IrregOtherRedImp*4.8+abs(InvestInc_p-InvestInc_n)+(BankAcct+JointBankAcct)/8.3+(SuperRet+Super)/29,0))
  HHtbl <- HHtbl %>% group_by(HouseID) %>% mutate(AssetWeightTot=sum(AssetsWeighting))
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(AssetsWeight=ifelse(Coupled==1,sum(AssetsWeighting),AssetsWeighting))
  HHtbl <- HHtbl %>% mutate(AssetProp=ifelse(NonDepOver25s>0,ifelse(AssetWeightTot>0,max(min(1,AssetsWeight/AssetWeightTot),0),1/NonDepOver25s),0))
  ## Deemed income
  HHtbl <- HHtbl %>% mutate(SharedDeemedAssets=HEquity_Investments+HCash_Investments+HTrust_Funds+HLife_Insurance,
                            DeemedAssets=ifelse(ComplicatedAssetSplit==1,SharedDeemedAssets*AssetProp+SuperAdj+BankBal,SharedDeemedAssets+SuperAdj+BankBal))
  return(HHtbl)
}
