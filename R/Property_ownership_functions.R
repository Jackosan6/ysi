
#' Age Pension Home ownership function
#'
#' (Jackson to complete...)
#'
#' @name OwnHomeAPfn
#' @param df Argument is HHtbl from the master age pension function
#' @return A data frame
#' @export
#'


## Home ownership function
OwnHomeAPfn <- function(HHtbl=NULL){
  ## Dummy the individual as partnered if in a couple and there is another person in the family in the same situation
  HHtbl <- HHtbl %>% group_by() %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0))
  HHtbl<-HHtbl %>% mutate(OwnHome=ifelse(PersonNo==1 & OwnHomeP1==1,1,
                                         ifelse(PersonNo==2 & OwnHomeP2==1,1,
                                                ifelse(PersonNo==3 & OwnHomeP3==1,1,
                                                       ifelse(PersonNo==4 & OwnHomeP4==1,1,
                                                              ifelse(PersonNo==5 & OwnHomeP5==1,1,
                                                                     ifelse(PersonNo==6 & OwnHomeP6==1,1,
                                                                            ifelse(PersonNo==7 & OwnHomeP7==1,1,
                                                                                   ifelse(PersonNo==8 & OwnHomeP8==1,1,
                                                                                          ifelse(PersonNo==9 & OwnHomeP9==1,1,
                                                                                                 ifelse(PersonNo==10 & OwnHomeP10==1,1,
                                                                                                        ifelse(PersonNo==11 & OwnHomeP11==1,1,
                                                                                                               ifelse(PersonNo==12 & OwnHomeP12==1,1,
                                                                                                                      ifelse(PersonNo==13 & OwnHomeP13==1,1,
                                                                                                                             ifelse(PersonNo==14 & OwnHomeP14==1,1,
                                                                                                                                    ifelse(PersonNo==15 & OwnHomeP15==1,1,
                                                                                                                                           ifelse(PersonNo==16 & OwnHomeP16==1,1,
                                                                                                                                                  ifelse(PersonNo==17 & OwnHomeP17==1,1,
                                                                                                                                                         ifelse(PersonNo==18 & OwnHomeP18==1,1,0)))))))))))))))))))
  HHtbl$OwnHome[is.na(HHtbl$OwnHome)] <- 0
  ## For breaking up home equity
  HHtbl <- HHtbl %>% group_by(HouseID) %>% mutate(HHSumOwnHome=sum(OwnHome))
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(UnitOwnHomeAP=(sum(OwnHome)>0),
                                                                     UnitOwnHomeShareAP=ifelse(HHSumOwnHome>0,sum(OwnHome)/HHSumOwnHome,0))
  HHtbl <- HHtbl %>% mutate(UnitHomeEquityAP=(HomeEquity-HomeEquity_n)*UnitOwnHomeShareAP)
  print(length(HHtbl$Wave_n))
  return(HHtbl)
}

#' Age Pension Other property ownership function
#'
#' (Jackson to complete...)
#'
#' @name OwnOPfn
#' @param df Argument is HHtbl from the master age pension function
#' @return A data frame
#' @export
#'
## Other property ownership function
OwnOPfn <- function(HHtbl=NULL){
  HHtbl<-HHtbl %>% mutate(OwnOP=ifelse(PersonNo==1 & OwnOPP1==1,1,
                                       ifelse(PersonNo==2 & OwnOPP2==1,1,
                                              ifelse(PersonNo==3 & OwnOPP3==1,1,
                                                     ifelse(PersonNo==4 & OwnOPP4==1,1,
                                                            ifelse(PersonNo==5 & OwnOPP5==1,1,
                                                                   ifelse(PersonNo==6 & OwnOPP6==1,1,
                                                                          ifelse(PersonNo==7 & OwnOPP7==1,1,
                                                                                 ifelse(PersonNo==8 & OwnOPP8==1,1,
                                                                                        ifelse(PersonNo==9 & OwnOPP9==1,1,
                                                                                               ifelse(PersonNo==10 & OwnOPP10==1,1,
                                                                                                      ifelse(PersonNo==11 & OwnOPP11==1,1,
                                                                                                             ifelse(PersonNo==12 & OwnOPP12==1,1,
                                                                                                                    ifelse(PersonNo==13 & OwnOPP13==1,1,
                                                                                                                           ifelse(PersonNo==14 & OwnOPP14==1,1,
                                                                                                                                  ifelse(PersonNo==15 & OwnOPP15==1,1,
                                                                                                                                         ifelse(PersonNo==16 & OwnOPP16==1,1,
                                                                                                                                                ifelse(PersonNo==17 & OwnOPP17==1,1,
                                                                                                                                                       ifelse(PersonNo==18 & OwnOPP18==1,1,0)))))))))))))))))))
  HHtbl$OwnOP[is.na(HHtbl$OwnOP)] <- 0
  ## For breaking up other properties
  HHtbl <- HHtbl %>% group_by(HouseID) %>% mutate(HHSumOwnOP=sum(OwnOP))
  HHtbl <- HHtbl %>% group_by() %>% mutate(Coupled=ifelse(Family %in% 1:4,1,0))
  HHtbl <- HHtbl %>% group_by(HouseID,IncomeUnit,Coupled) %>% mutate(UnitOwnOPAP=(sum(OwnOP)>0),
                                                                     UnitOwnOPShareAP=ifelse(HHSumOwnOP>0,sum(OwnOP)/HHSumOwnOP,0))
  ## only a part of assets if not living in nursing home
  HHtbl <- HHtbl %>% mutate(OtherPropEquity=(OtherPropEquity_p-OtherPropEquity_n),
                            UnitOPEquity=OtherPropEquity*UnitOwnOPShareAP)
  return(HHtbl)
}

