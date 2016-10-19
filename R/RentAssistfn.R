#' Age Pension Rent assistance eligibility function
#'
#' (Jackson to complete...)
#'
#' @name RentAssistfn
#' @param df Argument is HHtbl from the master age pension function
#' @return A data frame
#' @export
#'

##
RentAssistfn <- function(HHtbl=NULL){
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

  ## =================================== ARE THEY ELIGIBLE FOR RENT ASSISTANCE?
  HHtbl$Monthly_Rent[is.na(HHtbl$Monthly_Rent)] <- 0
  HHtbl$LivingSitch<-as.numeric(HHtbl$LivingSitch)
  HHtbl$LivingSitch[is.na(HHtbl$LivingSitch)] <- 5

  HHtbl <- HHtbl %>% mutate(Rent_Assist=(as.numeric(Landlord)!=3 & LivingSitch == 2 | LivingSitch == 3))
  HHtbl$Rent_Assist[is.na(HHtbl$Rent_Assist)] <- FALSE

  # Calculating fortnightly rent
  HHtbl <- HHtbl %>% mutate(Fortnightly_Rent=ifelse(NonDepOver25s>0,
                                                    ifelse(LivingSitch==2 & ComplicatedAssetSplit==0,Monthly_Rent/2.174,
                                                           ifelse(LivingSitch==2 & ComplicatedAssetSplit==1 & Coupled==1,Monthly_Rent/2.174*2/NonDepOver25s,
                                                                  ifelse(LivingSitch==2 & ComplicatedAssetSplit==1 & Coupled==0,Monthly_Rent/2.174/NonDepOver25s,0))),0))
  return(HHtbl)
}
