#' Age Pension Rates Function
#'
#' (Jackson to complete...)
#'
#' @name AgePensionRatefn
#' @param df Argument is HHtbl from the master age pension function
#' @return A data frame
#' @export



AgePensionRatefn <- function(HHtbl=NULL){

  ## Change the format of dates with as.Date in a all data frames
  HHtbl$IntDate <- as.Date(HHtbl$IntDate,format="%d/%m/%Y")
  RARates$RADate <- as.Date(RARates$RADate,format="%d/%m/%Y")
  APMaxBasicRate$APMaxBasicDate <- as.Date(APMaxBasicRate$APMaxBasicDate,format="%d/%m/%Y")
  APPensionSuppRates$APPensionSuppDate <- as.Date(APPensionSuppRates$APPensionSuppDate,format="%d/%m/%Y")
  APEnergySuppRates$APEnergySuppDate <- as.Date(APEnergySuppRates$APEnergySuppDate,format="%d/%m/%Y")
  DeemingRates$DeemingDate <- as.Date(DeemingRates$DeemingDate,format="%d/%m/%Y")
  APMeansTest$APMeansTestDate <- as.Date(APMeansTest$APMeansTestDate,format="%d/%m/%Y")

  ## Giving individuals without an interview date the median interview date
  na_int_date <- HHtbl %>% filter(is.na(IntDate))
  HHtbl <- HHtbl %>% filter(!is.na(IntDate))




  na_int_date <- na_int_date %>% mutate(IntDate = ifelse(Wave_n == "WAVE08", 2008-09-22,
                                                         ifelse(Wave_n == "WAVE09", 2009-09-24 ,
                                                                ifelse(Wave_n == "WAVE10",2010-09-21 ,
                                                                       ifelse(Wave_n == "WAVE11", 2011-09-07 ,
                                                                              ifelse(Wave_n == "WAVE12",2012-09-06  ,
                                                                                     ifelse(Wave_n == "WAVE13",2013-09-06,
                                                                                            ifelse(Wave_n == "WAVE14", 2014-09-03,NA))))))))

  HHtbl <- bind_rows(HHtbl, na_int_date)

  ## Find applicable dates the thresholds and rates for age pension were set
  HHtbl <- HHtbl %>% group_by(PersonID) %>% mutate(RADate=RARates$RADate[findInterval(IntDate,RARates$RADate)],
                                                   APMaxBasicDate=APMaxBasicRate$APMaxBasicDate[findInterval(IntDate,APMaxBasicRate$APMaxBasicDate)],
                                                   APPensionSuppDate=APPensionSuppRates$APPensionSuppDate[findInterval(IntDate,APPensionSuppRates$APPensionSuppDate)],
                                                   APEnergySuppDate=APEnergySuppRates$APEnergySuppDate[findInterval(IntDate,APEnergySuppRates$APEnergySuppDate)],
                                                   DeemingDate=DeemingRates$DeemingDate[findInterval(IntDate,DeemingRates$DeemingDate)],
                                                   APMeansTestDate=APMeansTest$APMeansTestDate[findInterval(IntDate,APMeansTest$APMeansTestDate)])
  ## Add the rates and thresholds to the HHtbl dataframe
  HHtbl <- merge(RARates,HHtbl,by="RADate",sort=FALSE,all.y=TRUE)
  HHtbl <- merge(APMaxBasicRate,HHtbl,by="APMaxBasicDate",sort=FALSE,all.y=TRUE)
  HHtbl <- merge(APPensionSuppRates,HHtbl,by="APPensionSuppDate",sort=FALSE,all.y=TRUE)
  HHtbl <- merge(APEnergySuppRates,HHtbl,by="APEnergySuppDate",sort=FALSE,all.y=TRUE)
  HHtbl <- merge(DeemingRates,HHtbl,by="DeemingDate",sort=FALSE,all.y=TRUE)
  HHtbl <- merge(APMeansTest,HHtbl,by="APMeansTestDate",sort=FALSE,all.y=TRUE)
  return(HHtbl)
}
