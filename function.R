librarycheck <- function(packages) {
  failed <- vector()
  for(package in packages) {
    if (!do.call(require,list(package))) {
      failed <- c(failed,package)
    }
  } 
  if (length(failed)>0) {
    stop(paste("failed to load packages:",paste(failed,collapse = ",")))
  }
}
##loading packages
packages <- c("anytime","dplyr")
librarycheck(packages)
## this function converts csv file downloaded from the
##"http://api.bitcoincharts.com/v1/csv/"
## and summerizes the data by day with sum of the price and volume and the count
##precondition: DF contains unixtime,price,volume
cleancurrency <- function(DF) {
  DF$date <- strftime(anytime(DF$unixtime),"%m/%d/%Y")
  DF$date <- factor (DF$date)
  USD <- DF %>%
    group_by (date) %>% 
    summarise(sumprice = sum (price),
              sumvolume = sum(volume),
              count = n())
  USD$date <- as.POSIXct(strptime(USD$date, "%m/%d/%Y"))
  return(USD)
}
