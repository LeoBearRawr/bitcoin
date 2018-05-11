#make sure that the file is in the given folder path 
if (dir.exists(folderpath <- "~/Desktop/bitcoin/")) {
  setwd(folderpath)
} else {
  stop(paste("directory does not exist:", folderpath))
}
source("function.R")
packages <- c("rvest","RCurl", "stringr","R.utils", "data.table","foreach")
librarycheck(packages) 

URL <- "http://api.bitcoincharts.com/v1/csv/"

## scrape the names of the exchanges 
scraping <- read_html(URL)
stocks <- scraping %>% 
  html_nodes("pre") %>% 
  html_nodes("a") %>% 
  html_text()

# make sure the first two element is ../ and inactive_exchanges/
if ("../" %in% stocks & "inactive_exchanges/" %in% stocks) {
  backindex <- which(stocks=="../")
  otherexchanges <- which(stocks=="inactive_exchanges/")
  #which search through vector and find the index of the element of interest
  stocks <- stocks[-c(backindex,otherexchanges)]
} else {
  stop("Format of the html has changed")
}
if (!all(grepl(pattern = ".csv.gz$", stocks))) {
  stop("The website contain non .csv.gz file")
}
#grepl can be used to spot pattern and spit out boolean, also "$" is used to indecate end of the text

paste(stocks)

##download and process all the files

exchangecurrency <- str_replace_all(stocks,"[^A-Z]","")

allcurrency <- data.frame(
  date = as.POSIXct(character()),
  currency = character(),
  sumvolume = double(),
  sumprice = double(),
  count = integer(),
  stringsAsFactors =TRUE)

Metadata <- vector()

options(warn = -1)
 for(i in  seq_along(stocks)) {
     
  stockname <- stocks[i] 
    
  if(download.file(paste0(URL,stockname),"temp.gz") == !0) {
    stop(paste("failed to download file from given:",URL,stockname))
  }
  gunzip("temp.gz")
  
  print(file.info("temp")$size)
  if(file.info("temp")$size <= 0){
    if(file.remove("temp","temp.tmp")[1]== FALSE){
      stop(paste("there was a problem removing temp,.tmp file"))
    }
    Metadata <- c(Metadata,stockname)
    
    next
  }
  
  exchangeCSV <- fread("temp",header= FALSE,showProgress = FALSE, col.names = c("unixtime","price", "volume"))
 if(nrow(exchangeCSV)<1){
   stop(paste("it seems like there was an error unziping .gz file"))
 }

  ##exchangeCSV1 <- read.csv(temp,col.names = c("unixtime","price", "volume"))
  if(file.remove("temp","temp.tmp")[1]== FALSE){
    stop(paste("there was a problem removing temp,.tmp file"))
  }

  currencydata <- cleancurrency(exchangeCSV)
  rm(exchangeCSV)
  currencydata$currency <- exchangecurrency[i]

  allcurrency <- bind_rows(currencydata,allcurrency) %>%
    group_by(currency,date) %>%
    summarise(sumvolume = sum(sumvolume),
              sumprice = sum(sumprice),
              count = sum(count))
    print(paste("exchange number:",i))
}

options(warn = 0)
allcurrency<- allcurrency %>%
  mutate(meanprice = sumprice/count,
         sumprice = NULL,
         date= as.POSIXct(date)) %>% 
  filter(format(date, format="%Y")>2000)




IQR (allcurrency$meanprice)

file.remove("allcurrency.csv")
write.table(allcurrency,"allcurrency.csv",sep = ",",row.names = F)


///////////////////////////////////////////////////////////////

install.packages("alfred")
install.packages("devtools")
devtools::install_github("onnokleen/alfred")
library(alfred)
economic_data <- get_alfred_series ("EMRATIO","employmentpopratio")
library(ggplot2)

p <- ggplot(economic_data)+
  geom_line(aes(x = date, y = employmentpopratio))+
  geom_line(allcurrency,aes(x = date, y = meanprice ))+
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = paste(employmentpopratio),
       x = "Date and time",
       colour = "Parameter")+
  theme(legend.position = c(0.8, 0.9))
p

