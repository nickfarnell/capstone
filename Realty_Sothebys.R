#scrape sothebys realty website
#tutorial from http://thatdatatho.com/2018/12/14/an-introduction-to-scraping-real-estate-data-with-rvest-and-rselenium/

url <- "https://sothebysrealty.ca/en/search-results/city-niagara-on-the-lake-reg-niagara-on-real-estate/price-0-10000000/status-1/view-table/show-mls/sort-1/pp-32/page-2/"

sapply(2:8, function(x) {
  url <- "https://sothebysrealty.ca/en/search-results/city-niagara-on-the-lake-reg-niagara-on-real-estate/price-0-10000000/status-1/view-table/show-mls/sort-1/pp-32/page-"
  paste0(url, x) }) -> urls

install.packages("RSelenium")
library(RSelenium)

driver <- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]



rD <- rsDriver(port = 4444L)
remDr <- rD[["client"]]
remDr$close()
rm(rD)
gc()



df_all <- data.frame()
for(i in 1:(length(urls))) {
  remDr$navigate(paste0(urls[[i]]))
  Sys.Sleep(1)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'plink']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(1)
  df_all <- rbind(df_all, df)
}





link <- 'https://www.google.com/'

rD <- rsDriver(verbose = TRUE,
               port=4837L, 
               browserName = 'chrome', 
               chromever = '83.0.4103.39',
               check = TRUE)
remDr <- rD$client
remDr$navigate(link)













library(RSelenium)
rD1 <- rsDriver(browser = "chrome", port = 4587L, geckover = NULL, 
                chromever =  "latest", iedrver = NULL, 
                phantomver = NULL, extraCapabilities = cprof)
remDr1 <- rD1[["client"]] 

remDr1$close()
rD1$server$stop() 
rm(rD1, rD2)
gc()

binman::list_versions("chromedriver")

# RSelenium::startServer() if required
require(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "chrome"
)

driver <- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]

df_all <- data.frame()
for(i in 1:(length(urls))) {
  remDr$navigate(paste0(urls[[i]]))
  Sys.Sleep(1)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'plink']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(1)
  df_all <- rbind(df_all, df)
}


df_all <- data.frame()
for(i in 1:(length(urls))) {
  remDr$navigate(paste0(urls[[i]]))
  Sys.Sleep(1)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'plink']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(1)
  df_all <- rbind(df_all, df)
}







library(tidyverse)
library(rvest)
h <- read_html(url)
url <- "https://sothebysrealty.ca/en/property/ontario/niagara-real-estate/niagara-on-the-lake/571932/"

html_text(h)

  
  
  
  
  
  
  
#####  
  
binman::list_versions("chromedriver")
rd <- rsDriver(browser = "chrome", port = 4235L, chromever = "84.0.4147.30")
remDr <- rd[["client"]]

remDr$close()
rd$server$stop()
Lego <- read_html(remDr$getPageSource()[[1]])

LinkTitle <- Lego %>% html_nodes("tr") %>% html_text()
LinkInfo <- Lego %>% html_nodes("tr")
Link <- Lego %>% html_nodes("tr") %>% 
  html_attr("onclick") %>%
  substr(start = 23, stop = 500)
  
Link <- gsub('.{1}$', '', Link)
Link <- paste0("https://sothebysrealty.ca/", Link)

Output <- data.frame(WebLink = Link)
#helpful link https://levelup.gitconnected.com/web-scraping-with-r-part-2-dynamic-webpages-de620a161671

house_price <- Link %>% 
  rvest::html_nodes("ul")


house_price <- Link %>% 
  rvest::html_nodes("ul")

#get house price
house_price <- html %>% 
  rvest::html_nodes("li") %>% 
  rvest::html_nodes(xpath = '//*[@class="price_social"]') %>% 
  rvest::html_text() %>%
  .[[1]]


#get house price
html <- read_html(Link[2])

sentences <- html%>%
  html_nodes("li") %>%
  html_text()




b <- Link[2]






# converting links stored in a data.frame() as factors to character type stored in a vector df
links <- sapply(Output$WebLink, as.character)

# initalize empty data frame where we will be storing our scraped data 
df_all_data <- data.frame()

# write our scraper function
scraper <- function(links) {
  
  # save link in url object
  url <- links
  # parse page url
  page <- xml2::read_html(url)
  Sys.sleep(0.25)
  
  #get house price
  house_price <- page %>% 
    rvest::html_nodes("ul") %>% 
    rvest::html_nodes(xpath = '//*[@class="price_social"]') %>% 
    rvest::html_text() %>%
    .[[1]]
  
  #get street address
  street_address <- page %>% 
    rvest::html_nodes("span") %>% 
    rvest::html_nodes(xpath = '//*[@class="span8"]') %>% 
    rvest::html_nodes("h1") %>%
    rvest::html_text()
  

  
  
  df_individual_page <- data.frame(price = house_price,
                                   address = street_address,
                                   )
  
  # rbinding df_all_data and df_individual_page
  # <<- makes df_all_data a global variable. Making it available in the global environment
  df_all_data <<- rbind(df_all_data, df_individual_page)
}

# looping over all links in the vector and applying scraper function to each link
sapply(links, scraper)






