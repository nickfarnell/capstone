#scrape sothebys realty website
#tutorial from http://thatdatatho.com/2018/12/14/an-introduction-to-scraping-real-estate-data-with-rvest-and-rselenium/

url <- "https://sothebysrealty.ca/en/search-results/region-greater-vancouver-british-columbia-real-estate/tloc-1/rloc-3/ptype-condo/price-0-1000000/view-grid/show-mls/sort-featured/pp-60/status-sales"
library(rvest)
library(tidyverse)

#sapply(1:965, function(x) {
sapply(1:965, function(x) {
  url <- "https://sothebysrealty.ca/en/search-results/price-0-10000000/status-1/view-grid/show-mls/sort-5/pp-16/page-"
  paste0(url, x) }) -> urls

install.packages("RSelenium")
library(RSelenium)

binman::list_versions("chromedriver")
driver <- rsDriver(browser = "chrome", port = 4236L, chromever = "84.0.4147.30")
remDr <- driver[["client"]]

remDr$close()
rd$server$stop()

df_all <- data.frame()
for(i in 1:(length(urls))) {
  remDr$navigate(paste0(urls[[i]]))
  Sys.sleep(1)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'plink']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(1)
  df_all <- rbind(df_all, df)
}

head(df_all)
nrow(df_all) #number of rows 11272 - while the website shows 15432 ???

# converting links stored in a data.frame() as factors to character type stored in a vector df
links <- sapply(df_all$link, as.character)

# initalize empty data frame where we will be storing our scraped data 
df_all_data <- data.frame()
head(df_all_data)
nrow(df_all_data)

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
    rvest::html_nodes(xpath = '//*[@class="address"]') %>% 
    #rvest::html_nodes("p") %>%  #changed this because it was getting the h1 title before
    rvest::html_text()
  
  # getting the key facts from the condo
  # key facts are: building type, square feet, year built, bedrooms and bathrooms,
  # taxes, and age
  key_facts <- page %>% 
    rvest::html_nodes("ul") %>% 
    rvest::html_nodes(xpath = '//*[@class="key_facts"]') %>% 
    rvest::html_nodes("li") %>%
    rvest::html_text()
  
  # removing unnecessary content from the vector of strings and naming the vector elements
  key_facts %>%
    stringr::str_replace_all(., ".*: ", "") %>%
    purrr::set_names(., nm = stringr::str_replace_all(key_facts, ":.*", "") %>%
                       stringr::str_replace_all(., "[0-9]+", "") %>%
                       stringi::stri_trim_both(.)) -> key_facts
  
  # the following code assigns the scraped data for each condo where applicable
  # if information is not available, we are filling the observation with a NA value
  # for example, there are condos where taxes are not available
  # moreover, some condos are going to get build in the future, so age was not available
  
  # get the building type 
  building_type <- ifelse("Property Type" %in% names(key_facts),
                          key_facts[ grep("Property Type", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                          NA) 
  
  # get square feet
  square_feet <- ifelse("Living Space" %in% names(key_facts),
                        key_facts[ grep("Living Space", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                        NA)
  
  # get the number of bedrooms
  bedrooms <- ifelse("Bedrooms" %in% names(key_facts),
                     key_facts[ grep("Bedrooms", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                     NA)
  
  # get the number of bathrooms
  bathrooms <- ifelse("Bathrooms" %in% names(key_facts),
                      key_facts[ grep("Bathrooms", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                      NA)
  
  # get when the condo was built
  year_built <- ifelse("Year Built" %in% names(key_facts),
                       key_facts[ grep("Year Built", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                       NA)
  
  # get the age of the condo
  age <- ifelse("Approximate Age" %in% names(key_facts),
                key_facts[ grep("Age", names(key_facts), ignore.case = TRUE, value = TRUE) ],
                NA)
  
  # get the taxes (property taxes)
  taxes <- ifelse("Other Taxes" %in% names(key_facts) | "Municipal Taxes" %in% names(key_facts),  
                  key_facts[ grep("taxes", names(key_facts), ignore.case = TRUE, value = TRUE) ], 
                  NA)
  
  # storing individual links in df_individual_page object
  df_individual_page <- data.frame(price = house_price,
                                   address = street_address,
                                   squares = square_feet,
                                   type = building_type,
                                   year = year_built,
                                   age = age,
                                   bed = bedrooms,
                                   bath = bathrooms,
                                   tax = taxes)
  
  # rbinding df_all_data and df_individual_page
  # <<- makes df_all_data a global variable. Making it available in the global environment
  df_all_data <<- rbind(df_all_data, df_individual_page)
}

# looping over all links in the vector and applying scraper function to each link
sapply(links, scraper)

write.csv(df_all_data,"Sothebys5_highestprice.csv", row.names = TRUE)


SampleData <- sample_n(df_all_data, 50)
write.csv(SampleData, "Sample_Sothebys.csv", row.names = TRUE)





BungolLink <- sapply("https://www.bungol.ca/listing/1073-concession-2-road-niagara-on-the-lake-x4732756-4085282/", as.character)
BungolPage <- xml2::read_html(BungolLink)

BungolHousePrice <- BungolPage %>% 
  rvest::html_nodes(xpath = '//*[@class="mb-2"]') %>% 
  rvest::html_nodes(xpath = '//*[@class="fw400 fs15 fs-sm-125"]') %>% 
  rvest::html_text()

binman::list_versions("chromedriver")
driver <- rsDriver(browser = "chrome", port = 4236L, chromever = "84.0.4147.30")
remDr <- driver[["client"]]
remDr$navigate("https://www.bungol.ca/listing/1073-concession-2-road-niagara-on-the-lake-x4732756-4085282/")
webElem <- remDr$findElement(using = 'class name', value = "mb-2")
webElem$getElementAttribute("class")
webElem$getElementText()


listingDateDifference <- remDr$findElement(using = 'id', value = "listingDateDifference")
listingDateDifference$getElementText()


webElem2 <- remDr$findElement(using = 'id', value = "listingStatus")
webElem2$getElementText()

#####
# converting links stored in a data.frame() as factors to character type stored in a vector df
bun_links <- as.character( c(
"https://www.bungol.ca/listing/4-goring-way-niagara-on-the-lake-x4840140-4184587/",
"https://www.bungol.ca/listing/1073-concession-2-road-niagara-on-the-lake-x4732756-4085282/"
))  


class(bun_links)  

links <- sapply(df_all$link, as.character)

# initalize empty data frame where we will be storing our scraped data 
df_all_data <- data.frame()
head(df_all_data)
nrow(df_all_data)

# write our scraper function
scraper <- function(bun_links) {
  
  # save link in url object
  url <- bun_links
  # parse page url
  page <- xml2::read_html(url)
  Sys.sleep(0.25)
  
  remDr$navigate(bun_links)
  webElem <- remDr$findElement(using = 'id', value = "listingStatus")
  hprice <- webElem$getElementText()
  
  webElem2 <- remDr$findElement(using = 'id', value = "listingDateDifference")
  listingDateDifference <- webElem$getElementText()

  # storing individual links in df_individual_page object
  df_individual_page <- data.frame(price = hprice,
                                   dif = listingDateDifference)
                                   

}

# looping over all links in the vector and applying scraper function to each link
big <- sapply(bun_links, scraper)
