#mapping the sampled sothebys data
library(dplyr)
library(stringr)


SampleSothebys <- read.csv("Sample_Sothebys.csv", stringsAsFactors = FALSE)

names(SampleSothebys)
#"X"       "price"   "address" "squares" "type"    "year"    "age"     "bed"     "bath"    "tax"  

SampleSothebys$price2 = gsub("\\$","", SampleSothebys$price)
SampleSothebys$price2 =as.numeric(gsub("\\,","", SampleSothebys$price2))
SampleSothebys$age2 =as.numeric(SampleSothebys$age)
SampleSothebys$bed2 =as.numeric(SampleSothebys$bed)

#new column for province
SampleSothebys <- SampleSothebys %>%
  mutate(Prov = str_sub(address,-3,-1))




# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(SampleSothebys))
{
  # Print("Working...")
  result <- geocode(SampleSothebys$address[i], output = "latlona", source = "google")
  SampleSothebys$lon[i] <- as.numeric(result[1])
  SampleSothebys$lat[i] <- as.numeric(result[2])
  SampleSothebys$geoAddress[i] <- as.character(result[3])
}


class(SampleSothebys$address)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("jessecambon/tidygeocoder")

# install.packages("tidygeocoder")  #cant install current CRAN version on R<4.0
library(tidygeocoder) 
library(tibble)

dc_addresses <- tribble( ~name,~addr,
                         "White House", "1600 Pennsylvania Ave Washington, DC",
                         "National Academy of Sciences", "2101 Constitution Ave NW, Washington, DC 20418",
                         "Department of Justice", "950 Pennsylvania Ave NW, Washington, DC 20530",
                         "Supreme Court", "1 1st St NE, Washington, DC 20543",
                         "Washington Monument", "2 15th St NW, Washington, DC 20024")

coordinates <- dc_addresses %>%
  geocode(addr)

new_add <- as_tibble(SampleSothebys)

coordinates2 <- new_add %>%
  geocode(address)
view(coordinates2)

lat_longs <- SampleSothebys %>%
  geocode(address, method = 'osm', lat = latitude2 , long = longitude2)
view(lat_longs)

#split the address apart to remove extra number for condos
a <- grep("^[0-9]{1,20}\\s[0-9]", SampleSothebys$address, value = TRUE)
b <- sub("^[0-9]{1,10}\\s[0-9]", "", SampleSothebys$address) #this removes the unit number, if one exists by searching for a pattern of number space number then removing the fir

c <- "^[0-9]{1,20}\\s[0-9]"
d <- sub(c, f, SampleSothebys$address) #this removes the unit number, if one exists by searching for a pattern of number space number then removing the fir

e <- regmatches(SampleSothebys$address, regexpr(c, SampleSothebys$address))
f <- str_sub(e, -2,-1)

g <- str_sub(regmatches(SampleSothebys$address, regexpr("^[0-9]{1,20}\\s[0-9]", SampleSothebys$address)), -2,-1)


StreetAddress <- gsub("([0-9]{1,20})\\s([0-9])", "\\2", SampleSothebys$address)

names(SampleSothebys)
head(SampleSothebys)

SampleSothebys <- SampleSothebys %>%
  mutate(StAddress = gsub("([0-9]{1,20})\\s([0-9])", "\\2", SampleSothebys$address))


lat_longs <- SampleSothebys %>%
  geocode(StAddress, method = 'osm', lat = latitude2 , long = longitude2)
view(lat_longs)











###Start### get city

cit <- gsub("\\,\\s[A-z]*", "", SampleSothebys$address)
grep("\\,\\s[A-z]*", SampleSothebys$address)
str_extract(SampleSothebys$address, "\\,\\s[A-z]*\\,")

str_extract(SampleSothebys$address, "(\\,\\s[A-z]*)\\,([A-z])")

gsub("(\\,\\s[A-z]*)\\,([A-z])", "", SampleSothebys$address)

gsub("([0-9]{1,20})\\s([0-9])", "\\2", df$Address)




###End### get city


library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs, aes(longitude2, latitude2), color="grey99") +
  borders("world") + geom_point() + 
  #geom_label_repel(aes(label = name)) + 
  theme_void()












ID <- c(1,2,3,4)
Address <- c("123 Fake St, Toronto, Ontario, Canada", "44 332 Pretend St, Toronto, Ontario, Canada", "3 299 New Road, Ottawa, Ontario, Canada", "2552 25 Old Lane, Toronto, Ontario, Canada")
Type <- c("House", "Condo", "Condo", "Condo")

df <- data.frame(ID, Address, Type)
df
       
      
#find the pattern when the start of Address is at least one number, then a space, then another number
k <- grep("^[0-9]{1,20}\\s[0-9]", df$Address, value = TRUE) #returns the full string when there is a match
l <- sub("^[0-9]{1,10}\\s[0-9]", "", df$Address) #this was my next thought, but it also removes the first digit of the real address

l2 <- sub("(?<[0-9]) (?=\\s)", "", df$Address) #this was my next thought, but it also removes the first digit of the real address


m <- gsub("^[0-9]{1,10}\\s[0-9]", "\\1\\5", df$Address) #this was my next thought, but it also removes the first digit of the real address
m


j<- gsub("(?<=^[0-9]{1,10}) (?=\\s[0-9])", "f", df$Address) #this was my next thought, but it also removes the first digit of the real address
j



x <- "A. J. Burnett"
gsub("([A-Z])\\.\\s([A-Z])\\.", "\\1.\\2.", x)


gsub("([0-9]{1,20})\\s([0-9])", "\\2", df$Address)







