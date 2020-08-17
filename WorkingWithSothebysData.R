#working the sampled sothebys data
library(dplyr)
library(stringr)

SampleSothebys <- read.csv("Sample_Sothebys.csv")

names(SampleSothebys)
#"X"       "price"   "address" "squares" "type"    "year"    "age"     "bed"     "bath"    "tax"  

SampleSothebys$price2 = gsub("\\$","", SampleSothebys$price)
SampleSothebys$price2 =as.numeric(gsub("\\,","", SampleSothebys$price2))
SampleSothebys$age2 =as.numeric(SampleSothebys$age)
SampleSothebys$bed2 =as.numeric(SampleSothebys$bed)

mean(SampleSothebys$price2)
class(SampleSothebys$price2)



#new column for province
SampleSothebys <- SampleSothebys %>%
  mutate(Prov = str_sub(address,-3,-1))

#summarize showing average price by province
SampleSothebys %>%
  group_by(Prov) %>%
  summarize(num = n(),
            mean_price = mean(price2, na.rm = TRUE),
            min_price = min(price2, na.rm = TRUE),
            max_price = max(price2, na.rm = TRUE)
  )


#summarize showing average age by province, with NA removed
SampleSothebys %>%
  group_by(Prov) %>%
  summarize(mean_age = mean(age2, na.rm = TRUE))

#summarize bedrooms
SampleSothebys %>%
  group_by(Prov) %>%
  summarize(num = n(),
            mean_bed = mean(bed2, na.rm = TRUE),
            min_bed = min(bed2, na.rm = TRUE),
            max_bed = max(bed2, na.rm = TRUE)
            )



