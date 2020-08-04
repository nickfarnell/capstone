### machine learning text ###
### https://bradleyboehmke.github.io/HOML/intro.html

install.packages("AmesHousing")

# access data
ames <- AmesHousing::make_ames()

# initial dimension
dim(ames)

# response variable
head(ames$Sale_Price)

#column headings
names(ames)
