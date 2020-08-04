### machine learning text ###
### https://bradleyboehmke.github.io/HOML/intro.html

# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
install.packages("rsample")
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
install.packages("h2o")
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o