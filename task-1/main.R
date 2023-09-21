library(psych)
library(tidyverse)

# dont use multiple times
setwd(file.path(getwd(), "task-1"))

data <- read.csv("data.csv")

# 2. priešanalizė
summary(data)
describeBy(data, "Industry")

# 4. praleistų reikšmių užpildymas
# fill in industries
data[data$Industry == "", ]
data[14, "Industry"] <- "IT Services"
data[15, "Industry"] <- "Financial Services"
data[136, "Industry"] <- "Construction"
unique(data$Industry)
