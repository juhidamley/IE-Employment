library(ggplot2)

cesDf <- read.csv("cesData.csv")

ces2024 <- cesDf[cesDf$Year %in% "2024" & cesDf$Month %in% "December",]

head(ces2024)


