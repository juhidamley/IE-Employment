library(ggplot2)

cesDf <- read.csv("cesData.csv")

ces2023 <- cesDf[cesDf$Year %in% "2023" & 
                   cesDf$Month %in% "December" & 
                   cesDf$Area.Type %in% "County" &
                   grepl("San Bernardino|Riverside|Los Angeles|Orange|Imperial", 
                         cesDf$Area.Name) &
                   cesDf$Industry.Title %in% "Total, All Industries"
                 ,]

over18 <- c(131573, 7812235, 2527531, 1938338, 1665130)

over65 <- c(25621, 1575175, 550224, 407608 ,294183)

countyDf <- data.frame(County = ces2023$Area.Name,
                       Employment = ces2023$Current.Employment,
                       LaborForce = over18 - over65
                       )

ieJobMarket <- sum(countyDf$Employment[2:length(countyDf$Employment)])
icJobMarket <- countyDf$Employment[1]

ieLaborMarket <- sum(countyDf$LaborForce[2:length(countyDf$LaborForce)])
icLaborMarket <- countyDf$LaborForce[1]

ieJobOpp <- ieJobMarket / ieLaborMarket
icJobOpp <- icJobMarket / icLaborMarket

ieMarket <- data.frame(Counties = countyDf$County[2:length(countyDf$County)],
                       Emp = countyDf$Employment[2:length(countyDf$Employment)],
                       LF = countyDf$LaborForce[2:length(countyDf$LaborForce)]
                       )
ieMarket$Market <- "IE Market"
ieMarket$jobOpp <- ieMarket$Emp/ieLaborMarket

icMarket <- data.frame(Counties = countyDf$County[1],
                       Emp = countyDf$Employment[1],
                       LF = countyDf$LaborForce[1]
                       )

icMarket$Market <- "IC Market"
icMarket$jobOpp <- icMarket$Emp/icLaborMarket

stackedJobOppDf <- rbind(icMarket, ieMarket)

jobOppTotal <- aggregate(jobOpp ~ Market, data = stackedJobOppDf, FUN = sum)

stackPlot <- ggplot(stackedJobOppDf, aes(x = Market, y = jobOpp, fill = Counties)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5)

print(stackPlot)



