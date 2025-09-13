library(ggplot2)

cesDf <- read.csv("cesData.csv")

#Extract needed information
ces2023 <- cesDf[cesDf$Year %in% "2023" & 
                   cesDf$Month %in% "December" & 
                   cesDf$Area.Type %in% "County" &
                   grepl("San Bernardino|Riverside|Los Angeles|Orange|Imperial", 
                         cesDf$Area.Name) &
                   cesDf$Industry.Title %in% "Total, All Industries"
                 ,]

# Compute working-age population (as opposed to labor force as an attempt to 
# account for discouraged workers, which would likely be present in such high 
# unemployment)
# Source is Census ACS DP05 for the selected counties
# Hard coded because it would be a bit tedious to code filtering for such a 
# small set of data points, but I am aware it is generally not the best practice
over18 <- c(131573, 7812235, 2527531, 1938338, 1665130)

over65 <- c(25621, 1575175, 550224, 407608 ,294183)

# Set up a new DF with the information we need from the filtered DF
countyDf <- data.frame(County = ces2023$Area.Name,
                       Employment = ces2023$Current.Employment,
                       LaborForce = over18 - over65
                       )

# Computing Job Opportunity Index
# I am aware that the hard-coded positions are not of the best practice,
# however I am very new to R and learning each command as I go

# Counts the working-age population in the commutable regions
ieLaborMarket <- sum(countyDf$LaborForce[2:length(countyDf$LaborForce)])
icLaborMarket <- countyDf$LaborForce[1]

# Creates a dataframe for just the IE
ieMarket <- data.frame(Counties = countyDf$County[2:length(countyDf$County)],
                       Emp = countyDf$Employment[2:length(countyDf$Employment)],
                       LF = countyDf$LaborForce[2:length(countyDf$LaborForce)]
                       )
# Assigns the IE to all elements
ieMarket$Market <- "Inland Empire"
# Creates the JOI for each subregion by dividing the county's number of jobs by 
# the total commutable region's entire working age population
ieMarket$jobOpp <- ieMarket$Emp/ieLaborMarket

# The same for Imperial County
icMarket <- data.frame(Counties = countyDf$County[1],
                       Emp = countyDf$Employment[1],
                       LF = countyDf$LaborForce[1]
                       )

icMarket$Market <- "Imperial County"
icMarket$jobOpp <- icMarket$Emp/icLaborMarket

# Combines the two dataframes, so they can be emulated into a stacked bar graph
stackedJobOppDf <- rbind(icMarket, ieMarket)

# Combines the JOI for each subregion for the total region
jobOppTotal <- aggregate(jobOpp ~ Market, data = stackedJobOppDf, FUN = sum)

# Creates stacked bar graph
stackPlot <- ggplot(stackedJobOppDf, aes(x = Market, y = jobOpp, fill = Counties)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(y = "Job Opportunity Index", 
       x = "Labor Market",
       title = "Job Opportunity Index in Commutable Regions of Imperial County \nand the Inland Empire", 
       subtitle= "Where 'commutable' is a county whose major city is within 75mi of the major \ncity of each region") +
  # colors
  scale_fill_manual(values = c(rgb(152, 26, 49, maxColorValue = 255), 
                               rgb(158, 124, 10, maxColorValue = 255), 
                               rgb(124, 106, 85, maxColorValue = 255),
                               rgb(255, 225, 186, maxColorValue = 255), 
                               rgb(35, 31, 32, maxColorValue = 255))) +
  # Displays the bar totals
  geom_text(data = jobOppTotal, aes(x = Market,
                                    y = jobOpp,
                                    label = round(jobOpp, 2)), 
            vjust = -0.5, 
            # Prevents it from displaying the totals for each subregions instead
            # of combining it for the total region
            inherit.aes = FALSE) +
  # Adds padding around the whitespace
  theme(plot.margin = unit(c(1,1,1, 1), "cm")) +
  # Adds padding around the greyspace vertically
  scale_y_continuous(expand = expansion(mult = c(.1, .1))) +
  # Assigns column width
  geom_col(width = .5)

# Saves file
ggsave("loweJOI.png", plot = stackPlot)



