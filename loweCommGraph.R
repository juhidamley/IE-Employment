# We need this library to make our graphs
library(ggplot2)

# Import census CSV
# We skip 45 rows as they do not contain the information we want
# We set header to false to avoid headers being assigned accidentally
censusFile <- read.csv("censusJWMNP.csv", skip = 45, header = FALSE)

# We give the columns more descriptive titles, as JWMNP isn't very intuitive 
colnames(censusFile) <- c("Geography", "Commute")

# Next, we find the values for the new df

# Since SB and RS were divided into many different cities, we are using the average
ieCommute <- mean(censusFile$Commute[grepl("Riverside|San Bernardino", censusFile$Geography)])

impCommute <- censusFile$Commute[grepl("Imperial", censusFile$Geography)]

# Since all the entries contain California, we use ^ to match the beginning of 
# the string and $ to match the end of the string, so it searches for the entry
# simply titled California
calCommute <- censusFile$Commute[grepl("^California$", censusFile$Geography)]

# Create the new dataframe

commuteTimes <- data.frame(
  Region = c("California", "Imperial County", "Inland Empire"),
  Commute = c(calCommute, impCommute, ieCommute)
)

# Barplot

# Creates the plot and assigns the data
commutePlot <- ggplot(data=commuteTimes, mapping=aes(x=Region, y=Commute, fill=Region)) + 
  # Assigns it as a bar graph
  geom_bar(stat = "identity") +
  # Assigns labels 
  labs(y = "Commute (in minutes)", 
       title = "Daily Commute Times in Selected Regions", 
       subtitle= "Where 'commute' is the census variable JWMNP: Travel time to work") +
  # Assigns colors
  scale_fill_manual(values = c(rgb(152, 26, 49, maxColorValue = 255), 
                               rgb(158, 124, 10, maxColorValue = 255), 
                               rgb(124, 106, 85, maxColorValue = 255))) +
  # Displays the bar totals
  geom_text(aes(label = round(Commute, 2), vjust = -0.5)) +
  # Remove legend as it would be redundant and adds padding around the whitespace
  theme(legend.position = "none",
        plot.margin = unit(c(1,1,1, 1), "cm")) +
  # Adds padding around the greyspace vertically
  scale_y_continuous(expand = expansion(mult = c(.1, .1))) +
  # Assigns column width
  geom_col(width = .5)

# Creates image file
ggsave("loweCommute.png", plot = commutePlot)

