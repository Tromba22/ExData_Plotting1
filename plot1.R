library(dplyr)
# Download and unzip the file:
dir.create("./air_pollution")
urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(urlzip, destfile = "./air_pollution.zip" )
unzip("./air_pollution.zip", exdir = "./air_pollution" )
# Load the data:
NEI <- readRDS("./air_pollution/summarySCC_PM25.rds")
SCC <- readRDS("./air_pollution/Source_Classification_Code.rds")
# Check NEI data
str(NEI)
# Check SCC data
str(SCC)
#♦select and filter the total emission and year
tot_emissions <- NEI %>%
  select(Emissions, year) %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))
#plot the total emission by year
plot(tot_emissions$year, tot_emissions$Total_Emissions, type = "o", 
     col = "steelblue3",
     xlab = "Year", ylab = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
     main = expression("Total US" ~ PM[2.5] ~ "Emissions by Year"))
