library(dbplyr)
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
#select the total emission and year in Baltimore
tot_emi_balti <- NEI %>%
  filter(fips == 24510) %>%
  select(fips, Emissions, year) %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))
#plot the emission by year in Baltimore
plot(x = tot_emi_balti$year, y = tot_emi_balti$Total_Emissions,
     type = "o", 
     main = expression("Total Baltimore" ~ PM[2.5] ~ "Emissions by Year"), 
     xlab = "Year", 
     ylab = expression("Total Baltimore "~ PM[2.5] ~ "Emissions"),
     col = "steelblue3")
