library(ggplot2)
library(RColorBrewer)
library(ggthemes)
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
#select vehicles from the SCC data
SCC_Vehicles <- SCC %>%
  filter(grepl('[Vv]ehicle', SCC.Level.Two)) %>%
  select(SCC, SCC.Level.Two)
#merge the vehicle with their emissions
Tot_Emi_24510_V <- NEI %>%
  filter(fips == "24510") %>%
  select(SCC, fips, Emissions, year) %>%
  inner_join(SCC_Vehicles, by = "SCC") %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  select(Total_Emissions, year)
#plot the emission of cars across the years
Baltimore_Vehicles_Plot <- ggplot(Tot_Emi_24510_V, aes(factor(year), 
                                                       Total_Emissions)) +
  geom_bar(stat = "identity", fill = "sienna3", width = 0.5) +
  labs(x = "Year", y = "Emissions (Tons)",
       title = "Total Motor Vehicle Related Emissions In Baltimore City From 1999 - 2008") +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  ggsave("plot5.png", width = 30, height = 30, units = "cm")

print(Baltimore_Vehicles_Plot)
