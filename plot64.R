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
# select the different levels of coal emission in the UN
SCC_Coal_Comb <- SCC %>%
  filter(grepl('[Cc]ombustion', SCC.Level.One)) %>%
  filter(grepl("[Cc]oal", SCC.Level.Three)) %>%
  select(SCC, SCC.Level.One, SCC.Level.Three)
#join the coal data to the other emissions 
NEI_Coal_Comb <- inner_join(NEI, SCC_Coal_Comb, by = "SCC")
#plot the emission of gazes related to coal
NEI_Coal_Comb_Plot <- ggplot(NEI_Coal_Comb, aes(factor(year), Emissions)) +
  geom_bar(stat = "identity", fill = "peachpuff3", width = 0.5) +
  labs(x = "Year", y = expression("Total PM"[2.5]*" Emission (10^5 Tons)"),
       title =expression("PM"[2.5]*" Coal Combustion Source 
                         Emissions Across US from 1999-2008")) +
  scale_fill_brewer(direction = -1) + 
  theme_economist() +
  ggsave("plot4.png", width = 30, height = 30, units = "cm")


print(NEI_Coal_Comb_Plot)
