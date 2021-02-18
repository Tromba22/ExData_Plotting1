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
#select and filter date in baltimore by type each year
tot_emi_baltitype <- NEI %>%
  filter(fips == 24510) %>%
  select(fips, type, Emissions, year) %>%
  group_by(year, type) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))
#plot data in Baltimore dpeneding on type 
Baltimore_By_Type <- ggplot(tot_emi_baltitype, 
                            aes(x = factor(year), 
                                y = Total_Emissions, fill = type, 
                                color = type)) +
  geom_bar(stat = "identity" ) +
  facet_grid(.~type, scales = "free", space = "free") + 
  labs(x = "Year", y = "Emissions (Tons)", 
       title = "Total Emissions By Type In Baltimore City, 
       Maryland From 1999 - 2008") +
  theme(plot.title = element_text(size = 7),
        axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_fill_brewer(direction = -1) +
  theme_grey()+
  ggsave("plot3.png", width = 30, height = 30, units = "cm")
print(Baltimore_By_Type)