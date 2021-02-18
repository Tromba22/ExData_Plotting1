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
tot_emissions <- NEI %>%
  select(Emissions, year) %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))
png("plot1.png")
plot(tot_emissions$year, tot_emissions$Total_Emissions, type = "o", 
     col = "steelblue3",
     xlab = "Year", ylab = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
     main = expression("Total US" ~ PM[2.5] ~ "Emissions by Year"))
dev.off()
tot_emi_balti <- NEI %>%
  filter(fips == 24510) %>%
  select(fips, Emissions, year) %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))
png("plot2.png")
plot(x = tot_emi_balti$year, y = tot_emi_balti$Total_Emissions,
     type = "o", 
     main = expression("Total Baltimore" ~ PM[2.5] ~ "Emissions by Year"), 
     xlab = "Year", 
     ylab = expression("Total Baltimore "~ PM[2.5] ~ "Emissions"),
     col = "steelblue3")
dev.off()
tot_emi_baltitype <- NEI %>%
  filter(fips == 24510) %>%
  select(fips, type, Emissions, year) %>%
  group_by(year, type) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE))
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
Baltimore_By_Type
SCC_Coal_Comb <- SCC %>%
  filter(grepl('[Cc]ombustion', SCC.Level.One)) %>%
  filter(grepl("[Cc]oal", SCC.Level.Three)) %>%
  select(SCC, SCC.Level.One, SCC.Level.Three)

NEI_Coal_Comb <- inner_join(NEI, SCC_Coal_Comb, by = "SCC")
NEI_Coal_Comb_Plot <- ggplot(NEI_Coal_Comb, aes(factor(year), Emissions)) +
  geom_bar(stat = "identity", fill = "peachpuff3", width = 0.5) +
  labs(x = "Year", y = expression("Total PM"[2.5]*" Emission (10^5 Tons)"),
       title =expression("PM"[2.5]*" Coal Combustion Source 
                         Emissions Across US from 1999-2008")) +
  scale_fill_brewer(direction = -1) + 
  theme_economist() +
  ggsave("plot4.png", width = 30, height = 30, units = "cm")


print(NEI_Coal_Comb_Plot)
SCC_Vehicles <- SCC %>%
  filter(grepl('[Vv]ehicle', SCC.Level.Two)) %>%
  select(SCC, SCC.Level.Two)

Tot_Emi_24510_V <- NEI %>%
  filter(fips == "24510") %>%
  select(SCC, fips, Emissions, year) %>%
  inner_join(SCC_Vehicles, by = "SCC") %>%
  group_by(year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  select(Total_Emissions, year)
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
SCC_Vehicles <- SCC %>%
  filter(grepl('[Vv]ehicle', SCC.Level.Two)) %>%
  select(SCC, SCC.Level.Two)

Tot_Emi_Two_Locs <- NEI %>%
  filter(fips == "24510" | fips == "06037") %>%
  select(fips, SCC, Emissions, year) %>%
  inner_join(SCC_Vehicles, by = "SCC") %>%
  group_by(fips, year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  select(Total_Emissions, fips, year)
Tot_Emi_Two_Locs$fips <- gsub("24510", "Baltimore City", Tot_Emi_Two_Locs$fips)
Tot_Emi_Two_Locs$fips <- gsub("06037", "Los Angeles County", Tot_Emi_Two_Locs$fips)
Two_Locs_Plot <- ggplot(Tot_Emi_Two_Locs, aes(x = factor(year), y = Total_Emissions, fill = fips)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_grid(.~fips) + 
  labs(x = "Year", y = "Emissions (Tons)", 
       title = "Comparison of Motor Vehicle Related Emissions 
       Between Baltimore City and Los Angeles From 1999 - 2008") +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 12)) +
  theme_dark() + 
  ggsave("plot6.png", width = 30, height = 30, units = "cm")

print(Two_Locs_Plot)
