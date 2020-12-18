#read table data
t <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", 
                colClasses = c('character','character','numeric','numeric',
                               'numeric','numeric','numeric','numeric','numeric'))
head(t)
summary(t)
str(t)

#set the names of the data 
names(t) <- c("Date","Time","Global_active_power","Global_reactive_power",
                      "Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
## Format date to Type Date
t$Date <- as.Date(t$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
t <- subset(t,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

#calling the basic plot function
hist(as.numeric(as.character(t$Global_active_power)),
     col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")

# annotating graph
title(main="Global Active Power")

#copying to png
dev.copy(png, filename="plot1.png")
dev.off()