#read table data
t <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", 
                colClasses = c('character','character','numeric','numeric',
                               'numeric','numeric','numeric','numeric','numeric'))


#set the names of the data 
names(t) <- c("Date","Time","Global_active_power","Global_reactive_power",
              "Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
## Format date to Type Date
t$Date <- as.Date(t$Date, "%d/%m/%Y")
t$Time <- strptime(t$Time, format="%H:%M:%S")
t[1:1440,"Time"] <- format(t[1:1440,"Time"],"2007-02-01 %H:%M:%S")
t[1441:2880,"Time"] <- format(t[1441:2880,"Time"],"2007-02-02 %H:%M:%S")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
t <- subset(t,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
t$Sub_metering_1 <- as.numeric(as.character(t$Sub_metering_1))
t$Sub_metering_2 <- as.numeric(as.character(t$Sub_metering_2))
t$Sub_metering_3 <- as.numeric(as.character(t$Sub_metering_3))

# calling the basic plot functions
plot(t$Time,t$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
with(t,lines(Time,Sub_metering_1))
with(t,lines(Time,Sub_metering_2,col="red"))
with(t,lines(Time,Sub_metering_3,col="blue"))
legend("topright", lty=1, col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

# annotating graph
title(main="Energy sub-metering")

#copying to png
dev.copy(png, filename="plot3.png")
dev.off()
