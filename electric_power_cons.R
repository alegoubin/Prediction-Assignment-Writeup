#function to read and subset data for the plotting, it will return it into the global env for future use
electric_power_cons <- function(wd) {
  
  
  # set working directorw with source file and saved plots
  setwd(wd)
  
  ## Read source data
  fullData <- read.csv("household_power_consumption.txt", header = TRUE, sep = ";")
  
  ## Convert fields into correct format
  fullData$Date<-as.Date(fullData$Date,format="%d/%m/%Y")
  fullData$Time<-strptime(x = as.character(fullData$Time),format = "%H:%M:%S")
  fullData$Time<- format(fullData$Time, "%H:%M:%S")
  
  ##subset data
  fullData <- fullData[(fullData$Date=="2007-02-01") | (fullData$Date=="2007-02-02"),]
  
  ## Convert fields into correct format
  fullData$Global_active_power <- as.numeric(as.character(fullData$Global_active_power))
  fullData$Global_reactive_power<- as.numeric(as.character(fullData$Global_reactive_power))
  fullData$Voltage<- as.numeric(as.character(fullData$Voltage))  
  fullData$Global_intensity <- as.numeric(as.character(fullData$Global_intensity))
  fullData$Sub_metering_1 <- as.numeric(as.character(fullData$Sub_metering_1))    
  fullData$Sub_metering_2 <- as.numeric(as.character(fullData$Sub_metering_2))   
  fullData$Sub_metering_3 <- as.numeric(as.character(fullData$Sub_metering_3))    
  
  return(fullData)
  
}  

