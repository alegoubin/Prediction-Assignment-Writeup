buildPlot4 <- function (df,width,height) {
  
  #sets a 2 * 2 grid
  par(mfrow = c(2,2))
  
  #1st plot
  with(df,plot(as.POSIXct(paste(Date,Time)),Global_active_power,type="l",ylab = "Global Active Power (kilowatts)",xlab=""))  
  
  #2nd plot
  with(df,plot(as.POSIXct(paste(Date,Time)),Voltage,type="l",ylab = "Voltage",xlab="datetime"))  
  
  #3rd plot
  with(df,plot(as.POSIXct(paste(Date,Time)),Sub_metering_1,type="l",ylab = "Energy sub metering",xlab=""))    
  with(df,lines(as.POSIXct(paste(Date,Time)),Sub_metering_2,col="red"))
  with(df,lines(as.POSIXct(paste(Date,Time)),Sub_metering_3,col="blue"))
  #legend without box + reduced size
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1),bty="n", cex=0.7)  
  
  #4th plot
  with(df,plot(as.POSIXct(paste(Date,Time)),Global_reactive_power,type="l",ylab = "Global_reactive_power",xlab="datetime"))  
  
  
  dev.copy(png, file="plot4.png", width=width, height=height)
  dev.off()
  cat("Plot4.png has been saved in", getwd())
  
}    






