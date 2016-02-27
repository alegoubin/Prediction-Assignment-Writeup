buildPlot3 <- function (df,width,height) {
  
  #sets a 1 grid
  par(mfrow = c(1,1))
  
  with(df,plot(as.POSIXct(paste(Date,Time)),Sub_metering_1,type="l",ylab = "Energy sub metering",xlab=""))    
  with(df,lines(as.POSIXct(paste(Date,Time)),Sub_metering_2,col="red"))
  with(df,lines(as.POSIXct(paste(Date,Time)),Sub_metering_3,col="blue"))
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))  
  
  dev.copy(png, file="plot3.png", width=width, height=height)
  dev.off()
  cat("Plot3.png has been saved in", getwd())
  
}    

