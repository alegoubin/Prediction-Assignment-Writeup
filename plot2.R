buildPlot2 <- function (df,width,height) {
  
  #sets a 1 grid
  par(mfrow = c(1,1))
  
  with(df,plot(as.POSIXct(paste(Date,Time)),Global_active_power,type="l",ylab = "Global Active Power (kilowatts)",xlab=""))  
  dev.copy(png, file="plot2.png", width=width, height=height)
  dev.off()
  cat("Plot2.png has been saved in", getwd())
  
}  

