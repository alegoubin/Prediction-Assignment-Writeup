#functions for each plot, width and height are used to specifiy the canvas size for the saved file
#the data frame must be passed as an argument as well

buildPlot1 <- function (df,width,height) {
  
  #sets a 1 grid
  par(mfrow = c(1,1))
  
  with(df,hist(x = Global_active_power,col = "red", xlab = "Global Active Power (kilowatts)",main = paste("Global Active Power")))
  dev.copy(png, file="plot1.png", width=width, height=height)
  dev.off()
  cat("Plot1.png has been saved in", getwd())
  
}

