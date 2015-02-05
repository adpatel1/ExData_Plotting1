#readin() reads in data from the desired dates, Feb 1 and Feb 2. 
#It uses the Date column to find the appropriate rows to read completely in
#note: this saves a (little) bit of time compared with reading everything and subsetting
readin <- function(fname="household_power_consumption.txt"){
  Date <- read.csv(fname,header=TRUE,as.is=TRUE,sep=";",colClasses=c(NA,rep("NULL",8)))
  rows<-which(Date=="1/2/2007"|Date=="2/2/2007")
  cols<-read.csv(fname,nrows=1,sep=";")
  dat <- read.csv(fname,header=FALSE,as.is=TRUE,sep=";",skip=min(rows),nrows=1+max(rows)-min(rows),col.names=names(cols))
  dat$Date.Time <- strptime(paste(dat$Date, dat$Time),  format = "%d/%m/%Y %H:%M:%S")
  dat
}

#makePlot3 makes the requested plot3 in a png file
makePlot3 <- function(dat=readin()){
  png(filename="plot3.png",width=480,height=480,units="px",bg="transparent")
  par(mar=c(5,4,3,2))
  with(dat,plot(Date.Time,Sub_metering_1,type="n",xlab="",ylab="Energy sub metering"))
  with(dat,lines(Date.Time,Sub_metering_1))
  with(dat,lines(Date.Time,Sub_metering_2, col = "red"))
  with(dat,lines(Date.Time,Sub_metering_3, col = "blue"))
  legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("black","red","blue"))
  dev.off()
}