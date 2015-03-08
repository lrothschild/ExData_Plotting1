#to run in RStudio, first source file, then call plot2()

setup<-function(){
        library(dplyr)
        library(lubridate)
        #Read in lines of data needed. The line numbers were found by using TextWrangler
        # in original text and searching for 1/2/2007 00:00:00.
        power<-read.csv("household_power_consumption.txt",sep=";",nrows=2880,skip=66637)
        #Since column names were missed on reading, they have to be added here.
        colnames(power)<-c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity",
                           "Sub_metering_1","Sub_metering_2","Sub_metering_3")
        #Use lubridate to create useful columns
        power<-tbl_df(power)
        power$Date<-dmy(as.character(power$Date))
        power$Time<-hms(power$Time)
        power<-mutate(power,Day=mday(Date))
        power<-mutate(power,Hour=hour(Time))
        power<-mutate(power,Minute=minute(Time))
}
plot2<-function(){
        power<-setup()
        #set some parameters for printing
        par(pch=22,col="black",mfrow=c(1,1),mar=c(5,5,5,5),cex.axis=.8)
        x<-1:2880
        y<-power$Global_active_power
        plot(x,y,type="n",axes=FALSE,frame.plot=TRUE,xlab="",
             ylab="Global Active Power (kilowatts)")
        #print graph on screen
        lines(x,y,type="l")
        axis(side=2,at=c(0,2,4,6,8),labels =c(0,2,4,6,""))
        axis(side=1,at=c(0,1440,2880),labels=c("Thu","Fri","Sat"))
        #print graph as png
        dev.copy(png,file="plot2.png",width = 480, height = 480)
        dev.off()
}