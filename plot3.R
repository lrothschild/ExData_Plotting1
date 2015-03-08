#to run in RStudio, first source file, then call plot3()

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
plot3<-function(){
        #construct data table "power"
        power<-setup()
        par(pch=22,mfrow=c(1,1),mar=c(6.5,5,6.5,5),cex.axis=.8)
        #first draw line graph for y<-power3$Sub_metering_1 as function of time
        x<-1:2880
        y1<-power$Sub_metering_1
        plot(y1,type="n",
             frame.plot=TRUE,ylim=c(0,40),xlim=c(0,2880),xlab="",ylab="",axes=FALSE,asp=65)
        lines(x,y1,type="l")
        # draw line graph for y<-power3$Sub_metering_2 on same chart
        par(new=TRUE)
        y2<-power$Sub_metering_2
        plot(y2, type="n",xlab="",ylab="",ylim=c(0,40),xlim=c(0,2880),axes=FALSE,asp=65)
        lines(x,y2,type="l",col="red")
        # draw line graph for y<-power3$Sub_metering_3 on same chart
        par(new=TRUE)
        y3<-power$Sub_metering_3
        plot(y3, type="n",xlab="",ylab="",ylim=c(0,40),xlim=c(0,2880),axes=FALSE,asp=65)
        lines(x,y3,type="l",col="blue")
        #add legend axes and title for y axis
        Names<-c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        legend("topright",cex=.72,legend=Names,bty="o", col=c("black","red","blue"),
               box.lwd=1,lty=1)
        axis(side=1,at=c(0,1440,2880),labels=c("Thu","Fri","Sat"))
        axis(side=2,at=c(0,10,20,30))
        par(cex=.7)
        title(ylab="Energy sub metering")
        dev.copy(png,file="plot3.png",width = 480, height = 480)
        dev.off()
}
        
