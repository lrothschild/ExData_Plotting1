#to run in RStudio, first source file and then call plot4()

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
        #add Qrt to label date-time by quarter hours
        power<-mutate(power,Qrt=(Day-1)*4*24 +Hour*4+ceiling(Minute/15))
}
#Set up plot at (2,2) as a function to be called
plot4_2_2<-function(){
        power<-setup()
        splus <- list(boxwex=2, staplewex=0, outwex=1, boxfill="darkgrey",
                      medlwd=2, medcol="black", whisklty=1, outlty=1, outpch=" ")
        #draw boxplot of Global_reactive_power at each quarter hour
        boxplot(Global_reactive_power~Qrt,data=power,range=0,ylim=c(0,.5),
                xlim=c(1,192),pars=splus,axes=FALSE,frame.plot=TRUE)
        par(cex=.7)
        axis(side=1,at=c(1,96,192),labels=c("Thu","Fri","Sat"))
        axis(side=2,at=c(0,.1,.2,.3,.4,.5),labels =c("0.0",0.1,0.2,0.3,0.4,0.5))
        title(xlab="datetime")
        title(ylab="Global_reactive_power")
}        
plot4<-function(){
        power<-setup()
        par(pch=22,col="black",mfrow=c(2,2),mar=c(5,5,2,2.5),cex.axis=.8)
        #draw plot at (1,1):
        x<-1:2880
        y<-power$Global_active_power
        plot(x,y,type="n",axes=FALSE,frame.plot=TRUE,xlab="",
             ylab="Global Active Power (kilowatts)")
        #print graph on screen at (1,1)
        lines(x,y,type="l")
        axis(side=2,at=c(0,2,4,6,8),labels =c(0,2,4,6,""))
        axis(side=1,at=c(0,1440,2880),labels=c("Thu","Fri","Sat"))
        #draw plot at (1,2):
        #first draw line graph for y<-as.numeric(power3$Sub_metering_1),
        x<-1:2880
        y<-power$Voltage
        plot(y,type="n",
             frame.plot=TRUE,ylim=c(233,247),xlab="",ylab="",axes=FALSE)
        #print on screen at (1,2) and add notations:
        lines(x,y,type="l")
        axis(side=1,at=c(0,1440,2880),labels=c("Thu","Fri","Sat"))
        axis(side=2,at=c(234,236,238,240,242,244,246),labels=c(234,"",238,"",242,"",246))
        title(ylab="Voltage")
        title(xlab="datetime")
        #plot for (2,1)
        x<-1:2880
        y1<-power$Sub_metering_1
        plot(y1,type="n",
             frame.plot=TRUE,ylim=c(0,40),xlim=c(0,2880),xlab="",ylab="",axes=FALSE,asp=65)
        lines(x,y1,type="l")
        par(new=TRUE)
        y2<-power$Sub_metering_2
        plot(y2, type="n",xlab="",ylab="",ylim=c(0,40),xlim=c(0,2880),axes=FALSE,asp=65)
        lines(x,y2,type="l",col="red")
        par(new=TRUE)
        y3<-power$Sub_metering_3
        plot(y3, type="n",xlab="",ylab="",ylim=c(0,40),xlim=c(0,2880),axes=FALSE,asp=65)
        lines(x,y3,type="l",col="blue")
        #add legend and other notations
        Names<-c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
        legend(list(x = 1450,y = 45),cex=.5,legend=Names,bty="o", col=c("black","red","blue"),
               box.lwd=1,lty=1)
        axis(side=1,at=c(0,1470,2880),labels=c("Thu","Fri","Sat"))
        axis(side=2,at=c(0,10,20,30))
        title(ylab="Energy sub metering")
        #plot for (2,2):
        plot4_2_2()
        #print all to png file
        dev.copy(png,file="plot4.png",width = 480, height = 480)
        dev.off()
}

        
        

