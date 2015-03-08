#Code for Plot 1.  Function "setup" reads and cleans data to produce a data table.  
#The function "plot1" reads the data table produced by setup as "power" and prints
##a histogram for Global active power. The lines for reading in the data were
##determined by using TextWrangler to number the lines and then again using the Find to
## find the line numbers of the desired dates.

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
plot1<-function(){
        #define data table power
        power<-setup()
        par(cex.axis=.4)
        #print histogram to screen
        hist(power$Global_active_power,col="red",main="Global Active Power"
             ,xlab="Global Active Power (kilowatts)")
        #copy to png
        dev.copy(png,file="plot1.png",width = 480, height = 480)
        dev.off()
}
        