#DO data script
#Matthew Powers 
#powerm3@oregonstate.edu
#Analyzes and visualizes DO data from BOB and SH populations
#Last edited 1-21-25

#load required/preferred packages
library(MESS) #data wrangling and modeling
library(stats) #data wrangling and modeling
library(lme4) #data wrangling and modeling
library(lmerTest) #Get null hypothesis test from lme4 lmer models
library(emmeans) #data wrangling and modeling
library(ggpubr) #Also loads basic ggplot2
library(cowplot) #Pretty ggplots!
library(reshape2) #Data wrangling
library(dplyr) #Data wrangling
library(tidyverse) #Data wrangling
library(stringr) #data wrangling for splitting column characters
library(MetBrewer) #Pretty colors!
library(runner) #Do functions on sliding windows of data
library(scales) #Pretty colors!
library(palettetown) #Pretty ggplots!
library(grid) #for adding tables to plots and making multiple panels
library(gridExtra) #for adding tables to plots and making multiple panels
library(MuMIn) #For getting R squared values from mixed models
library(glue) #Stick stuff in other stuff! Like pasting text in data frames and plot elements
library(data.table) #Read in and work with text files and other tables
library(gifski) #Save animated figures
library(gganimate) #Make animated plots
library(patchwork) #stitching graphs together
library(ggiraph) #Connected interactive graphs


#Set theme globally
theme_set(theme_cowplot())

#Use to generate greek letters to copy and paste into labels
greek <- paste("\U03B1","\U03B2","\U03B5","\u00B0","\U03C0","\U03BC") #Add whichever symbol unicodes you want to the list
greek #Run this line and then copy and paste whatever symbol you want from the list


#Sunrise and sunset data
datum.ss <- read.csv(file = "Sunrise and Sunset times.csv", header = TRUE)

str(datum.ss)

#Add leading zero to get in right format
datum.ss$Month.numeric <- sprintf('%02d', datum.ss$Month.numeric) #For months
datum.ss$Day <- sprintf('%02d', datum.ss$Day) #For days

#Combine year-month-day to make date variable
datum.ss$Full.Date <- paste(datum.ss$Year, datum.ss$Month.numeric, datum.ss$Day, sep="-")

#combine date and time columns for sunrise and sunset
datum.ss$Date.rise <- paste(datum.ss$Full.Date, datum.ss$Sunrise, sep=" ")
datum.ss$Date.set <- paste(datum.ss$Full.Date, datum.ss$Sunset, sep = " ")

#Convert sunrise and sunset times to posix format
datum.ss$Date.rise <- as.POSIXct(datum.ss$Date.rise, format = "%Y-%m-%d %H:%M:%S")
datum.ss$Date.set <- as.POSIXct(datum.ss$Date.set, format = "%Y-%m-%d %H:%M:%S")

#Create month day column. This should be run after formatting the day with the leading 0
datum.ss$Month.day <- paste(datum.ss$Month, datum.ss$Day)

#Relevel month variable to be in chrono order
datum.ss$Month <- factor(datum.ss$Month, levels = c("June", "July", "August", "September"))

#Add another column with full date written as day of year for filtering
datum.ss$DOY <- yday(datum.ss$Full.Date)
    
#For SH data
    #Read in data
    datum.SH <- read.table(file="SH_pool3_30JUN-27SEP_Full.txt", header = F, strip.white = T, skip =9, sep=",")
    
    # change column names
    names(datum.SH) <- c("Time", "Date.Central", "Date", "Battery.Volts", "Temp", "DO", "Sat", "Q")
    
    
    str(datum.SH) #Check formats of variables
    
    
    #Reformat epoch time stamp as time of day as posix format.
    datum.SH$Date <- as.POSIXct(datum.SH$Date)
    
    #Format Month and day column using the posix date in the date column
    datum.SH$Month.day <- format(as.POSIXct(datum.SH$Date), format = "%B %d")
    
    #Format TOD using the posix date in the date column
    datum.SH$TOD <- format(as.POSIXct(datum.SH$Date), format = "%H:%M:%S")
    
    #Format TOD using the posix date in the date column
    datum.SH$Month <- format(as.POSIXct(datum.SH$Date), format = "%B")
    
    #Format TOD using the posix date in the date column
    datum.SH$Day <- format(as.POSIXct(datum.SH$Date), format = "%d")
    
    #Relevel month variable to be in chrono order
    datum.SH$Month <- factor(datum.SH$Month, levels = c("June", "July", "August", "September")) 
    
    #Round DO and Sat data
    datum.SH$DO <- round(datum.SH$DO, 3)
    datum.SH$Sat <- round(datum.SH$Sat, 3)
  
    
#BOB
    #Read in data
    datum.BOB <- read.table(file="BOB_pool2_28JUN-27SEP_Full.txt", header = F, strip.white = T, skip =9, sep=",")
    
    # change column names
    names(datum.BOB) <- c("Time", "Date.Central", "Date", "Battery.Volts", "Temp", "DO", "Sat", "Q")
    
    
    str(datum.BOB) #Check formats of variables
    
    
    #Reformat epoch time stamp as posix format
    datum.BOB$Date <- as.POSIXct(datum.BOB$Date)
    
    #Format Month and day column using the posix date in the date column
    datum.BOB$Month.day <- format(as.POSIXct(datum.BOB$Date), format = "%B %d")
    
    #Format TOD using the posix date in the date column
    datum.BOB$TOD <- format(as.POSIXct(datum.BOB$Date), format = "%H:%M:%S")
    
    #Format TOD using the posix date in the date column
    datum.BOB$Month <- format(as.POSIXct(datum.BOB$Date), format = "%B")
    
    #Format TOD using the posix date in the date column
    datum.BOB$Day <- format(as.POSIXct(datum.BOB$Date), format = "%d")
    
    #Relevel month variable to be in chrono order
    datum.BOB$Month <- factor(datum.BOB$Month, levels = c("June", "July", "August", "September"))
    
    #Round DO and Sat data
    datum.BOB$DO <- round(datum.BOB$DO, 3)
    datum.BOB$Sat <- round(datum.BOB$Sat, 3)
    
       
####  create time series plots 
  
#### SH ####
    
    
      SH.all.data <- datum.SH |>
        filter(Month != "June") |>
        ggplot( aes(x=Date, y=DO)) +
        geom_line()+
        geom_line(aes(x=Date, y=Temp), col="blue")+
      geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
      geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
      geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
      geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
      annotate("text", x = min(datum.BOB$Date), y = 8.5, label =  "7.4 mg/L", size = 5, color ="darkgreen")+
      annotate("text", x = min(datum.BOB$Date), y = 2.9, label =  "2 mg/L", size = 5, color ="orange")+
      annotate("text", x = min(datum.BOB$Date), y = 0.8, label =  "0 mg/L", size = 5, color ="red")+
      annotate("text", x = max(datum.BOB$Date)+86400, y = 20.8, label =  "20 °C", size = 5, color ="blue")+
        scale_x_datetime(name = "Date", date_labels = "%b %d", date_breaks = "1 week")+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
                           )+
        ggtitle(label="SH")+
        theme(axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5))
      SH.all.data
      
      
      #Export plot as tiff
      tiff(filename = "All data with DO and Temp SH.tiff", width = 16, height = 10, units = "in", res = 300, compression = "lzw")
      SH.all.data
      dev.off()
      
      #Faceted by month
          #With just DO
          SH.month.data.DO <- datum.SH |>
            filter(Month != "June") |>
            ggplot(aes(x=Date, y=DO)) +
            geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, Month != "June"), 
                      ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
            geom_line()+
            geom_line(aes(x=Date, y=Temp), col="blue")+
            geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
            geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
            geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
            geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
            facet_wrap(~Month, nrow=3, ncol=1, scales = "free_x")+
            scale_x_datetime(name = "Day of Month", date_labels = "%d", date_breaks = "2 day", expand = c(0.02, 0.02))+
            scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 4), 
                               sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =4))
            )+
            ggtitle(label="SH")+
            theme(axis.text.y.right = element_text(color="blue", size =16), axis.title.y.right = element_text(color="blue", size = 22), 
                  axis.text.y.left = element_text(size = 16), axis.title.y.left = element_text(size = 22), 
                  axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
                  panel.grid.major.y = element_line(color="grey95"), 
                  panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5),
                  strip.text = element_text(size = 20),
                  plot.title = element_text(size = 24))
          
          SH.month.data.DO
          
          #Export plot as tiff
          tiff(filename = "DO data facet by month SH.tiff", width = 10, height = 12, units = "in", res = 300, compression = "lzw")
          SH.month.data.DO
          dev.off()
          
          
      #Zoomed into two weeks to show pattern
          #Instructions: 
          #For the Time filter line:
          #  1) choose start time number that correspond to desired start POSIX dates in date column
          #  2) choose the ending time number that corresponds to the desired end date. 
          #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day.
          #For the geom_rect subset line:
          #   1) set the filter to the days you want. Refer to the data frame to see which day of year (DOY) corresponds to the dates being plotted
          SH.zoom.2week <- datum.SH |>
            filter(between(Time, 1659337740, 1660546140)) |>
            ggplot(aes(x=Date, y=DO)) +
            geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, DOY %in% 213:227), 
                     ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
            geom_line() +
            geom_line(aes(x=Date, y=Temp), col="blue")+
            scale_x_datetime(name = "", date_labels = "%b %d", date_breaks = "1 day", expand = c(0.02, 0.02))+
            scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                               sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
            )+
            geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
            geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
            geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
            geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
            theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
                  axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
                  panel.grid.major.y = element_line(color="grey95"), 
                  panel.grid.major.x = element_line(colour="grey95", linewidth=0.5))+
            ggtitle(label="SH - August 1 - August 14")
          SH.zoom.2week
          
          #Export plot as tiff
          tiff(filename = "DO data zoomed 2 weeks SH.tiff", width = 11, height = 7, units = "in", res = 300, compression = "lzw")
          SH.zoom.2week
          dev.off()
          
      #Zoomed into just two days to show pattern
          #Instructions: 
          #For the Time filter line:
          #  1) choose start time number that correspond to desired start POSIX dates in date column
          #  2) choose the ending time number that corresponds to the desired end date. 
          #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day.
          #For the geom_rect subset line:
          #   1) set the filter to the days you want. Refer to the data frame to see which day of year (DOY) corresponds to the dates being plotted
          SH.zoom.2day <- datum.SH |>
            filter(between(Time, 1659510540, 1659682740)) |>
            ggplot(aes(x=Date, y=DO)) +
            geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, DOY %in% 215:216), 
                      ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
            geom_line() +
            geom_line(aes(x=Date, y=Temp), col="blue")+
            scale_x_datetime(name = "", date_labels = "%b %d %H:%M:%S", date_breaks = "4 hour", expand = c(0.02, 0.02))+
            scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 4), 
                               sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =4))
            )+
            geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
            geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
            geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
            geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
            theme(axis.text.y.right = element_text(color="blue", size =16), axis.title.y.right = element_text(color="blue", size = 22), 
                  axis.text.y.left = element_text(size = 16), axis.title.y.left = element_text(size = 22), 
                  axis.title.x = element_text(size = 20), axis.text.x = element_text(angle=45, hjust=1, vjust=1,size = 16),
                  panel.grid.major.y = element_line(color="grey95"), 
                  panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5),
                  strip.text = element_text(size = 20),
                  plot.title = element_text(size = 24))+
            ggtitle(label="SH - August 3-4")
          SH.zoom.2day
          
          
          #Export plot as tiff
          tiff(filename = "DO data zoomed 2 day SH.tiff", width = 11, height = 7, units = "in", res = 300, compression = "lzw")
          SH.zoom.2day
          dev.off()
          
          
        #Combine facet and two week and two day zooms
          tiff(filename = "DO data with facet two weeks and two days SH.tiff", width = 18, height = 12, units = "in", res = 300, compression = "lzw")
          SH.month.data.DO + (SH.zoom.2week /SH.zoom.2day)+
            plot_annotation(tag_levels = 'A')
          dev.off()
      
  ###Animated graph of oxygen data
      
    ##Two week animation 
      
      #Select window with second filter 
          #Instructions: 
          #  1) choose start time number that correspond to desired start POSIX dates in date column
          #  2) choose the ending time number that corresponds to the desired end date. 
          #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day.SH.zoom.animation <- datum.SH |>
      SH.zoom.animation.2week <- datum.SH |>
        filter(between(Time, 1659337740, 1660546140)) |>
        ggplot(aes(x=Date, y=DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set, label = NULL), data = subset(datum.ss, DOY %in% 213:227), 
                 ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line() +
        geom_line(aes(x=Date, y=Temp), col="blue")+
        geom_point() +
        geom_point(aes(x=Date, y=Temp), col="blue") +
        scale_x_datetime(name = "", date_labels = "%b %d", date_breaks = "1 day", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
        )+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        annotate("text", x = min(datum.SH$Date)+3980000, y = 8.5, label =  "7.4 mg/L", size = 4, color ="darkgreen")+
        annotate("text", x = min(datum.SH$Date)+3980000, y = 2.9, label =  "2 mg/L", size = 4, color ="orange")+
        annotate("text", x = min(datum.SH$Date)+3980000, y = 0.8, label =  "0 mg/L", size = 4, color ="red")+
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
              axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.major.x = element_line(colour="grey95", linewidth=0.5))+
        ggtitle(label="SH - Oxygen")+
        transition_reveal(Date)
      
      
      anim <- animate(SH.zoom.animation.2week, nframes = 200, fps = 10, height = 6.5, width = 11, end_pause = 40, units = "in", res = 300, compression = "lzw")
      
      anim_save("DO Animation 2 weeks in August for SH.gif", anim)
      
      
  ##Two day animation 
      
      #Select window with filter 
      #Instructions: 
      #  1) choose start time number that correspond to desired start POSIX dates in date column
      #  2) choose the ending time number that corresponds to the desired end date. 
      #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day. 
      SH.zoom.animation.2day <- datum.SH |>
        filter(between(Time, 1659510540, 1659682740)) |>
        ggplot(aes(x=Date, y=DO, label = DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set, label=NULL), data = subset(datum.ss, DOY %in% 215:216), 
                  ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line() +
        geom_line(aes(x=Date, y=Temp), col="blue")+
        geom_point() +
        geom_point(aes(x=Date, y=Temp), col="blue") +
        geom_text(hjust=-0.25, vjust=0)+
        scale_x_datetime(name = "", date_labels = "%b %d %H:%M:%S", date_breaks = "2 hour", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
        )+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        annotate("text", x = min(datum.SH$Date)+2910000, y = 8.5, label =  "7.4 mg/L", size = 4, color ="darkgreen")+
        annotate("text", x = min(datum.SH$Date)+2910000, y = 2.9, label =  "2 mg/L", size = 4, color ="orange")+
        annotate("text", x = min(datum.SH$Date)+2910000, y = 0.8, label =  "0 mg/L", size = 4, color ="red")+
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
              axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"),
              panel.grid.minor.x = element_line(color = "grey95"))+
        ggtitle(label="SH - August 3-4")+
        transition_reveal(Date, keep_last = TRUE)
      
      
      anim <- animate(SH.zoom.animation.2day, nframes = 200, fps = 10, height = 6.5, width = 11, end_pause = 40, units = "in", res = 300, compression = "lzw")
      
      anim_save("DO Animation 2 days in August for SH.gif", anim)
      
      
    ##Saturation facet by month
      SH.month.data.sat <- datum.SH |>
        filter(Month != "June") |>
        ggplot(aes(x=Date, y=Sat)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, Month != "June"), 
                  ymin = 0, ymax = 500, fill = "yellow", alpha=0.15)+
        geom_line()+
        facet_wrap(~Month, nrow=3, ncol=1, scales = "free_x")+
        scale_x_datetime(name = "Day of Month", date_labels = "%d", date_breaks = "1 day", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "% Oxygen saturation", breaks = seq(0,500, by = 50))+
        ggtitle(label="SH")+
        theme(panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5))
      
      SH.month.data.sat
      
      #Export plot as tiff
      tiff(filename = "Saturation data facet by month SH.tiff", width = 10, height = 12, units = "in", res = 300, compression = "lzw")
      SH.month.data.sat
      dev.off()
      
      
#####BOB####
      
      #All data
      BOB.all.data <- datum.BOB |>
        filter(Month != "June") |>
        ggplot( aes(x=Date, y=DO)) +
        geom_line()+
        geom_line(aes(x=Date, y=Temp), col="blue")+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
        annotate("text", x = min(datum.BOB$Date), y = 8.5, label =  "7.4 mg/L", size = 5, color ="darkgreen")+
        annotate("text", x = min(datum.BOB$Date), y = 2.9, label =  "2 mg/L", size = 5, color ="orange")+
        annotate("text", x = min(datum.BOB$Date), y = 0.8, label =  "0 mg/L", size = 5, color ="red")+
        annotate("text", x = max(datum.BOB$Date)+86400, y = 20.8, label =  "20 °C", size = 5, color ="blue")+
        scale_x_datetime(name = "Date", date_labels = "%b %d", date_breaks = "1 week")+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
        )+
        ggtitle(label="BOB")+
        theme(axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5))
      BOB.all.data
      
      
      #Export plot as tiff
      tiff(filename = "All data with DO and Temp BOB.tiff", width = 16, height = 10, units = "in", res = 300, compression = "lzw")
      BOB.all.data
      dev.off()
      
      #Faceted by month
      #With just DO
      BOB.month.data.DO <- datum.BOB |>
        filter(Month != "June") |>
        ggplot(aes(x=Date, y=DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, Month != "June"), 
                  ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line()+
        geom_line(aes(x=Date, y=Temp), col="blue")+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
        facet_wrap(~Month, nrow=3, ncol=1, scales = "free_x")+
        scale_x_datetime(name = "Day of Month", date_labels = "%d", date_breaks = "2 day", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 4), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =4))
        )+
        ggtitle(label="BOB")+
        theme(axis.text.y.right = element_text(color="blue", size =16), axis.title.y.right = element_text(color="blue", size = 22), 
              axis.text.y.left = element_text(size = 16), axis.title.y.left = element_text(size = 22), 
              axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16),
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5),
              strip.text = element_text(size = 20),
              plot.title = element_text(size = 24))
      
      BOB.month.data.DO
      
      #Export plot as tiff
      tiff(filename = "DO data facet by month BOB.tiff", width = 10, height = 12, units = "in", res = 300, compression = "lzw")
      BOB.month.data.DO
      dev.off()
      
      
      #Zoomed into two weeks to show pattern
      #Instructions: 
      #For the Time filter line:
      #  1) choose start time number that correspond to desired start POSIX dates in date column
      #  2) choose the ending time number that corresponds to the desired end date. 
      #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day.
      #For the geom_rect subset line:
      #   1) set the filter to the days you want. Refer to the data frame to see which day of year (DOY) corresponds to the dates being plotted
      BOB.zoom.2week <- datum.BOB |>
        filter(between(Time, 1659337500, 1660546500)) |>
        ggplot(aes(x=Date, y=DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, DOY %in% 213:227), 
                  ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line() +
        geom_line(aes(x=Date, y=Temp), col="blue")+
        scale_x_datetime(name = "", date_labels = "%b %d", date_breaks = "1 day", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
        )+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
              axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.major.x = element_line(colour="grey95", linewidth=0.5))+
        ggtitle(label="BOB - August 1 - August 14")
      BOB.zoom.2week
      
      #Export plot as tiff
      tiff(filename = "DO data zoomed 2 weeks BOB.tiff", width = 11, height = 7, units = "in", res = 300, compression = "lzw")
      BOB.zoom.2week
      dev.off()
      
      #Zoomed into just two days to show pattern
      #Instructions: 
      #For the Time filter line:
      #  1) choose start time number that correspond to desired start POSIX dates in date column
      #  2) choose the ending time number that corresponds to the desired end date. 
      #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day.
      #For the geom_rect subset line:
      #   1) set the filter to the days you want. Refer to the data frame to see which day of year (DOY) corresponds to the dates being plotted
      BOB.zoom.2day <- datum.BOB |>
        filter(between(Time, 1659510300, 1659682500)) |>
        ggplot(aes(x=Date, y=DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, DOY %in% 215:216), 
                  ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line() +
        geom_line(aes(x=Date, y=Temp), col="blue")+
        scale_x_datetime(name = "", date_labels = "%b %d %H:%M:%S", date_breaks = "4 hour", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 4), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =4))
        )+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        geom_hline(yintercept=20, lty=2, linewidth =1, color="blue")+
        theme(axis.text.y.right = element_text(color="blue", size =16), axis.title.y.right = element_text(color="blue", size = 22), 
              axis.text.y.left = element_text(size = 16), axis.title.y.left = element_text(size = 22), 
              axis.title.x = element_text(size = 20), axis.text.x = element_text(angle=45, hjust=1, vjust=1,size = 16),
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5),
              strip.text = element_text(size = 20),
              plot.title = element_text(size = 24))+
        ggtitle(label="BOB - August 3-4")
      BOB.zoom.2day
      
      #Export plot as tiff
      tiff(filename = "DO data zoomed 2 day BOB.tiff", width = 11, height = 7, units = "in", res = 300, compression = "lzw")
      BOB.zoom.2day
      dev.off()
      
      
      #Combine facet and two week and two day zooms
          tiff(filename = "DO data with facet two weeks and two days BOB.tiff", width = 18, height = 12, units = "in", res = 300, compression = "lzw")
          BOB.month.data.DO + (BOB.zoom.2week /BOB.zoom.2day)+
            plot_annotation(tag_levels = 'A')
          dev.off()
      
      ###Animated graph of oxygen data
      
      ##Two week animation 
      
      #Select window with second filter 
      #Instructions: 
      #  1) choose start time number that correspond to desired start POSIX dates in date column
      #  2) choose the ending time number that corresponds to the desired end date. 
      #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day.SH.zoom.animation <- datum.SH |>
      BOB.zoom.animation.2week <- datum.BOB |>
        filter(between(Time, 1659337500, 1660546500)) |>
        ggplot(aes(x=Date, y=DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, DOY %in% 213:227), 
                  ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line() +
        geom_line(aes(x=Date, y=Temp), col="blue")+
        geom_point() +
        geom_point(aes(x=Date, y=Temp), col="blue") +
        scale_x_datetime(name = "", date_labels = "%b %d", date_breaks = "1 day", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
        )+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        annotate("text", x = min(datum.BOB$Date)+4149420, y = 8.5, label =  "7.4 mg/L", size = 4, color ="darkgreen")+
        annotate("text", x = min(datum.BOB$Date)+4149420, y = 2.9, label =  "2 mg/L", size = 4, color ="orange")+
        annotate("text", x = min(datum.BOB$Date)+4149420, y = 0.8, label =  "0 mg/L", size = 4, color ="red")+
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
              axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"), 
              panel.grid.major.x = element_line(colour="grey95", linewidth=0.5))+
        ggtitle(label="BOB - Oxygen")+
        transition_reveal(Date)
      
      
      anim <- animate(BOB.zoom.animation.2week, nframes = 200, fps = 10, height = 6.5, width = 11, end_pause = 40, units = "in", res = 300, compression = "lzw")
      
      anim_save("DO Animation 2 weeks in August for BOB.gif", anim)
      
      
      ##Two day animation 
      
      #Select window with filter 
      #Instructions: 
      #  1) choose start time number that correspond to desired start POSIX dates in date column
      #  2) choose the ending time number that corresponds to the desired end date. 
      #  3) for best plotting, recommend start time as first row for start day and end time as last row for end day. 
      BOB.zoom.animation.2day <- datum.BOB |>
        filter(between(Time, 1659510300, 1659682500)) |>
        ggplot(aes(x=Date, y=DO, label=DO)) +
        geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set, label=NULL), data = subset(datum.ss, DOY %in% 215:216), 
                  ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
        geom_line() +
        geom_line(aes(x=Date, y=Temp), col="blue")+
        geom_point() +
        geom_point(aes(x=Date, y=Temp), col="blue") +
        geom_text(hjust=-0.25, vjust=0)+
        scale_x_datetime(name = "", date_labels = "%b %d %H:%M:%S", date_breaks = "2 hour", expand = c(0.02, 0.02))+
        scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                           sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2))
        )+
        geom_hline(yintercept=7.4, lty=2, linewidth =1, color="darkgreen")+
        geom_hline(yintercept=2, lty=2, linewidth =1, color="orange")+
        geom_hline(yintercept=0, lty=2, linewidth =1, color="red")+
        annotate("text", x = min(datum.BOB$Date)+3087220, y = 8.5, label =  "7.4 mg/L", size = 4, color ="darkgreen")+
        annotate("text", x = min(datum.BOB$Date)+3087220, y = 3.1, label =  "2 mg/L", size = 4, color ="orange")+
        annotate("text", x = min(datum.BOB$Date)+3087220, y = 0.8, label =  "0 mg/L", size = 4, color ="red")+
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
              axis.text.y.right = element_text(color="blue"), axis.title.y.right = element_text(color="blue"), 
              panel.grid.major.y = element_line(color="grey95"),
              panel.grid.minor.x = element_line(color = "grey95"))+
        ggtitle(label="BOB - August 3-4")+
        transition_reveal(Date, keep_last = TRUE)
      
      
      anim <- animate(BOB.zoom.animation.2day, nframes = 200, fps = 10, height = 6.5, width = 11, end_pause = 40, units = "in", res = 300, compression = "lzw")
      
      anim_save("DO Animation 2 days in August for BOB.gif", anim)
      

      ##Saturation facet by month
          BOB.month.data.sat <- datum.BOB |>
            filter(Month != "June") |>
            ggplot(aes(x=Date, y=Sat)) +
            geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, Month != "June"), 
                      ymin = 0, ymax = 400, fill = "yellow", alpha=0.15)+
            geom_line()+
            facet_wrap(~Month, nrow=3, ncol=1, scales = "free_x")+
            scale_x_datetime(name = "Day of Month", date_labels = "%d", date_breaks = "1 day", expand = c(0.02, 0.02))+
            scale_y_continuous(name = "% Oxygen saturation", breaks = seq(0,400, by = 25))+
            ggtitle(label="BOB")+
            theme(panel.grid.major.y = element_line(color="grey95"), 
                  panel.grid.minor.x = element_line(colour="grey95", linewidth=0.5))
          
          BOB.month.data.sat
          
          #Export plot as tiff
          tiff(filename = "Saturation data facet by month BOB.tiff", width = 10, height = 12, units = "in", res = 300, compression = "lzw")
          BOB.month.data.sat
          dev.off()
          
          
      ##   
      ## Make combined figure 1 for manuscript with BOB and SH data
      ##
          tiff(filename = "Figure 1 data by month and day BOTH sites.tiff", width = 18, height = 20, units = "in", res = 300, compression = "lzw")
          (SH.month.data.DO + BOB.month.data.DO) / (SH.zoom.2day + BOB.zoom.2day) +
           plot_layout(heights = c(2, 1)) + plot_annotation(tag_levels = 'A') &
          theme(plot.tag = element_text(size = 24))
          dev.off()
          
          png(filename = "Figure 1 data by month and day BOTH sites.png", width = 18, height = 20, units = "in", res = 300)
          (SH.month.data.DO + BOB.month.data.DO) / (SH.zoom.2day + BOB.zoom.2day) +
            plot_layout(heights = c(2, 1)) + plot_annotation(tag_levels = 'A') &
            theme(plot.tag = element_text(size = 24))
          dev.off()
          
          
      
#### Summarize time spent below hypoxia thresholds or in anoxia ####
   
  ####SH   
      
      #Pop.sex summary
      datum.SH.dailies <- datum.SH |>
        filter(Month != "June") |>
        group_by(Month.day, Month) |> 
        summarise(mean_DO = mean(DO) |> round(3),
                  min_DO = min(DO) |> round(3),
                  max_DO = max(DO) |> round(3),
                  readings_below_2 = length(which(DO < 2)),
                  readings_below_0.5 = length(which(DO < 0.5)),
                  readings_below_0.1 = length(which(DO < 0.1))
        )
      
      #Convert the number of 10 minute observations into time spent below 2 mg/L DO in minutes
      datum.SH.dailies$minutes_below_2 = datum.SH.dailies$readings_below_2*10  
      #Convert the number of 10 minute observations into time spent below 0.5 mg/L DO in minutes
      datum.SH.dailies$minutes_below_0.5 = datum.SH.dailies$readings_below_0.5*10
      #Convert the number of 10 minute observations into time spent below 0.1 mg/L DO in minutes
      datum.SH.dailies$minutes_below_0.1 = datum.SH.dailies$readings_below_0.1*10
      
      
      
      #How many days did DO go below 2?
        days.below.2.SH <- length(which(datum.SH.dailies$readings_below_2 != 0))
        days.below.2.SH
      #How long on average did DO stay below 2 on days where it got that low?
        avg.below.2.SH <- mean(datum.SH.dailies$minutes_below_2[datum.SH.dailies$minutes_below_2>0])
        avg.below.2.SH <- round(avg.below.2.SH, digits=0)
        avg.below.2.SH
      #What was the range of times spent at 2 across all the days that got that low? 
        range.below.2.SH <- paste(range(datum.SH.dailies$minutes_below_2[datum.SH.dailies$minutes_below_2>0]) |> round(3), collapse = '-')
        range.below.2.SH
        
        
        #How many days did DO go below 0.5?
        days.below.0.5.SH <- length(which(datum.SH.dailies$readings_below_0.5 != 0))
        days.below.0.5.SH
        #How long on average did DO stay below 0.5 on days where it got that low?
        avg.below.0.5.SH <- mean(datum.SH.dailies$minutes_below_0.5[datum.SH.dailies$minutes_below_0.5>0])
        avg.below.0.5.SH <- round(avg.below.0.5.SH, digits=0)
        avg.below.0.5.SH
        #What was the range of times spent at 0.5 across all the days that got that low? 
        range.below.0.5.SH <- paste(range(datum.SH.dailies$minutes_below_0.5[datum.SH.dailies$minutes_below_0.5>0]) |> round(3), collapse = '-')
        range.below.0.5.SH
        
        
        #How many days did DO go below 0.1?
        days.below.0.1.SH <- length(which(datum.SH.dailies$readings_below_0.1 != 0))
        days.below.0.1.SH
        #How long on average did DO stay below 2 on days where it got that low?
        avg.below.0.1.SH <- mean(datum.SH.dailies$minutes_below_0.1[datum.SH.dailies$minutes_below_0.1>0])
        avg.below.0.1.SH <- round(avg.below.0.1.SH, digits=0)
        avg.below.0.1.SH
        #What was the range of times spent at 2 across all the days that got that low? 
        range.below.0.1.SH <- paste(range(datum.SH.dailies$minutes_below_0.1[datum.SH.dailies$minutes_below_0.1>0]) |> round(3), collapse = '-')
        range.below.0.1.SH
        
        
      #Make histogram showing distribution of minimum oxygen values across all the days that summer
        min.histo.SH <- datum.SH.dailies |>
        ggplot(aes(x=min_DO))+ 
        geom_histogram(color="black", fill="deepskyblue", binwidth=0.5)+
          scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,30, by = 2))+
          scale_x_continuous(name = "Minimum DO value measured each day (mg/L)", breaks = seq(0,10, by = 0.5))+
          ggtitle(label = "SH")+
          annotate("text", x = 4.5, y = 22, 
                   label =  paste("Number of days < 2 mg/L DO =", days.below.2.SH, "days"), size = 5, color ="black")+
          annotate("text", x = 4.5, y = 20, 
                   label =  paste("Number of days < 0.5 mg/L DO =", days.below.0.5.SH, "days"), size = 5, color ="black")+
          annotate("text", x = 4.5, y = 18, 
                   label =  paste("Number of days < 0.1 mg/L DO =", days.below.0.1.SH, "days"), size = 5, color ="black")+
          theme(plot.title = element_text(size = 20),
                axis.text = element_text(size = 16), axis.title = element_text(size = 18))
        min.histo.SH
    
    #Make histogram showing distribution of time spent below 2 mg/L DO across all the days that summer
      min.time.histo.2.SH <- datum.SH.dailies |>
        ggplot(aes(x=minutes_below_2))+ 
        geom_histogram(color="black", fill="cornflowerblue", binwidth=60)+
        scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,30, by = 2))+
        scale_x_continuous(name = "Min. per day below 2 mg/L DO", breaks = seq(0,1000, by = 60))+
        annotate("text", x = 500, y = 26, 
                 label =  paste("Avg. time < 2 mg/L DO =", avg.below.2.SH, "min."), size = 4, color ="black")+
        annotate("text", x = 500, y = 24, 
                 label =  paste("Range < 2 mg/L DO =", range.below.2.SH, "min."), size = 4, color ="black")+
        theme(axis.text.x = element_text(size=13, angle = 45, vjust=1, hjust=1), axis.title = element_text(size=18))
      min.time.histo.2.SH
      
    #Make histogram showing distribution of time spent below 0.5 mg/L DO across all the days that summer
      min.time.histo.0.5.SH <- datum.SH.dailies |>
        ggplot(aes(x=minutes_below_0.5))+ 
        geom_histogram(color="black", fill="cornflowerblue", binwidth=60)+
        scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,100, by = 5))+
        scale_x_continuous(name = "Min. per day below 0.5 mg/L DO", breaks = seq(0,1000, by = 60))+
        annotate("text", x = 240, y = 65, 
                 label =  paste("Avg. time < 0.5 mg/L DO =", avg.below.0.5.SH, "min."), size = 4, color ="black")+
        annotate("text", x = 240, y = 60, 
                 label =  paste("Range < 0.5 mg/L DO =", range.below.0.5.SH, "min."), size = 4, color ="black")+
        theme(axis.text.x = element_text(size=13.5), axis.title = element_text(size=18))
      min.time.histo.0.5.SH
      
    #Make histogram showing distribution of time spent below 0.1 mg/L DO across all the days that summer
      min.time.histo.0.1.SH <- datum.SH.dailies |>
        ggplot(aes(x=minutes_below_0.1))+ 
        geom_histogram(color="black", fill="cornflowerblue", binwidth=60)+
        scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,100, by = 5))+
        scale_x_continuous(name = "Min. per day below 0.1 mg/L DO", breaks = seq(0,1000, by = 60))+
        annotate("text", x = 210, y = 70, 
                 label =  paste("Avg. time < 0.1 mg/L DO =", avg.below.0.1.SH, "min."), size = 4, color ="black")+
        annotate("text", x = 210, y = 65, 
                 label =  paste("Range < 0.1 mg/L DO =", range.below.0.1.SH, "min."), size = 4, color ="black")+
        theme(axis.text.x = element_text(size=14), axis.title = element_text(size=18))
      min.time.histo.0.1.SH

      
      #Create and export figure
      tiff(filename = "Histograms of DO data SH.tiff", width = 13, height = 9, units = "in", res = 300, compression = "lzw")
            min.histo.SH / (min.time.histo.2.SH + min.time.histo.0.5.SH + min.time.histo.0.1.SH) +
              plot_annotation(tag_levels = 'A')
            dev.off()
            
            
            
            
   ####BOB
            
        #Pop.sex summary
            datum.BOB.dailies <- datum.BOB |>
              filter(Month != "June") |>
              group_by(Month.day, Month) |> 
              summarise(mean_DO = mean(DO) |> round(3),
                        min_DO = min(DO) |> round(3),
                        max_DO = max(DO) |> round(3),
                        readings_below_2 = length(which(DO < 2)),
                        readings_below_0.5 = length(which(DO < 0.5)),
                        readings_below_0.1 = length(which(DO < 0.1))
              )
            
            #Convert the number of 10 minute observations into time spent below 2 mg/L DO in minutes
            datum.BOB.dailies$minutes_below_2 = datum.BOB.dailies$readings_below_2*10  
            #Convert the number of 10 minute observations into time spent below 0.5 mg/L DO in minutes
            datum.BOB.dailies$minutes_below_0.5 = datum.BOB.dailies$readings_below_0.5*10
            #Convert the number of 10 minute observations into time spent below 0.1 mg/L DO in minutes
            datum.BOB.dailies$minutes_below_0.1 = datum.BOB.dailies$readings_below_0.1*10
            
            
            
            #How many days did DO go below 2?
            days.below.2.BOB <- length(which(datum.BOB.dailies$readings_below_2 != 0))
            days.below.2.BOB
            #How long on average did DO stay below 2 on days where it got that low?
            avg.below.2.BOB <- mean(datum.BOB.dailies$minutes_below_2[datum.BOB.dailies$minutes_below_2>0])
            avg.below.2.BOB <- round(avg.below.2.BOB, digits=0)
            avg.below.2.BOB
            #What was the range of times spent at 2 across all the days that got that low? 
            range.below.2.BOB <- paste(range(datum.BOB.dailies$minutes_below_2[datum.BOB.dailies$minutes_below_2>0]) |> round(3), collapse = '-')
            range.below.2.BOB
            
            
            #How many days did DO go below 0.5?
            days.below.0.5.BOB <- length(which(datum.BOB.dailies$readings_below_0.5 != 0))
            days.below.0.5.BOB
            #How long on average did DO stay below 0.5 on days where it got that low?
            avg.below.0.5.BOB <- mean(datum.BOB.dailies$minutes_below_0.5[datum.BOB.dailies$minutes_below_0.5>0])
            avg.below.0.5.BOB <- round(avg.below.0.5.BOB, digits=0)
            avg.below.0.5.BOB
            #What was the range of times spent at 0.5 across all the days that got that low? 
            range.below.0.5.BOB <- paste(range(datum.BOB.dailies$minutes_below_0.5[datum.BOB.dailies$minutes_below_0.5>0]) |> round(3), collapse = '-')
            range.below.0.5.BOB
            
            
            #How many days did DO go below 0.1?
            days.below.0.1.BOB <- length(which(datum.BOB.dailies$readings_below_0.1 != 0))
            days.below.0.1.BOB
            #How long on average did DO stay below 2 on days where it got that low?
            avg.below.0.1.BOB <- mean(datum.BOB.dailies$minutes_below_0.1[datum.BOB.dailies$minutes_below_0.1>0])
            avg.below.0.1.BOB <- round(avg.below.0.1.BOB, digits=0)
            avg.below.0.1.BOB
            #What was the range of times spent at 2 across all the days that got that low? 
            range.below.0.1.BOB <- paste(range(datum.BOB.dailies$minutes_below_0.1[datum.BOB.dailies$minutes_below_0.1>0]) |> round(3), collapse = '-')
            range.below.0.1.BOB
            
            
          #Make histogram showing distribution of minimum oxygen values across all the days that summer
            min.histo.BOB <- datum.BOB.dailies |>
              ggplot(aes(x=min_DO))+ 
              geom_histogram(color="black", fill="deepskyblue", binwidth=0.5)+
              scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,30, by = 2))+
              scale_x_continuous(name = "Minimum DO value measured each day (mg/L)", breaks = seq(0,10, by = 0.5))+
              ggtitle(label="BOB")+
              annotate("text", x = 4.5, y = 20, 
                       label =  paste("Number of days below 2 mg/L DO =", days.below.2.BOB, "days"), size = 5, color ="black")+
              annotate("text", x = 4.5, y = 18, 
                       label =  paste("Number of days below 0.5 mg/L DO =", days.below.0.5.BOB, "days"), size = 5, color ="black")+
              annotate("text", x = 4.5, y = 16, 
                       label =  paste("Number of days below 0.1 mg/L DO =", days.below.0.1.BOB, "days"), size = 5, color ="black")+
              theme(plot.title = element_text(size = 20),
                    axis.text = element_text(size = 16), axis.title = element_text(size = 18))
            min.histo.BOB
            
          #Make histogram showing distribution of time spent below 2 mg/L DO across all the days that summer
            min.time.histo.2.BOB <- datum.BOB.dailies |>
              ggplot(aes(x=minutes_below_2))+ 
              geom_histogram(color="black", fill="cornflowerblue", binwidth=60)+
              scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,60, by = 4))+
              scale_x_continuous(name = "Min. per day below 2 mg/L DO", breaks = seq(0,1000, by = 60))+
              annotate("text", x = 500, y = 49, 
                       label =  paste("Avg. time < 2 mg/L DO =", avg.below.2.BOB, "min."), size = 4, color ="black")+
              annotate("text", x = 500, y = 45, 
                       label =  paste("Range of time < 2 mg/L DO =", range.below.2.BOB, "min."), size = 4, color ="black")+
              theme(axis.text.x = element_text(size=13, angle = 45, vjust=1, hjust=1), axis.title = element_text(size=18))
            min.time.histo.2.BOB
            
          #Make histogram showing distribution of time spent below 0.5 mg/L DO across all the days that summer
            min.time.histo.0.5.BOB <- datum.BOB.dailies |>
              ggplot(aes(x=minutes_below_0.5))+ 
              geom_histogram(color="black", fill="cornflowerblue", binwidth=60)+
              scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,100, by = 5))+
              scale_x_continuous(name = "Min. per day below 0.5 mg/L DO", breaks = seq(0,1000, by = 60))+
              annotate("text", x = 300, y = 65, 
                       label =  paste("Avg. time < 0.5 mg/L DO =", avg.below.0.5.BOB, "min."), size = 4, color ="black")+
              annotate("text", x = 330, y = 60, 
                       label =  paste("Range of time < 0.5 mg/L DO =", range.below.0.5.BOB, "min."), size = 4, color ="black")+
              theme(axis.text.x = element_text(size=13.5, angle = 45, vjust=1, hjust=1), axis.title = element_text(size=18))
            min.time.histo.0.5.BOB
            
          #Make histogram showing distribution of time spent below 0.1 mg/L DO across all the days that summer
            min.time.histo.0.1.BOB <- datum.BOB.dailies |>
              ggplot(aes(x=minutes_below_0.1))+ 
              geom_histogram(color="black", fill="cornflowerblue", binwidth=60)+
              scale_y_continuous(name = "Frequency (# of days)", breaks = seq(0,100, by = 5))+
              scale_x_continuous(name = "Min. per day below 0.1 mg/L DO", breaks = seq(0,1000, by = 60))+
              annotate("text", x = 250, y = 70, 
                       label =  paste("Avg. time < 0.1 mg/L DO =", avg.below.0.1.BOB, "min."), size = 4, color ="black")+
              annotate("text", x = 260, y = 65, 
                       label =  paste("Range of time < 0.1 mg/L DO =", range.below.0.1.BOB, "min."), size = 4, color ="black")+
              theme(axis.text.x = element_text(size=14), axis.title = element_text(size=18))
            min.time.histo.0.1.BOB
            
            
          #Create and export figure
            tiff(filename = "Histograms of DO data BOB.tiff", width = 13, height = 9, units = "in", res = 300, compression = "lzw")
            min.histo.BOB / (min.time.histo.2.BOB + min.time.histo.0.5.BOB + min.time.histo.0.1.BOB) +
              plot_annotation(tag_levels = 'A')
            dev.off()
            
            
          #Create and export figure
            tiff(filename = "Histograms of DO data BOB and SH together.tiff", width = 13.5, height = 18, units = "in", res = 300, compression = "lzw")
            min.histo.SH / (min.time.histo.2.SH + min.time.histo.0.5.SH + min.time.histo.0.1.SH) /
            min.histo.BOB / (min.time.histo.2.BOB + min.time.histo.0.5.BOB + min.time.histo.0.1.BOB) + 
              plot_layout(heights = c(4,4,0.1,4,4))+
              plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 18))
            dev.off()
      
            png(filename = "Histograms of DO data BOB and SH together.png", width = 13.5, height = 18, units = "in", res = 300)
            min.histo.SH / (min.time.histo.2.SH + min.time.histo.0.5.SH + min.time.histo.0.1.SH) /
              plot_spacer() /
              min.histo.BOB / (min.time.histo.2.BOB + min.time.histo.0.5.BOB + min.time.histo.0.1.BOB) + 
              plot_layout(heights = c(4,4,0.1,4,4))+
              plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 18))
            dev.off()
      
            
      