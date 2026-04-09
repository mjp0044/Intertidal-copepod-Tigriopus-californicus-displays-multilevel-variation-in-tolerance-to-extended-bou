
#  The resistance to extreme hypoxia by an intertidal crustacean

## Instructions for utilizing scripts and data sheets. 

Each R script will have the required .csv files listed at the start of the script when you open it up in R. The scripts and required .csv files are saved as individual files in this repository. 

**Exceptions** to the above instructions: 
1) The "Combined Resp Script.R" script imports raw respirometry files in one at a time, based on the user's choice. 
These raw respirometry scripts can be found in the zip folder called "All Individual Respirometry Raw Data Files". Simply save these files in your R working directory and you can import them into the combined resp script one at a time.

2) The DO Script.R script imports oxygen data from the two text files "SH_pool3_30JUN-27SEP_Full.txt" and "BOB_pool2_28JUN-27SEP_Full.txt". These are imported as text files and not .csv files in the DO Script.R script. 

# Highlights from the study

## Vizualizing continuous oxygen sensor data from the copepod's habitat

Problem: How do we take raw oxygen values from a sensor that sat in a tidepool for three months straight and turn those into a clear picture what the copepods experience on a daily and nightly basis?

Let's start with the data. After reading my data into R using: 

```r
datum.SH <- read.table(file="SH_pool3_30JUN-27SEP_Full.txt", header = F, strip.white = T, skip =9, sep=",")
```

We can take a look at the data after I rename the columns with friendlier labels. 

```r
 # change column names
    names(datum.SH) <- c("Time", "Date.Central", "Date", "Battery.Volts", "Temp", "DO", "Sat", "Q")
```

Below is the snapshot of the raw data file. 


```r
> head(datum.SH)
        Time        Date.Central                Date Battery.Volts   Temp       DO      Sat     Q
1 1656603540 2022-06-30 15:39:00 2022-06-30 08:39:00          3.56 15.148 4.281573 52.78721 1.010
2 1656604140 2022-06-30 15:49:00 2022-06-30 08:49:00          3.56 15.019 4.025691 49.50560 1.011
3 1656604740 2022-06-30 15:59:00 2022-06-30 08:59:00          3.56 15.019 4.220118 51.89655 1.010
4 1656605340 2022-06-30 16:09:00 2022-06-30 09:09:00          3.56 15.122 4.383858 54.02042 1.010
5 1656605940 2022-06-30 16:19:00 2022-06-30 09:19:00          3.56 15.328 4.392670 54.35014 1.010
6 1656606540 2022-06-30 16:29:00 2022-06-30 09:29:00          3.55 15.551 4.592175 57.06900 1.009
```


The first important thing we have to deal with is the formatting of the Date column. If we check the format of the columns we see it is listed as a string of characters. 

This is a problem, because if we want to use the date and time and meaningful pieces of information with which we can generate figures, we need to be able to split the information up easily.

 ```r
>     str(datum.SH) #Check formats of variables
'data.frame':	12814 obs. of  8 variables:
 $ Time         : int  1656603540 1656604140 1656604740 1656605340 1656605940 1656606540 1656607140 1656607740 1656608340 1656608940 ...
 $ Date.Central : chr  "2022-06-30 15:39:00" "2022-06-30 15:49:00" "2022-06-30 15:59:00" "2022-06-30 16:09:00" ...
 $ Date         : chr  "2022-06-30 08:39:00" "2022-06-30 08:49:00" "2022-06-30 08:59:00" "2022-06-30 09:09:00" ...
 $ Battery.Volts: num  3.56 3.56 3.56 3.56 3.56 3.55 3.56 3.56 3.56 3.55 ...
 $ Temp         : num  15.1 15 15 15.1 15.3 ...
 $ DO           : num  4.28 4.03 4.22 4.38 4.39 ...
 $ Sat          : num  52.8 49.5 51.9 54 54.4 ...
 $ Q            : num  1.01 1.01 1.01 1.01 1.01 ...
```

I could choose to utilize regex or string split functions (like those in the `stringr` package). However, base R also has some useful tools for dealing with time and date data. 

Using base R, we can convert the character string to an object of class POSIXct that represent calendar times and dates. 

```r
#Reformat epoch time stamp as time of day as posix format.
    datum.SH$Date <- as.POSIXct(datum.SH$Date)
```

From there, we can split the Date column information into individual parts. This will help us chunk or isolate pieces of data later (for example, by month or week or day). 
We will split the Date column into four new columns: Month.day (the month and day together), TOD (time of day), Month, and Day. 

```r
#Format Month and day column using the posix date in the date column
    datum.SH$Month.day <- format(as.POSIXct(datum.SH$Date), format = "%B %d")
#Format TOD using the posix date in the date column
    datum.SH$TOD <- format(as.POSIXct(datum.SH$Date), format = "%H:%M:%S")
#Format TOD using the posix date in the date column
    datum.SH$Month <- format(as.POSIXct(datum.SH$Date), format = "%B")
#Format TOD using the posix date in the date column
    datum.SH$Day <- format(as.POSIXct(datum.SH$Date), format = "%d")
```

Two last bits of housekeeping before we start plotting. First, we will also make sure we order our Month variable as a factor in chronological order. Otherwise, most R plotting packages will order the data alphabetically (i.e., in our data placing "August" before "June" on a plot).

Second, we will round the dissolved oxygen and saturation data to 3 figures to avoid overcrowding plots. 

```r
#Relevel month variable to be in chrono order
    datum.SH$Month <- factor(datum.SH$Month, levels = c("June", "July", "August", "September"))
#Round DO and Sat data
    datum.SH$DO <- round(datum.SH$DO, 3)
    datum.SH$Sat <- round(datum.SH$Sat, 3)
```

Now, we have our data prepared for plotting using `ggplot2`. The final format looks like this: 

```r
> head(datum.SH)
        Time        Date.Central                Date Battery.Volts   Temp    DO    Sat     Q Month.day      TOD Month Day
1 1656603540 2022-06-30 15:39:00 2022-06-30 08:39:00          3.56 15.148 4.282 52.787 1.010   June 30 08:39:00  June  30
2 1656604140 2022-06-30 15:49:00 2022-06-30 08:49:00          3.56 15.019 4.026 49.506 1.011   June 30 08:49:00  June  30
3 1656604740 2022-06-30 15:59:00 2022-06-30 08:59:00          3.56 15.019 4.220 51.897 1.010   June 30 08:59:00  June  30
4 1656605340 2022-06-30 16:09:00 2022-06-30 09:09:00          3.56 15.122 4.384 54.020 1.010   June 30 09:09:00  June  30
5 1656605940 2022-06-30 16:19:00 2022-06-30 09:19:00          3.56 15.328 4.393 54.350 1.010   June 30 09:19:00  June  30
6 1656606540 2022-06-30 16:29:00 2022-06-30 09:29:00          3.55 15.551 4.592 57.069 1.009   June 30 09:29:00  June  30
```

## Animated versions of figure 1 from the manuscript main text. 
### These show the rise and fall of dissolved oxygen and temperature in the SH and BOB splashpools. 
*Dissolved oxygen and temperature regimes in copepod-inhabited splash pools at Strawberry Hill (SH) and Boiler Bay (BOB), Oregon. Data collected at 10-min intervals from 01 July through 27 September 2022. Zoom-in of a 2-day and 2-week period for SH and BOB sites showing anoxia during the night. Black lines represent dissolved oxygen in mg O2 l-1 and the blue line shows the temperature in Celsius. Red dotted lines mark 0 mg O2 l-1 (anoxia), the orange dotted lines represent 2 mg O2 l-1 (severe hypoxia for most organisms), the green dotted lines represent 7.4 mg O2 l-1 (normoxic conditions at 20°C). The blue dotted lines indicate the incubator temperature of 20°C used in our laboratory assays. The yellow shading starts and ends at the sunrise and sunset times for each day.*

### 2 Days in August at SH pool
![](https://github.com/mjp0044/Intertidal-copepod-Tigriopus-californicus-displays-multilevel-variation-in-tolerance-to-extended-bou/blob/main/animated_figures/DO%20Animation%202%20days%20in%20August%20for%20SH.gif)

### 2 Weeks in August at SH pool
![](https://github.com/mjp0044/Intertidal-copepod-Tigriopus-californicus-displays-multilevel-variation-in-tolerance-to-extended-bou/blob/main/animated_figures/DO%20Animation%202%20weeks%20in%20August%20for%20SH.gif)

### 2 Days in August at BOB pool
![](https://github.com/mjp0044/Intertidal-copepod-Tigriopus-californicus-displays-multilevel-variation-in-tolerance-to-extended-bou/blob/main/animated_figures/DO%20Animation%202%20days%20in%20August%20for%20BOB.gif)

### 2 Weeks in August at BOB pool
![](https://github.com/mjp0044/Intertidal-copepod-Tigriopus-californicus-displays-multilevel-variation-in-tolerance-to-extended-bou/blob/main/animated_figures/DO%20Animation%202%20weeks%20in%20August%20for%20BOB.gif)
