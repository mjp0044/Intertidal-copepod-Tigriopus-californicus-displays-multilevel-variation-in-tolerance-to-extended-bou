
#  The resistance to extreme hypoxia by an intertidal crustacean
<img src="Figures/MEPS-2024-08-027R2 Social media image UPDATED.png" width="750">


## Instructions for utilizing scripts and data sheets. 

Each R script will have the required .csv files listed at the start of the script when you open it up in R. The scripts and required .csv files are saved as individual files in this repository. 

**Exceptions** to the above instructions: 
1) The "Combined Resp Script.R" script imports raw respirometry files in one at a time, based on the user's choice. 
These raw respirometry scripts can be found in the zip folder called "All Individual Respirometry Raw Data Files". Simply save these files in your R working directory and you can import them into the combined resp script one at a time.

2) The DO Script.R script imports oxygen data from the two text files "SH_pool3_30JUN-27SEP_Full.txt" and "BOB_pool2_28JUN-27SEP_Full.txt". These are imported as text files and not .csv files in the DO Script.R script. 

# Highlights from the study

## Vizualizing continuous oxygen sensor data from the copepod's habitat

Problem: How do we take raw oxygen values from a sensor that sat in a tidepool collecting continuous data for three months straight and turn all that dissolved oxygen data into a clear picture that shows us what copepods experience on a daily and nightly basis?

Let's start with the data. After reading the data into R using: 

```r
datum.SH <- read.table(file="SH_pool3_30JUN-27SEP_Full.txt", header = F, strip.white = T, skip =9, sep=",")
```

We can take a look at the data after we rename the columns with friendlier labels. 

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

We could choose to utilize regex or string split functions (like those in the `stringr` package). However, base R also has some useful tools for dealing with time and date data. 

Using base R, we can convert the character string to an object of class **POSIXct** that represent calendar times and dates. 

```r
#Reformat epoch time stamp as time of day as posix format.
    datum.SH$Date <- as.POSIXct(datum.SH$Date)
```

From there, we can split the Date column information into individual parts. This will help us summarize data later if we want (for example, by month or week or day). 
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

At the start of this readme, you got a preview of how we can leverage this data to zoom on any window of data we want to see what is happening in the tide pools at any day and time. Retention of the Time data will be key to let us be precise in the windows we select. 

Let's go ahead see how we acheived this by looking at a wider window of time like two weeks. 

Ideally, we'd like to visualize the flux in oxygen over time continuously. Because oxygen content in the water is also dependent on temperature, it would be ideal to also show the flux in temperature. 

Let's move to using `ggplot2` to build our graph. We will start with calling our data frame and using a filter of our Time variable to grab a specific two week period. Note that this variable is in **POSIX**, which count seconds elapsed since 1970-01-01 00:00:00 UTC. It directly corresponds to our meta data in the other columns, so we can pick out the Time values we want to grab by looking at those columns. 

Lets pick a two week window at the start of August, when the weather is typically very warm along the coast. We will look at August 1st through August 15th. 

We can query our data frame to isolate just the Time column for the first value on our start day and the last value on our end day by:

```r
> # Grab the row corresponding to the start of the first day you want so you can get the Time value only
> datum.SH[datum.SH$Month.day == "August 01", ][1, "Time"]
[1] 1659337740
> # Grab the row corresponding to the end of the last day you want so you can see the Time value
> datum.SH[datum.SH$Month.day == "August 15", ][nrow(datum.SH[datum.SH$Month.day == "August 15", ]), "Time"]
[1] 1660633140
```

Now that we have those values, we can filter the data set using: 

```r
datum.SH |>
filter(between(Time, 1659337740, 1660546140)) |>
ggplot()
```

For our ggplot code, we will specify the overall plot mapping to have Date on the x-axis and dissolved oxygen on the y-axis

We will place down two geoms, one with a line mapping to DO and a second with separate mappings to Temperature. We will make sure to give the temperature line a separate color. 


```r
datum.SH |>
filter(between(Time, 1659337740, 1660546140)) |>
ggplot(aes(x=Date, y=DO)) +
geom_line() +
geom_line(aes(x=Date, y=Temp), col="blue")
```

This gives the basic structure of our plot. We can already see the cyclical nature of the oxygen and temperature values over time.

<img src="Figures/2 week no format.png" width="400">

The plot is pretty basic and lacks a lot of valuable information that we can use to better understanding the timing of the cycles. We're also missing a y-axis that corresponds to our blue temperature line. 

Let's add those now using two helpful commands built in to ggplot2, scale_x_dateime and scale_y_continuous. 

`Scale_x_datatime` allows us to specify that the Date data is in POSIX format, and lets us manipulate the labels using the same formatting syntax as above, where %b %d specifies we want to list the dates in the format "Aug 01". We can also specify how many days we want to show on the axis using the `date_breaks` option and add some padding around the data using `expand`. 

For scale_y_continuous, we will change our label, specify our breaks for the tick marks, and add the second axis using the `sec.axis` option, which has its own set of labeling and breaks information. 

```r
datum.SH |>
filter(between(Time, 1659337740, 1660546140)) |>
ggplot(aes(x=Date, y=DO)) +
geom_line() +
geom_line(aes(x=Date, y=Temp), col="blue")+
scale_x_datetime(name = "", date_labels = "%b %d", date_breaks = "1 day", expand = c(0.02, 0.02))+
scale_y_continuous(name = "DO (mg/L)", breaks = seq(0,40, by = 2), 
                               sec.axis = sec_axis(~., name = "Temp (°C)", breaks = seq(0,40, by =2)))
```

<img src="Figures/2 week base.png" width="400">

