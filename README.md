
#  The resistance to extreme hypoxia by an intertidal crustacean
<img src="Figures/MEPS-2024-08-027R2 Social media image UPDATED.png" width="750">


## Instructions for utilizing scripts and data sheets. 

Each R script will have the required .csv files listed at the start of the script when you open it up in R. The scripts and required .csv files are saved as individual files in this repository. 

**Exceptions** to the above instructions: 
1) The "Combined Resp Script.R" script imports raw respirometry files in one at a time, based on the user's choice. 
These raw respirometry scripts can be found in the zip folder called "All Individual Respirometry Raw Data Files". Simply save these files in your R working directory and you can import them into the combined resp script one at a time.

2) The DO Script.R script imports oxygen data from the two text files "SH_pool3_30JUN-27SEP_Full.txt" and "BOB_pool2_28JUN-27SEP_Full.txt". These are imported as text files and not .csv files in the DO Script.R script. 

# Data analysis highlights! 

## A lesson in vizualizing continuous oxygen sensor data from the copepod's habitat

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

I prefer very clean and minimalistic plots, so I usually set a global base theme for all my ggplot elements using the theme from the `cowplot` package. This makes it so I don't have to specify the theme for everyplot. 

```r
#Set theme globally
theme_set(theme_cowplot())
```

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

<img src="Figures/2 week base.png" width="600">

This looks more informative.

We can also add context to the scale by labeling some important values on the plot. 

We will draw:
1. A line at 0 mg O2 per liter in red to mark where DO values reach true anoxia.
2. A line at 2 mg O2 per liter in orange to mark the widely accepted threshold of severe hypoxia for most aquatic organisms.
3. A line at 7.4 mg O2 per liter in green to mark normal oxygen levels in our lab when measured at 20C.
4. A line in blue at 20C to show the normal temperature we keep the copepods in captivity.

We will do this with `geom_hline`. 

While we're at it, we will tweak the asthetics of the plot by change `theme` parameters. We will tilt the axis text, and make the secondary y-axis blue to match the blue line representing the temperature. Because we now have a lot of elements on one graphs, we can add some faint background gridlines with the `panel.grid.major` option. 

Finally, we will add a title with `ggtitle`. 

```r
datum.SH |>
filter(between(Time, 1659337740, 1660546140)) |>
ggplot(aes(x=Date, y=DO)) +
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

```

<img src="Figures/2 week lines.png" width="600">


This is looking great. The aesthetics are clean and we have a lot of information in one place. Its easy to see the cycle of oxygen and temperature over time and how it compares to both normal lab conditions and dangerous hypoxic conditions. 

The cycles are clearly daily. 

We could hypothesize that oxygen levels and temperature in the pools increase in daylight hours. But this maybe isn't so easy to see on the graph quite yet. 

What if we could mark the sunrise and sunset time to show when the pools are exposed to sunlight?

To do that, we just need sunrise and sunset times for the days on which we recorded data from the pools. This can be obtained from almanac websites, the US Naval Observatory website, or the R package `suncalc`. I downloaded data from an Oregon almanac, since our data was from Oregon pools. 

```r
#Sunrise and sunset data
datum.ss <- read.csv(file = "Sunrise and Sunset times.csv", header = TRUE)

> head(datum.ss)
  Month.numeric Month Day Year  Sunrise   Sunset
1             6  June   1 2022 05:34:00 20:54:00
2             6  June   2 2022 05:33:00 20:55:00
3             6  June   3 2022 05:33:00 20:56:00
4             6  June   4 2022 05:32:00 20:56:00
5             6  June   5 2022 05:32:00 20:57:00
6             6  June   6 2022 05:31:00 20:58:00

> str(datum.ss)
'data.frame':	122 obs. of  6 variables:
 $ Month.numeric: int  6 6 6 6 6 6 6 6 6 6 ...
 $ Month        : chr  "June" "June" "June" "June" ...
 $ Day          : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Year         : int  2022 2022 2022 2022 2022 2022 2022 2022 2022 2022 ...
 $ Sunrise      : chr  "05:34:00" "05:33:00" "05:33:00" "05:32:00" ...
 $ Sunset       : chr  "20:54:00" "20:55:00" "20:56:00" "20:56:00" ... 
```

The data is all set up as integers or character strings, which is fine. We can format this data to match our formatting in the oxygen sensor data frame using simple base R functions. 

We'll start with using `sprintf`, which returns character vectors containing formatted text and values that we can specify. 

We'll start by adding 0's to the front of month and day values and combining the values to make a full date column. 
```r
str(datum.ss)

#Add leading zero to get in right format
datum.ss$Month.numeric <- sprintf('%02d', datum.ss$Month.numeric) #For months
datum.ss$Day <- sprintf('%02d', datum.ss$Day) #For days

#Combine year-month-day to make date variable
datum.ss$Full.Date <- paste(datum.ss$Year, datum.ss$Month.numeric, datum.ss$Day, sep="-")
```

Then, we will combine our date and time values together in a single column each for sunrise and sunset times. You'll notice, this now gives us an exact same format matching the **Date** variable in our oxygen sensor data frame. 

```r
#combine date and time columns for sunrise and sunset
datum.ss$Date.rise <- paste(datum.ss$Full.Date, datum.ss$Sunrise, sep=" ")
datum.ss$Date.set <- paste(datum.ss$Full.Date, datum.ss$Sunset, sep = " ")

> head(datum.ss[,c(8:9)])
            Date.rise            Date.set
1 2022-06-01 05:34:00 2022-06-01 20:54:00
2 2022-06-02 05:33:00 2022-06-02 20:55:00
3 2022-06-03 05:33:00 2022-06-03 20:56:00
4 2022-06-04 05:32:00 2022-06-04 20:56:00
5 2022-06-05 05:32:00 2022-06-05 20:57:00
6 2022-06-06 05:31:00 2022-06-06 20:58:00
```

Now, we can just convert those columns into POSIX format to work with our plotting code. 

We will also add a couple other columns for downstream filtering that might be helpful later. These are the dates by Month and Day, and the date converted to a sequential single number. 

We will also relevel our Month variable to match chronological order, instead of the R default alphabetical. 
```r
#Convert sunrise and sunset times to posix format
datum.ss$Date.rise <- as.POSIXct(datum.ss$Date.rise, format = "%Y-%m-%d %H:%M:%S")
datum.ss$Date.set <- as.POSIXct(datum.ss$Date.set, format = "%Y-%m-%d %H:%M:%S")

#Create month day column. This should be run after formatting the day with the leading 0
datum.ss$Month.day <- paste(datum.ss$Month, datum.ss$Day)

#Relevel month variable to be in chrono order
datum.ss$Month <- factor(datum.ss$Month, levels = c("June", "July", "August", "September"))

#Add another column with full date written as day of year for filtering
datum.ss$DOY <- yday(datum.ss$Full.Date)

> head(datum.ss)
  Month.numeric Month Day Year  Sunrise   Sunset  Full.Date           Date.rise            Date.set Month.day DOY
1            06  June  01 2022 05:34:00 20:54:00 2022-06-01 2022-06-01 05:34:00 2022-06-01 20:54:00   June 01 152
2            06  June  02 2022 05:33:00 20:55:00 2022-06-02 2022-06-02 05:33:00 2022-06-02 20:55:00   June 02 153
3            06  June  03 2022 05:33:00 20:56:00 2022-06-03 2022-06-03 05:33:00 2022-06-03 20:56:00   June 03 154
4            06  June  04 2022 05:32:00 20:56:00 2022-06-04 2022-06-04 05:32:00 2022-06-04 20:56:00   June 04 155
5            06  June  05 2022 05:32:00 20:57:00 2022-06-05 2022-06-05 05:32:00 2022-06-05 20:57:00   June 05 156
6            06  June  06 2022 05:31:00 20:58:00 2022-06-06 2022-06-06 05:31:00 2022-06-06 20:58:00   June 06 157
```

Now we can make a final modification to our code to highlight daylight hours using `geom_rect`. 

We will laydown mostly transparent, verticle rectangles that directly start and end at the sunrise and sunset times. Because we went through the trouble of getting Date.rise and Date.set in POSIX format matching our existing data, we can easily map these to our exisiting x-axis with a single line. 

```r
geom_rect(aes(NULL, NULL, xmin = Date.rise, xmax = Date.set), data = subset(datum.ss, DOY %in% 213:227),
                     ymin = 0, ymax = 32, fill = "yellow", alpha=0.15)+
```

The final product: 

```r

datum.SH |>
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
```

<img src="Figures/DO data zoomed 2 weeks SH.png" width="600">

Now we can directly see the connection between sunlight and oxygen fluctuations. 

With longitudinal data, you have an opportunity to tell a story. The data changes over time and we can take advantage of that to make our figure impactful. 

Let's animate the figure so we can watch the rise and fall of the oxygen in the pools over the daily cycles. To do this, we can use a package expansion to ggplot2 called `gganimate`. 

All we need to do is add a line to the end of our code with the `transition_reveal` command. This command allows you to make data gradually appear based on a time variable. 

Since our x-axis represents our time variable, we can code the reveal based on the Date columnn in our data frame. 

Lastly, we will remove the top blue line showing lab temperature, just to keep our focus on the oxygen flux. 

```r
datum.SH |>
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
```

Next, we will animate our figure and save it as a gif_image object in our R environment using the `animate` command. 

We want the animation to be smooth, so we will specify the gif to be 200 frames at 10 frames per second with a pause at the end before restarting. 

To maximize quality we will also specify the compression to be lzw (lossless, typical for gif files). 

We will then export the gif using the `anim_save` function. 

```r
anim <- animate(SH.zoom.animation.2week, nframes = 200, fps = 10, height = 6.5, width = 11, end_pause = 40,
units = "in", res = 300, compression = "lzw")

anim_save("DO Animation 2 weeks in August for SH.gif", anim)
```

Here's our final product!

<img src="animated_figures/DO Animation 2 weeks in August for SH.gif" width="600">

If we zoom in to just two days, we can really see the timing of oxygen increase and decrease. 

Let's also add an extra bit of mapping to our plot with `label = DO`.

```r
datum.SH |>
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
```


<img src="animated_figures/DO Animation 2 days in August for SH.gif" width="600">

You'll notice that oxygen starts increasing with the sun rising, even before temperature increases. This is good evidence that temperature is not causing the increase in oxygen, but something else. 

Turns out, the only living things in the pools besides the copepdos are algae. At night, they consume oxygen because they can't photosynthesize; but during the day, they switch back to photosynthesis and produce a ton of oxygen! 




