
#  The resistance to extreme hypoxia by an intertidal crustacean

## Instructions for utilizing scripts and data sheets. 

Each R script will have the required .csv files listed at the start of the script when you open it up in R. The scripts and required .csv files are saved as individual files in this repository. 

**Exceptions** to the above instructions: 
1) The "Combined Resp Script.R" script imports raw respirometry files in one at a time, based on the user's choice. 
These raw respirometry scripts can be found in the zip folder called "All Individual Respirometry Raw Data Files". Simply save these files in your R working directory and you can import them into the combined resp script one at a time.

2) The DO Script.R script imports oxygen data from the two text files "SH_pool3_30JUN-27SEP_Full.txt" and "BOB_pool2_28JUN-27SEP_Full.txt". These are imported as text files and not .csv files in the DO Script.R script. 

# Highlights from the study

## Vizualizing continuous oxygen sensor data from the copepod's habitat

Problem: How do we take raw oxygen values recorded every 10 minutes and turn those into a clear picture what the copepods experience on a daily and nightly basis?

Let's start with the data. Below is a snapshot of the raw data file and below that an abbreviated view of the columns we care about. 

<details>
<summary>Raw data snapshot (R)</summary>

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

</details>

| Time       | Date (Central)      | Month | Temp (°C) | DO (mg/L) | Sat (%) |
|------------|---------------------|-------|-----------|-----------|---------|
| 1656603540 | 2022-06-30 08:39:00 | June  | 15.148    | 4.282     | 52.787  |
| 1656604140 | 2022-06-30 08:49:00 | June  | 15.019    | 4.026     | 49.506  |
| 1656604740 | 2022-06-30 08:59:00 | June  | 15.019    | 4.220     | 51.897  |
| 1656605340 | 2022-06-30 09:09:00 | June  | 15.122    | 4.384     | 54.020  |
| 1656605940 | 2022-06-30 09:19:00 | June  | 15.328    | 4.393     | 54.350  |
| 1656606540 | 2022-06-30 09:29:00 | June  | 15.551    | 4.592     | 57.069  |

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
