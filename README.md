Instructions for utilizing scripts and data sheets. 

Each R script in the zip folder will have the required .csv files listed at the start of the script near the top. 
You will notice that the name of the file to be read in (in the format filename.csv) corresponds to the name of a sheet in the Master Data Workbook .xlsx file. 
To import the data into R, simply save each sheet as its own .csv file in the working directory you wish to use to run the R script. 

Exceptions to the above instructions: 
1) The "Combined Resp Script.R" script imports raw respirometry files in one at a time, based on the user's choice. 
These raw respirometry scripts can be found in the zip folder called "All Individual Respirometry Raw Data Files". Simply save these files in your R working directory and you can import them into the combined resp script one at a time.

2) The DO Script.R script imports oxygen data from the two text files "SH_pool3_30JUN-27SEP_Full.txt" and "BOB_pool2_28JUN-27SEP_Full.txt". These have been uploaded separate from the master data workbook .xlsx sheet. 


