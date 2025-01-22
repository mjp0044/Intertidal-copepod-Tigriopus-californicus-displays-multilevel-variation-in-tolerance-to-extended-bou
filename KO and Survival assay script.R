#KO and Survival Assay Script 
#Matthew Powers 
#powerm3@oregonstate.edu
#Combines analysis of KO assay data and generates figures
#Last edited 1-21-25

#load required packages
library(ggpubr) #Also loads basic ggplot2
library(cowplot) #Pretty ggplots!
library(reshape2) #Data wrangling
library(dplyr) #Data wrangling
library(tidyverse) #Data wrangling
library(stringr) #data wrangling for splitting column characters
library(MetBrewer) #Pretty colors!
library(scales) #Pretty colors!
library(palettetown) #Pretty ggplots!
library(grid) #for adding tables to plots and making multiple panels
library(gridExtra) #for adding tables to plots and making multiple panels
library(glue) #Stick stuff in other stuff! Like pasting text in data frames and plot elements
library(patchwork) #stitching graphs together
library(ggiraph) #Connected interactive graphs
library(MESS) #data wrangling and modeling
library(stats) #data wrangling and modeling
library(lme4) #data wrangling and modeling
library(lmerTest) #Get null hypothesis test from lme4 lmer models
library(emmeans) #data wrangling and modeling
library(multcomp) #automatically assign pairwise significance groupings
library(multcompView) #See groupings from multcomp
library(betareg) #To model proportions


#Set theme globally
theme_set(theme_cowplot())

#Read in data 
  datum.KO = read.csv(file="KO.data.csv", header = TRUE)
  datum.surv = read.csv(file="Plate survival.csv", header = TRUE)

#Remove data points with less than 10 copepods
  # datum.KO <- subset(datum.KO, Total.copepods == 10)
  # datum.surv <- subset(datum.surv, Total.copepods == 10)

  
###Prep and summarize KO data
  
    #Relabel sex factor 
      datum.KO %>% 
      mutate(Sex = fct_recode(Sex, "Female" = "F",
                              "Male" = "M")) -> datum.KO
    
    #Combine sex and population factors for plotting or stats groupings 
      datum.KO$Pop.sex = paste(datum.KO$Sex, datum.KO$Pop)
    
      #Create lengthwise data frame for time series data
      ###Melt KO data frame to organize LOE data by group ID
          #datum.KO2 = melt(datum.KO[,c(3, 8, 9, 10, 11, 12, 13, 14, 15, 28)], id=c("Replicate.ID", "Time.minutes", "Group")) #counts
          ##OR
          datum.KO2 = melt(datum.KO[,c(3, 8, 20, 21, 22, 23, 24, 25, 26, 28)], id=c("Replicate.ID", "Time.minutes", "Group")) #proportion
          
          datum.KO2[c('Pop.sex', 'replicate')] <- str_split_fixed(datum.KO2$Replicate.ID, '[.]', 2)
          
        
          #Relabel time factor (choose 1 of 2 below)
          
          #for proportions
          datum.KO2 %>%
             mutate(variable = fct_recode(variable, "0" = "Hour0.prop",
                                     "1" = "Hour1.prop",
                                     "2" = "Hour2.prop",
                                     "3" = "Hour3.prop",
                                     "4" = "Hour4.prop",
                                     "5" = "Hour5.prop",
                                     "6" = "Hour6.prop")) -> datum.KO2
          #OR for counts
           # datum.KO2 %>% 
           #   mutate(variable = fct_recode(variable, "0" = "X0hr",
           #                                "1" = "X1hr",
           #                                "2" = "X2hr",
           #                                "3" = "X3hr",
           #                                "4" = "X4hr",
           #                                "5" = "X5hr",
           #                                "6" = "X6hr")) -> datum.KO2
    
          
          
          #Relabel group factor
          datum.KO2 %>% 
                mutate(Pop.sex = fct_recode(Pop.sex, "Female BOB" = "FBOB", 
                                          "Female BR" = "FBR", 
                                          "Female SD" = "FSD", 
                                          "Female SH" = "FSH", 
                                          "Male BOB" = "MBOB", 
                                          "Male BR" = "MBR", 
                                          "Male SD" = "MSD", 
                                          "Male SH" = "MSH")) -> datum.KO2
          
          #Make new columns with sex and pop split
          datum.KO2[c('Sex', 'Pop')] <- str_split_fixed(datum.KO2$Pop.sex, ' ', 2)
          
          datum.KO2$Sex = as.factor(datum.KO2$Sex) #Change to factor
          datum.KO2$Pop = as.factor(datum.KO2$Pop) #Change to factor
          
          #Relevel group variable to put in the desired order for plotting
          datum.KO2$Pop.sex<- factor(datum.KO2$Pop.sex,
                                                  levels = c("Female SD", "Male SD","Female BR", "Male BR",
                                                             "Female BOB", "Male BOB", "Female SH", "Male SH"))
          
          #Relevel group variable to put in the desired order for plotting
          datum.KO2$Pop<- factor(datum.KO2$Pop,
                                levels = c("SD", "BR","BOB", "SH"))
          
          
          
          #Rename columns to easier names to remember
          colnames(datum.KO2)[c(4,5)] <- c("Time", "LOE")
          
          #Refactor time column 
          datum.KO2$Time <- as.numeric(as.character(datum.KO2$Time))
          
          #Transform proportions to be between 0 and 1 but not include 0 and 1 according to DOI: 10.1037/1082-989X.11.1.54 
          #Smithson, M. & Verkuilen, J. Psychol. Methods 11, 54–71 (2006). 
          datum.KO2$LOE.trans <- ((datum.KO2$LOE*391)+0.5)/392 
          
          str(datum.KO2) #Check variable encoding
          
          
          #Population summary
            datum.KO.pop.mean <- datum.KO2 |> 
              filter(Group != "Control") |>
              filter(LOE != "NA") |>
              group_by(Pop, Time) |> 
              summarise(mean_KO = mean(LOE) |> round(2),
                        sd_KO = sd(LOE) |> round(2),
                        n = n(),
                        iqr = IQR(LOE, na.rm=TRUE),
                        range = paste(range(LOE) |> round(2), collapse = ' - ')
              ) |> 
              mutate(mean_text = glue('In {Pop}, the mean KO at {Time} hours was {number(mean_KO, accuracy = 0.01)} individuals out of 10')) |>
              mutate(se_KO = sd_KO/sqrt(n)) 
            datum.KO.pop.mean$se_KO <- round(datum.KO.pop.mean$se_KO, 2) #Round se column to 2 digits
          
          #Sex summary
            datum.KO.sex.mean <- datum.KO2 |>
              filter(Group != "Control") |>
              filter(LOE != "NA") |>
              group_by(Sex, Time) |> 
              summarise(mean_KO = mean(LOE) |> round(2),
                        sd_KO = sd(LOE) |> round(2),
                        n = n(),
                        iqr = IQR(LOE, na.rm=TRUE),
                        range = paste(range(LOE) |> round(2), collapse = ' - ')
              ) |> 
              mutate(mean_text = glue('In {Sex}s, the mean KO at {Time} hours was {number(mean_KO, accuracy = 0.01)} individuals out of 10')) |>
              mutate(se_KO = sd_KO/sqrt(n)) 
            datum.KO.sex.mean$se_KO <- round(datum.KO.sex.mean$se_KO, 2) #Round se column to 2 digits
          
          #Pop.sex summary
            datum.KO.popsex.mean <- datum.KO2 |> 
              filter(Group != "Control") |>
              filter(LOE != "NA") |>
              group_by(Pop.sex, Time) |> 
              summarise(mean_KO = mean(LOE) |> round(2),
                        sd_KO = sd(LOE) |> round(2),
                        n = n(),
                        iqr = IQR(LOE, na.rm=TRUE),
                        range = paste(range(LOE) |> round(2), collapse = ' - ')
              ) |> 
              mutate(mean_text = glue('In {Pop.sex}s, the mean KO at {Time} hours was {number(mean_KO, accuracy = 0.01)} individuals out of 10')) |>
              mutate(se_KO = sd_KO/sqrt(n)) 
            datum.KO.popsex.mean$se_KO <- round(datum.KO.popsex.mean$se_KO, 2) #Round se column to 2 digits
          
            
        ##Control data 
            #Population summary
            datum.KO.pop.mean.control <- datum.KO2 |> 
              filter(Group == "Control") |>
              filter(LOE != "NA") |>
              group_by(Pop, Time) |> 
              summarise(mean_KO = mean(LOE) |> round(2),
                        sd_KO = sd(LOE) |> round(2),
                        n = n(),
                        iqr = IQR(LOE, na.rm=TRUE),
                        range = paste(range(LOE) |> round(2), collapse = ' - ')
              ) |> 
              mutate(mean_text = glue('In the {Pop} controls, the mean KO at {Time} hours was {number(mean_KO, accuracy = 0.01)} individuals out of 10')) |>
              mutate(se_KO = sd_KO/sqrt(n)) 
            datum.KO.pop.mean.control$se_KO <- round(datum.KO.pop.mean.control$se_KO, 2) #Round se column to 2 digits
            
            #Sex summary
            datum.KO.sex.mean.control <- datum.KO2 |>
              filter(Group == "Control") |>
              filter(LOE != "NA") |>
              group_by(Sex, Time) |> 
              summarise(mean_KO = mean(LOE) |> round(2),
                        sd_KO = sd(LOE) |> round(2),
                        n = n(),
                        iqr = IQR(LOE, na.rm=TRUE),
                        range = paste(range(LOE) |> round(2), collapse = ' - ')
              ) |> 
              mutate(mean_text = glue('In control {Sex}s, the mean KO at {Time} hours was {number(mean_KO, accuracy = 0.01)} individuals out of 10')) |>
              mutate(se_KO = sd_KO/sqrt(n)) 
            datum.KO.sex.mean.control$se_KO <- round(datum.KO.sex.mean.control$se_KO, 2) #Round se column to 2 digits
            
            
            #Sex summary
            datum.KO.sex.mean.control <- datum.KO2 |>
              filter(Group == "Control") |>
              filter(LOE != "NA") |>
              group_by(Sex, Time) |> 
              summarise(mean_KO = mean(LOE) |> round(2),
                        sd_KO = sd(LOE) |> round(2),
                        n = n(),
                        iqr = IQR(LOE, na.rm=TRUE),
                        range = paste(range(LOE) |> round(2), collapse = ' - ')
              ) |> 
              mutate(mean_text = glue('In control {Sex}s, the mean KO at {Time} hours was {number(mean_KO, accuracy = 0.01)} individuals out of 10')) |>
              mutate(se_KO = sd_KO/sqrt(n)) 
            datum.KO.sex.mean.control$se_KO <- round(datum.KO.sex.mean.control$se_KO, 2) #Round se column to 2 digits
        


### Prep and summarize Survival data
      
      #Relabel sex factor
      datum.surv %>% 
        mutate(Sex = fct_recode(Sex, "Female" = "F",
                                "Male" = "M")) -> datum.surv
      
      #Combine sex and population factors into a Pop.sex factor
      datum.surv$Pop.sex <- paste(datum.surv$Sex, datum.surv$Pop)
      
      #Relevel group variable to put in the desired order for plotting
      datum.surv$Pop.sex<- factor(datum.surv$Pop.sex,
                           levels = c("Female SD", "Male SD","Female BR", "Male BR",
                                      "Female BOB", "Male BOB", "Female SH", "Male SH"))
      
      #Relevel group variable to put in the desired order for plotting
      datum.surv$Pop<- factor(datum.surv$Pop,
                         levels = c("SD", "BR","BOB", "SH"))
      
      #Relevel group variable to put in the desired order for plotting
      datum.surv$Group<- factor(datum.surv$Group,
                                  levels = c("Con - 6 Hours", "Con - 24 Hours","Exp - 6 Hours", "Exp - 15 Hours",
                                             "Exp - 24 Hours"))
      
      #Transform proportions to be between 0 and 1 but not include 0 and 1 according to DOI: 10.1037/1082-989X.11.1.54 
      #Smithson, M. & Verkuilen, J. Psychol. Methods 11, 54–71 (2006). 
      datum.surv$Surv.trans <- ((datum.surv$Surv.prop*159)+0.5)/160
      datum.surv$KO.trans <- ((datum.surv$KO.prop*159)+0.5)/160
      
      str(datum.surv) #Check data coding before proceeding
      
      
    #Summarize data for plotting
      #Population summary
        datum.surv.pop.mean <- datum.surv |> 
          filter(Replicate != "control")|>
          group_by(Pop, Hours) |> 
          summarise(mean_surv = mean(Surv.prop) |> round(2),
                    sd_surv = sd(Surv.prop) |> round(2),
                    n = n(),
                    iqr = IQR(Surv.prop),
                    range = paste(range(Surv.prop) |> round(2), collapse = ' - ')
          ) |> 
          mutate(mean_text = glue('In {Pop}, the mean survival proportion at {Hours} hours was {number(mean_surv, accuracy = 0.01)}'))
        
      #Sex summary
        datum.surv.sex.mean <- datum.surv |> 
          filter(Replicate != "control")|>
          group_by(Sex, Hours) |> 
          summarise(mean_surv = mean(Surv.prop) |> round(2),
                    sd_surv = sd(Surv.prop) |> round(2),
                    n = n(),
                    iqr = IQR(Surv.prop),
                    range = paste(range(Surv.prop) |> round(2), collapse = ' - ')
          ) |> 
          mutate(mean_text = glue('In {Sex}s, the mean survival proportion at {Hours} hours was {number(mean_surv, accuracy = 0.01)}'))
        
      #Pop.sex summary
        datum.surv.popsex.mean <- datum.surv |>
          filter(Replicate != "control")|>
          group_by(Pop.sex, Pop, Sex, Hours) |> 
          summarise(mean_surv = mean(Surv.prop) |> round(2),
                    sd_surv = sd(Surv.prop) |> round(2),
                    n = n(),
                    iqr = IQR(Surv.prop),
                    range = paste(range(Surv.prop) |> round(2), collapse = ' - ')
          ) |> 
          mutate(mean_text = glue('In {Pop} {Sex}s, the mean survival proportion at {Hours} hours was {number(mean_surv, accuracy = 0.01)}'))
      
      
      #Control summary
        datum.surv.control.mean <- datum.surv |>
          group_by(Group) |> 
          summarise(mean_surv = mean(Surv.prop) |> round(2),
                    sd_surv = sd(Surv.prop) |> round(2),
                    n = n(),
                    iqr = IQR(Surv.prop),
                    range = paste(range(Surv.prop) |> round(2), collapse = ' - ')
          ) |> 
          mutate(mean_text = glue('In {Group}, the mean survival proportion was {number(mean_surv, accuracy = 0.01)}'))
        
      
        #Summarize total survivors
        surv.totals.24 <- datum.surv |>
          filter(Hours == 24)
          
          sum(surv.totals.24$Surv)
          
          surv.totals.15 <- datum.surv |>
            filter(Hours == 15)
          
          sum(surv.totals.15$Surv)
          
          surv.totals.6 <- datum.surv |>
            filter(Hours == 6)
          
          sum(surv.totals.6$Surv)
          
          
          
      
### Summarize time to anoxia data
      #Summarize data for plotting
      #Population summary
      datum.tua.pop.mean <- datum.surv |> 
        filter(Replicate != "control")|>
        filter(Hours != "control") |>
        group_by(Pop) |> 
        summarise(mean_tua = mean(Time_minutes) |> round(2),
                  sd_tua = sd(Time_minutes) |> round(2),
                  n = n(),
                  iqr = IQR(Time_minutes),
                  range = paste(range(Time_minutes) |> round(2), collapse = ' - ')
        ) |> 
        mutate(mean_text = glue('In {Pop}, the mean time to anoxia was {number(mean_tua, accuracy = 0.01)} minutes'))
      
      #Sex summary
      datum.tua.sex.mean <- datum.surv |>
        filter(Replicate != "control")|>
        filter(Hours != "control") |>
        group_by(Sex) |> 
        summarise(mean_tua = mean(Time_minutes) |> round(2),
                  sd_tua = sd(Time_minutes) |> round(2),
                  n = n(),
                  iqr = IQR(Time_minutes),
                  range = paste(range(Time_minutes) |> round(2), collapse = ' - ')
        ) |> 
        mutate(mean_text = glue('In {Sex}s, the mean time to anoxia was {number(mean_tua, accuracy = 0.01)} minutes'))
      
      #Pop.sex summary
      datum.tua.popsex.mean <- datum.surv |>
        filter(Replicate != "control")|>
        filter(Hours != "control") |>
        group_by(Pop.sex, Pop, Sex) |> 
        summarise(mean_tua = mean(Time_minutes) |> round(2),
                  sd_tua = sd(Time_minutes) |> round(2),
                  n = n(),
                  iqr = IQR(Time_minutes),
                  range = paste(range(Time_minutes) |> round(2), collapse = ' - ')
        ) |> 
        mutate(mean_text = glue('In {Pop} {Sex}s, the mean time to anoxia was {number(mean_tua, accuracy = 0.01)} minutes'))
      
#Color pallete tweaking for ggplot (optional)
      br_pal <- met.brewer("Signac")
      br_pal2 <- met.brewer("Renoir")
      my_pal <- c(br_pal2[c(8,6)], br_pal[c(3,1)], br_pal2[c(2,4, 12,11)])
      # just for displaying old and new palette - not required for solution
      show_col(br_pal)
      show_col(br_pal2)
      show_col(my_pal)
      
      
      #Color pallete tweaking for ggplot (optional)
      br_pal <- met.brewer("Signac")
      br_pal2 <- met.brewer("Renoir")
      my_pal2 <- c(br_pal2[8], br_pal[3], br_pal2[c(2,12)])
      # just for displaying old and new palette - not required for solution
      show_col(br_pal)
      show_col(br_pal2)
      show_col(my_pal2)
      
      #Color pallete tweaking for ggplot (optional)
      br_pal3 <- pokepal(pokemon = "crawdaunt")
      my_pal3 <- c(br_pal3[c(11,4)])
      # just for displaying old and new palette - not required for solution
      show_col(br_pal3)
      show_col(my_pal3)
      
      #Color pallete tweaking for ggplot (optional)
      my_pal4 <- c(br_pal3[c(15,3)])
      # just for displaying old and new palette - not required for solution
      show_col(my_pal4)
      
      my_pal5 <- c(br_pal3[c(15,12,4,3,2)])
      # just for displaying old and new palette - not required for solution
      show_col(my_pal5)

      
#### KO curves and multiple variable models  ####
    
        
    #Make individual replicate curves
        KOcurves <- datum.KO2 |>
        filter(Group != "Control") |>
        ggplot(aes(x=Time, y=LOE, group=Replicate.ID, color=Pop.sex)) +
          geom_line_interactive(size=1, alpha=1)+
          geom_point_interactive()+
          facet_wrap(~Pop,  nrow=4, strip.position = "left")+
          scale_y_continuous(name="Number of copepods with LOE (out of 10)", breaks = seq(0, 1, by = 0.1))+
          scale_x_continuous(name="Hours post plateau", breaks = seq(0, 6, by = 1), position="bottom")+
          xlab("Hours post plateau")+
          scale_color_manual("Groups (A, B):",values = my_pal) +
          theme(axis.text.x = element_text(size = 11),
                axis.title = element_text(size = 13),
                axis.text.y = element_text(size=10),
                legend.position = "none", legend.direction = "horizontal", strip.placement = 'outside', strip.text = element_text(size = 14))
        KOcurves
        
        #Export plot as png
        png(filename = "Individual KO curves.png", width = 5, height = 7, units = "in", res = 300)
        KOcurves
        dev.off()
        
        
    #Experimental data   
      #Make interactive plot for population
        KO.by.pop <- datum.KO.pop.mean |>
          ggplot(aes(x=Time, y=mean_KO, col=Pop))+
          geom_point_interactive(size = 6, aes(tooltip = mean_text)) +
          geom_line_interactive(linewidth=2)+
          geom_errorbar(aes(ymax = mean_KO + se_KO, 
                            ymin = mean_KO - se_KO),
                        width=0.4)+
          scale_x_continuous(name="Hours anoxia exposure", breaks = seq(0, 6, by = 1), position="bottom")+
          scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(0, 1))+
          scale_color_manual("Populations:",values = my_pal2)+
          theme(legend.position = "bottom", legend.direction = "horizontal")+
          ggtitle(label = "6 Hours")
        KO.by.pop
        
      #Make interactive plot for sex
        KO.by.sex <- datum.KO.sex.mean |>
          ggplot(aes(x=Time, y=mean_KO, col=Sex))+
          geom_point_interactive(size = 6, aes(tooltip = mean_text)) +
          geom_line_interactive(linewidth=2)+
          geom_errorbar(aes(ymax = mean_KO + se_KO, 
                            ymin = mean_KO - se_KO),
                        width=0.4)+
          scale_x_continuous(name="Hours anoxia exposure", breaks = seq(0, 6, by = 1), position="bottom")+
          scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
          scale_color_manual("Sexes:",values = my_pal3)+
          theme(legend.position = "bottom", legend.direction = "horizontal")+
          ggtitle(label = "6 Hours")
        KO.by.sex
        
      #Make interactive plot for population and sex
        KO.by.popsex <- datum.KO.popsex.mean |>
          ggplot(aes(x=Time, y=mean_KO, col=Pop.sex))+
          geom_point_interactive(size = 6, aes(tooltip = mean_text)) +
          geom_line_interactive(linewidth=2)+
          geom_errorbar(aes(ymax = mean_KO + se_KO, 
                            ymin = mean_KO - se_KO),
                        width=0.4)+
          scale_x_continuous(name="Hours anoxia exposure", breaks = seq(0, 6, by = 1), position="bottom")+
          scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
          scale_color_manual("Groups:",values = my_pal)+
          theme(legend.position = "bottom", legend.direction = "horizontal", axis.title.x = element_text(vjust=14),
                axis.title.y = element_text(size=12))+
          ggtitle(label = "6 Hours")
        KO.by.popsex
        
        
    ##Control data
      #Make interactive plot for population
        KO.by.pop.control <- datum.KO.pop.mean.control |>
          ggplot(aes(x=Time, y=mean_KO, col=Pop))+
          geom_point_interactive(size = 6, aes(tooltip = mean_text), alpha=0.5) +
          geom_line_interactive(linewidth=2)+
          geom_errorbar(aes(ymax = mean_KO + se_KO, 
                            ymin = mean_KO - se_KO),
                        width=0.4)+
          scale_x_continuous(name="Hours post plateau", breaks = seq(0, 6, by = 1), position="bottom")+
          scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
          scale_color_manual("Populations:",values = my_pal2)+
          theme(legend.position = "bottom", legend.direction = "horizontal",
                title = element_text(size=10))+
          ggtitle(label = "Control - 6 Hours")
        KO.by.pop.control
        
        #Make interactive plot for sex
        KO.by.sex.control <- datum.KO.sex.mean.control |>
          ggplot(aes(x=Time, y=mean_KO, col=Sex))+
          geom_point_interactive(size = 6, aes(tooltip = mean_text), alpha=0.5) +
          geom_line_interactive(linewidth=2)+
          geom_errorbar(aes(ymax = mean_KO + se_KO, 
                            ymin = mean_KO - se_KO),
                        width=0.4)+
          scale_x_continuous(name="Hours anoxia exposure", breaks = seq(0, 6, by = 1), position="bottom")+
          scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
          scale_color_manual("Sexes:",values = my_pal3)+
          theme(legend.position = "bottom", legend.direction = "horizontal",
                title = element_text(size=10))+
          ggtitle(label = "Control - 6 Hours")
        KO.by.sex.control
        
        #Make interactive plot for population and sex
        KO.by.popsex.control <- datum.KO2 |>
          filter(Group == "Control") |>
          ggplot(aes(x=Time, y=LOE, col=Pop.sex))+
          geom_point_interactive(size = 6) +
          geom_line_interactive(linewidth=2)+
          scale_x_continuous(name="Hours anoxia exposure", breaks = seq(0, 6, by = 1), position="bottom")+
          scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
          scale_color_manual("Groups:",values = my_pal)+
          theme(legend.position = "bottom", legend.direction = "horizontal", axis.title.x = element_text(vjust=14),
                title = element_text(size=10), axis.title.y = element_text(size=11))+
          ggtitle(label = "Control - 6 Hours")
        KO.by.popsex.control
        
        
#### Modeling effect of time spent at 0 on LOE  ####
        
      #Is there a significant difference in the effect of anoxia on LOE between the different populations?
        #Model looks for effect of time and pop and their interaction on LOE
        #Model includes effect of sex to control for non-independence of data coming from different sexes
        #beta regression logit link function used because it's proportion data (0,1) in the response variable
        
        #Relevel group variable to put in the desired order for plotting
        datum.KO2$Pop<- factor(datum.KO2$Pop,
                               levels = c("SD", "BR","BOB", "SH"))
        
        mod.1 <- betareg(LOE.trans ~Time*Pop + Sex, data=subset(datum.KO2, Group != "Control"))
        summary(mod.1)
        plot(mod.1)

        emm.1 <- emtrends(mod.1, ~Pop, var="Time",  mode = "response") #Create em_grid object on response scale (odds ratio)
        emm.1 #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
        emm.1.contrasts <- contrast(emm.1,"pairwise", type = "response") #Do pairwise contrasts between pops
        emm.1.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
        confint(emm.1.contrasts) #Show pairwise contrasts with confidence intervals
        cld(emm.1) #Get group letter assignments for plotting
            
            write.csv(emm.1, file = "Time effect on KO - Among Pop comparison means.csv", row.names = FALSE) 
            write.csv(emm.1.contrasts, file = "Time effect on KO - Among Pop comparison.csv", row.names = FALSE) 
            write.csv(confint(emm.1.contrasts), file = "Time effect on KO - Among Pop comparison CIs.csv", row.names = FALSE)
         
        
      #Is there a significant difference in the effect of time on LOE between the different population and sex groupings?
        #Model looks for effect of time and grouping and their interaction on LOE
        #beta regression logit link function used because it's proportion data (0,1) in the response variable
         
  
          mod.2 <- betareg(LOE.trans ~Time*Pop.sex, data=subset(datum.KO2, Group != "Control"))
          summary(mod.2)
          plot(mod.2)
          
          emm.2 <- emtrends(mod.2, ~Pop.sex, var="Time",  mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.2 #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.2.contrasts <- contrast(emm.2,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.2.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.2.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.2) #Get group letter assignments for plotting
            
            write.csv(emm.2, file = "Time effect on KO - Among Popsex comparison means.csv", row.names = FALSE)
            write.csv(emm.2.contrasts, file = "Time effect on KO - Among PopSex Grouping comparison .csv", row.names = FALSE) #Save final frame as a csv
            write.csv(confint(emm.2.contrasts), file = "Time effect on KO - Among PopSex Grouping comparison CIs.csv", row.names = FALSE) #Save final frame as a csv
    
    #Is there a significant difference in the effect of time on LOE between males and females?
      #Model looks for effect of time and sex and their interaction on LOE
      #Model includes random effect of population to control for non-independence of data coming from different pops.
      #poisson distribution link function used because it's count data in the response variable
          mod.3 <- betareg(LOE.trans~Time*Sex + Pop, data=subset(datum.KO2, Group != "Control"))
          summary(mod.3)
          plot(mod.3)
          
          emm.3 <- emtrends(mod.3, ~Sex, var="Time",  mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.3 #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.3.contrasts <- contrast(emm.3,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.3.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.3.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.3) #Get group letter assignments for plotting
          
          
          write.csv(emm.3, file = "Time effect on KO - Between Sexes comparison means.csv", row.names = FALSE)
          write.csv(emm.3.contrasts, file = "Time effect on KO - Between Sexes comparions.csv") #Export result as .csv for supplement
          write.csv(confint(emm.3.contrasts), file = "Time effect on KO - Between Sexes comparions CIs.csv") #Export result as .csv for supplement
    
          
          
      #### Does time spent in anoxia affect the rate of LOE in controls? 
          #Using beta regression model again
          datum.KO2.controls <- subset(datum.KO2, Group == "Control")
          
          mod.KO.controls <- betareg(LOE.trans~Time, data = datum.KO2.controls)
          summary(mod.KO.controls)
          
          emm.con <- emtrends(mod.KO.controls, ~Time, var = "Time", mode = "response")
          emm.con
          
          summary(mod.KO.controls)$pseudo.r.squared
          
          
          
      ###Do experimental and control wells differ in mean LOE proportion after 6 hours anoxia?
          #Transform proportions to be between 0 and 1 but not include 0 and 1 according to DOI: 10.1037/1082-989X.11.1.54 
          #Smithson, M. & Verkuilen, J. Psychol. Methods 11, 54–71 (2006). 
          datum.KO$Hour6.prop.trans <- ((datum.KO$Hour6.prop*55)+0.5)/56 
          
          mod.control.vs.exp <- betareg(Hour6.prop.trans ~ Group, data = datum.KO)
          summary(mod.control.vs.exp)
          plot(mod.control.vs.exp)
          
          emm.exp.con <- emmeans(mod.control.vs.exp, ~Group,  mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.exp.con #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.exp.con.contrasts <- contrast(emm.exp.con, "pairwise", type = "response") #Do pairwise contrasts between pops
          emm.exp.con.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.exp.con.contrasts)
          
        #Make plot comparing experimental vs control LOE proportion after 6 hours anoxia exposure
          exp.vs.control.LOE <- datum.KO |>
            ggplot(aes(x=Group, y=Hour6.prop, fill=Group))+
            geom_boxplot()+
            geom_point()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="Proportion of copepods with LOE", breaks = seq(0, 1, by = 0.1), limits = c(-0.05, 1))+
            scale_fill_manual("Groups:",values=my_pal4)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
                  axis.title.y = element_text(size = 14),title = element_text(size = 13),
                  axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1))+
            ggtitle(label = "LOE after 6 hours")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "b", size = 5)
          exp.vs.control.LOE
      
####  Survival analysis ####
      
  #Population
      #Is there a significant difference among populations in each of the exposure times and controls?
          #6 hours
            mod.4 <- betareg(Surv.trans~Pop + Sex, data = subset(datum.surv, Hours == "6"))
            summary(mod.4)
            plot(mod.4)
            
            emm.4 <- emmeans(mod.4, ~Pop, mode = "response") #Create em_grid object on response scale (odds ratio)
            emm.4 #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
            
            emm.4.contrasts <- contrast(emm.4, "pairwise", type = "response") #Do pairwise contrasts between pops
            emm.4.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
            confint(emm.4.contrasts) #Show pairwise contrasts with confidence intervals
            cld(emm.4) #Get group letter assignments for plotting
            
            write.csv(emm.4.contrasts, file = "Survival comparison by pop 6 hours contrasts.csv", row.names = FALSE)
            write.csv(confint(emm.4.contrasts), file = "Survival comparison by pop 6 hours contrasts CIs.csv", row.names = FALSE)
            
            
            
          #15 hours
            mod.4b <- betareg(Surv.trans~Pop + Sex, data = subset(datum.surv, Hours == "15"))
            summary(mod.4b)
            plot(mod.4b)
            
            emm.4b <- emmeans(mod.4b, ~Pop, mode = "response") #Create em_grid object on response scale (odds ratio)
            emm.4b #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
            
            emm.4b.contrasts <- contrast(emm.4b,"pairwise", type = "response") #Do pairwise contrasts between pops
            emm.4b.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
            confint(emm.4b.contrasts) #Show pairwise contrasts with confidence intervals
            cld(emm.4b) #Get group letter assignments for plotting
            
            write.csv(emm.4b.contrasts, file = "Survival comparison by pop 15 hours contrasts.csv", row.names = FALSE)
            write.csv(confint(emm.4b.contrasts), file = "Survival comparison by pop 15 hours contrasts CIs.csv", row.names = FALSE)
            
            
          
          #24 hours
            mod.4c <- betareg(Surv.trans~Pop + Sex, data = subset(datum.surv, Hours == "24"))
            summary(mod.4c)
            plot(mod.4c)
            emm.4c <- emmeans(mod.4c, ~Pop, mode = "response") #Create em_grid object on response scale (odds ratio)
            emm.4c #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
            
            emm.4c.contrasts <- contrast(emm.4c,"pairwise", type = "response") #Do pairwise contrasts between pops
            emm.4c.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
            confint(emm.4c.contrasts) #Show pairwise contrasts with confidence intervals
            cld(emm.4c) #Get group letter assignments for plotting
            
            write.csv(emm.4c.contrasts, file = "Survival comparison by pop 24 hours contrasts.csv", row.names = FALSE)
            write.csv(confint(emm.4c.contrasts), file = "Survival comparison by pop 24 hours contrasts CIs.csv", row.names = FALSE)
            
            
            
          #24 hour control 
            mod.4d <- betareg(Surv.trans~Pop + Sex, data = subset(datum.surv, Hours == "control"))
            summary(mod.4d)
            plot(mod.4d)
            emm.4d <- emmeans(mod.4d, ~Pop, mode = "response") #Create em_grid object on response scale (odds ratio)
            emm.4d #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
            
            emm.4d.contrasts <- contrast(emm.4d,"pairwise", type = "response") #Do pairwise contrasts between pops
            emm.4d.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
            confint(emm.4d.contrasts) #Show pairwise contrasts with confidence intervals
            cld(emm.4d) #Get group letter assignments for plotting
            
            write.csv(emm.4d.contrasts, file = "Survival comparison by pop control 24 hours contrasts.csv", row.names = FALSE)
            write.csv(confint(emm.4d.contrasts), file = "Survival comparison by pop control 24 hours contrasts CIs.csv", row.names = FALSE)
            
            
            
          #6 hour control
            mod.4e <- betareg(Surv.trans~Pop + Sex, data = subset(datum.surv, Hours == "control6"))
            summary(mod.4e)
            plot(mod.4e)
            emm.4e <- emmeans(mod.4e, ~Pop, mode = "response") #Create em_grid object on response scale (odds ratio)
            emm.4e #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
            
            emm.4e.contrasts <- contrast(emm.4e,"pairwise", type = "response") #Do pairwise contrasts between pops
            emm.4e.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
            confint(emm.4e.contrasts) #Show pairwise contrasts with confidence intervals
            cld(emm.4e) #Get group letter assignments for plotting
            
            write.csv(emm.4e.contrasts, file = "Survival comparison by pop control 6 hours contrasts.csv", row.names = FALSE)
            write.csv(confint(emm.4e.contrasts), file = "Survival comparison by pop control 6 hours contrasts CIs.csv", row.names = FALSE)
            
      
      
  #Run this before you make plots        
  #Relevel group variable to put in the desired order for plotting
          datum.surv$Pop<- factor(datum.surv$Pop,
                         levels = c("SD", "BR","BOB", "SH"))
      
      
      
      
  #Make boxplots for 6 hrs, 15 hrs, 24 hrs, and controls by population
      survival.pop.6 <- datum.surv |>
        full_join(datum.surv.pop.mean) |>
        filter(Hours == "6") |>
        ggplot(aes(x=Pop, y=Surv.prop, fill=Pop, data_id=Pop))+
        geom_boxplot_interactive(aes(
          tooltip = glue(
            '
        {levels(datum.surv$Pop)[Pop]}\n
        {n} Replicates\n
        Mean survival: {mean_surv}\n
        Range: {range}\n
        IQR: {iqr}
        '
          )))+
        geom_point_interactive()+
        stat_summary(fun=mean, geom="point", shape=15, size=4)+
        scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits = c(-0.05, 1))+
        scale_fill_manual("Groups:",values=my_pal2)+
        scale_color_manual(values=my_pal2)+
        theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
              axis.title.y = element_text(size = 14))+
        ggtitle(label = "6 hours")+
        annotate("text", 1, -0.05, label = "a", size = 5)+
        annotate("text", 2, -0.05, label = "a", size = 5)+
        annotate("text", 3, -0.05, label = "b", size = 5)+
        annotate("text", 4, -0.05, label = "ab", size = 5)
      survival.pop.6
      
      
      survival.pop.15 <- datum.surv |>
        full_join(datum.surv.pop.mean) |>
        filter(Hours == "15") |>
        ggplot(aes(x=Pop, y=Surv.prop, fill=Pop, data_id=Pop))+
        geom_boxplot_interactive(aes(
          tooltip = glue(
            '
        {levels(datum.surv$Pop)[Pop]}\n
        {n} Replicates\n
        Mean survival: {mean_surv}\n
        Range: {range}\n
        IQR: {iqr}
        '
          )))+
        geom_point_interactive()+
        stat_summary(fun=mean, geom="point", shape=15, size=4)+
        scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits = c(-0.05, 1))+
        scale_fill_manual("Groups:",values=my_pal2)+
        scale_color_manual(values=my_pal2)+
        theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank())+
        ggtitle(label = "15 hours")+
        annotate("text", 1, -0.05, label = "a", size = 5)+
        annotate("text", 2, -0.05, label = "b", size = 5)+
        annotate("text", 3, -0.05, label = "a", size = 5)+
        annotate("text", 4, -0.05, label = "a", size = 5)
      survival.pop.15
      
      
      survival.pop.24 <- datum.surv |>
        full_join(datum.surv.pop.mean) |>
        filter(Hours == "24") |>
        ggplot(aes(x=Pop, y=Surv.prop, fill=Pop, data_id=Pop))+
        geom_boxplot_interactive(aes(
          tooltip = glue(
            '
        {levels(datum.surv$Pop)[Pop]}\n
        {n} Replicates\n
        Mean survival: {mean_surv}\n
        Range: {range}\n
        IQR: {iqr}
        '
          )))+
        geom_point_interactive()+
        stat_summary(fun=mean, geom="point", shape=15, size=4)+
        scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
        scale_fill_manual("Groups:",values=my_pal2)+
        scale_color_manual(values=my_pal2)+
        theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank())+
        ggtitle(label = "24 hours")+
        annotate("text", 1, -0.05, label = "ab", size = 5)+
        annotate("text", 2, -0.05, label = "a", size = 5)+
        annotate("text", 3, -0.05, label = "b", size = 5)+
        annotate("text", 4, -0.05, label = "b", size = 5)
      survival.pop.24
      
      
      survival.pop.control.24 <- datum.surv |>
        full_join(datum.surv.pop.mean) |>
        filter(Hours == "control") |>
        ggplot(aes(x=Pop, y=Surv.prop, fill=Pop, data_id=Pop))+
        geom_boxplot_interactive(aes(
          tooltip = glue(
            '
        {levels(datum.surv$Pop)[Pop]}\n
        {n} Replicates\n
        Mean survival: {mean_surv}\n
        Range: {range}\n
        IQR: {iqr}
        '
          )))+
        geom_point_interactive()+
        stat_summary(fun=mean, geom="point", shape=15, size=4)+
        scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
        scale_fill_manual("Groups:",values=my_pal2)+
        scale_color_manual(values=my_pal2)+
        theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(),
              axis.title.y = element_text(size = 8), title = element_text(size=11))+
        ggtitle(label = "Control - 24 hours")+
        annotate("text", 1, -0.05, label = "a", size = 5)+
        annotate("text", 2, -0.05, label = "a", size = 5)+
        annotate("text", 3, -0.05, label = "a", size = 5)+
        annotate("text", 4, -0.05, label = "a", size = 5)
      survival.pop.control.24
      
      
      survival.pop.control.6 <- datum.surv |>
        full_join(datum.surv.pop.mean) |>
        filter(Hours == "control6") |>
        ggplot(aes(x=Pop, y=Surv.prop, fill=Pop, data_id=Pop))+
        geom_boxplot_interactive(aes(
          tooltip = glue(
            '
        {levels(datum.surv$Pop)[Pop]}\n
        {n} Replicates\n
        Mean survival: {mean_surv}\n
        Range: {range}\n
        IQR: {iqr}
        '
          )))+
        geom_point_interactive()+
        stat_summary(fun=mean, geom="point", shape=15, size=4)+
        scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
        scale_fill_manual("Groups:",values=my_pal2)+
        scale_color_manual(values=my_pal2)+
        theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
              title = element_text(size=11),
              axis.title.y = element_text(size = 12))+
        ggtitle(label = "Control - 6 hours")+
        annotate("text", 1, -0.05, label = "a", size = 5)+
        annotate("text", 2, -0.05, label = "b", size = 5)+
        annotate("text", 3, -0.05, label = "a", size = 5)+
        annotate("text", 4, -0.05, label = "c", size = 5)
      survival.pop.control.6
      
      #export figures as pngs
      png(filename = "KO and Survival by pop.png", width = 11, height = 5, units = "in", res = 300)
      KO.by.pop + (survival.pop.6 + survival.pop.15 + survival.pop.24)+
        plot_layout(widths = c(0.3, 0.95))+
        plot_annotation(tag_levels = 'A')
      dev.off()
      
      tiff(filename = "KO and Survival by pop.tiff", width = 11, height = 5, units = "in", res = 300)
      KO.by.pop + (survival.pop.6 + survival.pop.15 + survival.pop.24)+
        plot_layout(widths = c(0.3, 0.95))+
        plot_annotation(tag_levels = 'A')
      dev.off()
      
      png(filename = "KO and Survival by pop controls.png", width = 9, height = 4, units = "in", res = 300)
      KO.by.pop.control + (survival.pop.control.6 + survival.pop.control.24)+ 
        plot_layout(widths = c(0.4, 0.95))+
        plot_annotation(tag_levels = 'A')
      dev.off()
      
    #Make interactive figure for experimental trials
      ggiraph(
        ggobj = KO.by.pop + ((survival.pop.6 + survival.pop.15 + survival.pop.24)/(survival.pop.control.24+survival.pop.control.6))+ 
          plot_layout(widths = c(0.6, 0.95))+
          plot_annotation(tag_levels = 'A'),
        options = list(
          opts_hover_inv(css = "opacity:0.1;"),
          opts_tooltip(offx = 0, offy = 0, css = 'font-size: larger;')
        ),
        hover_css = "",
        height_svg = 9,
        width_svg = 16
      )
      
    
      
      
  #Sex
      #Is there a significant difference between males and females? 
        #6 hours
          mod.5 <- betareg(Surv.trans~Sex + Pop, data = subset(datum.surv, Hours == "6"))
          summary(mod.5)
          plot(mod.5)
          
          emm.5 <- emmeans(mod.5, ~Sex, mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.5 #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.5.contrasts <- contrast(emm.5,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.5.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.5.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.5) #Get group letter assignments for plotting
          
          write.csv(emm.5.contrasts, file = "Survival comparison by sex 6 hours contrasts.csv", row.names = FALSE)
          write.csv(confint(emm.5.contrasts), file = "Survival comparison by sex 6 hours contrasts CIs.csv", row.names = FALSE)
          
          
      
      
        #15 hours
          mod.5b <- betareg(Surv.trans~Sex + Pop, data = subset(datum.surv, Hours == "15"))
          summary(mod.5b)
          plot(mod.5b)
          
          emm.5b <- emmeans(mod.5b, ~Sex, mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.5b #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.5b.contrasts <- contrast(emm.5b,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.5b.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.5b.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.5b) #Get group letter assignments for plotting
          
          write.csv(emm.5b.contrasts, file = "Survival comparison by sex 15 hours contrasts.csv", row.names = FALSE)
          write.csv(confint(emm.5b.contrasts), file = "Survival comparison by sex 15 hours contrasts CIs.csv", row.names = FALSE)
          
      
        #24 hours
          mod.5c <- betareg(Surv.trans~Sex + Pop, data = subset(datum.surv, Hours == "24"))
          summary(mod.5c)
          plot(mod.5c)
          
          emm.5c <- emmeans(mod.5c, ~Sex, mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.5c #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.5c.contrasts <- contrast(emm.5c,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.5c.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.5c.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.5c) #Get group letter assignments for plotting
          
          write.csv(emm.5c.contrasts, file = "Survival comparison by sex 24 hours contrasts.csv", row.names = FALSE)
          write.csv(confint(emm.5c.contrasts), file = "Survival comparison by sex 24 hours contrasts CIs.csv", row.names = FALSE)
          
          
          
        #24 hour control 
          mod.5d <- betareg(Surv.trans~Sex + Pop, data = subset(datum.surv, Hours == "control"))
          summary(mod.5d)
          plot(mod.5d)
          
          emm.5d <- emmeans(mod.5d, ~Sex, mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.5d #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.5d.contrasts <- contrast(emm.5d,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.5d.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.5d.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.5d) #Get group letter assignments for plotting
          
          write.csv(emm.5d.contrasts, file = "Survival comparison by sex control 24 hours contrasts.csv", row.names = FALSE)
          write.csv(confint(emm.5d.contrasts), file = "Survival comparison by sex control 24 hours contrasts CIs.csv", row.names = FALSE)
          
      
      
        #6 hour control
          mod.5e <- betareg(Surv.trans~Sex + Pop, data = subset(datum.surv, Hours == "control6"))
          summary(mod.5e)
          plot(mod.5e)
          
          emm.5e <- emmeans(mod.5e, ~Sex, mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.5e #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.5e.contrasts <- contrast(emm.5e,"pairwise", type = "response") #Do pairwise contrasts between pops
          emm.5e.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.5e.contrasts) #Show pairwise contrasts with confidence intervals
          cld(emm.5e) #Get group letter assignments for plotting
          
          write.csv(emm.5e.contrasts, file = "Survival comparison by sex control 6 hours contrasts.csv", row.names = FALSE)
          write.csv(confint(emm.5e.contrasts), file = "Survival comparison by sex control 6 hours contrasts CIs.csv", row.names = FALSE)
          
      
      
      #Make boxplots for 6 hrs, 15 hrs, 24 hrs, and controls by sex
          survival.sex.6 <- datum.surv |>
            full_join(datum.surv.sex.mean) |>
            filter(Hours == "6") |>
            ggplot(aes(x=Sex, y=Surv.prop, fill=Sex, data_id=Sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Sex)[Sex]}\n
            {n} Replicates\n
            Mean survival: {mean_surv}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Sexes:",values=my_pal3)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
                  axis.title.y = element_text(size = 14))+
            ggtitle(label = "6 hours")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "a", size = 5)
          survival.sex.6
          
          
          survival.sex.15 <- datum.surv |>
            full_join(datum.surv.sex.mean) |>
            filter(Hours == "15") |>
            ggplot(aes(x=Sex, y=Surv.prop, fill=Sex, data_id=Sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Sex)[Sex]}\n
            {n} Replicates\n
            Mean survival: {mean_surv}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Sexes:",values=my_pal3)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank())+
            ggtitle(label = "15 hours")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "b", size = 5)
          survival.sex.15
          
          
          survival.sex.24 <- datum.surv |>
            full_join(datum.surv.sex.mean) |>
            filter(Hours == "24") |>
            ggplot(aes(x=Sex, y=Surv.prop, fill=Sex, data_id=Sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Sex)[Sex]}\n
            {n} Replicates\n
            Mean survival: {mean_surv}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Sexes:",values=my_pal3)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank())+
            ggtitle(label = "24 hours")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "b", size = 5)
          survival.sex.24
          
          
          survival.sex.control.24hr <- datum.surv |>
            full_join(datum.surv.sex.mean) |>
            filter(Hours == "control") |>
            ggplot(aes(x=Sex, y=Surv.prop, fill=Sex, data_id=Sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Sex)[Sex]}\n
            {n} Replicates\n
            Mean survival: {mean_surv}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Sexes:",values=my_pal3)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
                  title = element_text(size=11), axis.title.y = element_text(size = 10))+
            ggtitle(label = "Control - 24 hours")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "a", size = 5)
          survival.sex.control.24hr
          
          
          survival.sex.control.6hr <- datum.surv |>
            full_join(datum.surv.sex.mean) |>
            filter(Hours == "control6") |>
            ggplot(aes(x=Sex, y=Surv.prop, fill=Sex, data_id=Sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Sex)[Sex]}\n
            {n} Replicates\n
            Mean survival: {mean_surv}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Sexes:",values=my_pal3)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(),
                  axis.title.y = element_text(size = 10), title = element_text(size=11))+
            ggtitle(label = "Control - 6 hours")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "b", size = 5)
          survival.sex.control.6hr
      
          
      #Export figures as png
        png(filename = "KO and Survival by sex.png", width = 11, height = 5, units = "in", res = 300)
        KO.by.sex + (survival.sex.6 + survival.sex.15 + survival.sex.24) + 
          plot_layout(widths = c(0.3, 0.95))+
          plot_annotation(tag_levels = 'A')
        dev.off()
        
        tiff(filename = "KO and Survival by sex.tiff", width = 11, height = 5, units = "in", res = 300)
        KO.by.sex + (survival.sex.6 + survival.sex.15 + survival.sex.24) + 
          plot_layout(widths = c(0.3, 0.95))+
          plot_annotation(tag_levels = 'A')
        dev.off()
        
        png(filename = "KO and Survival by sex controls.png", width = 9, height = 4, units = "in", res = 300)
        KO.by.sex.control + (survival.sex.control.6hr + survival.sex.control.24hr) + 
          plot_layout(widths = c(0.4, 0.95))+
          plot_annotation(tag_levels = 'A')
        dev.off()
        
      
  
      #Make interactive figure 
        ggiraph(
          ggobj = KO.by.sex + (survival.sex.6 + survival.sex.15 + survival.sex.24) + plot_layout(widths = c(0.6, 0.95)),
          options = list(
            opts_hover_inv(css = "opacity:0.1;"),
            opts_tooltip(offx = 0, offy = 0, css = 'font-size: larger;')
          ),
          hover_css = "",
          height_svg = 9,
          width_svg = 16
        )
      
      
    #Population and sex
          #Is there a significant difference between males and females of each population? 
        #6 hours
        mod.6 <- betareg(Surv.trans~Pop.sex, data = subset(datum.surv, Hours == "6"))
        summary(mod.6)
        plot(mod.6)
        
        emm.6 <- emmeans(mod.6, ~Pop.sex, mode = "response") #Create em_grid object on response scale (odds ratio)
        emm.6 #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
        
        emm.6.contrasts <- contrast(emm.6,"pairwise", type = "response") #Do pairwise contrasts between pops
        emm.6.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
        confint(emm.6.contrasts) #Show pairwise contrasts with confidence intervals
        cld(emm.6) #Get group letter assignments for plotting
        
        write.csv(emm.6.contrasts, file = "Survival comparison by Popsex 6 hours contrasts.csv", row.names = FALSE)
        write.csv(confint(emm.6.contrasts), file = "Survival comparison by Popsex 6 hours contrasts CIs.csv", row.names = FALSE)
        
        
        
        #15 hours
        mod.6b <- betareg(Surv.trans~Pop.sex, data = subset(datum.surv, Hours == "15"))
        summary(mod.6b)
        plot(mod.6b)
        
        emm.6b <- emmeans(mod.6b, ~Pop.sex, mode = "response") #Create em_grid object on response scale (odds ratio)
        emm.6b #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
        
        emm.6b.contrasts <- contrast(emm.6b,"pairwise", type = "response") #Do pairwise contrasts between pops
        emm.6b.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
        confint(emm.6b.contrasts) #Show pairwise contrasts with confidence intervals
        cld(emm.6b) #Get group letter assignments for plotting
        
        write.csv(emm.6b.contrasts, file = "Survival comparison by Popsex 15 hours contrasts.csv", row.names = FALSE)
        write.csv(confint(emm.6b.contrasts), file = "Survival comparison by Popsex 15 hours contrasts CIs.csv", row.names = FALSE)
        
        
        #24 hours
        mod.6c <- betareg(Surv.trans~Pop.sex, data = subset(datum.surv, Hours == "24"))
        summary(mod.6c)
        plot(mod.6c)
        
        emm.6c <- emmeans(mod.6c, ~Pop.sex, mode = "response") #Create em_grid object on response scale (odds ratio)
        emm.6c #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
        
        emm.6c.contrasts <- contrast(emm.6c,"pairwise", type = "response") #Do pairwise contrasts between pops
        emm.6c.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
        confint(emm.6c.contrasts) #Show pairwise contrasts with confidence intervals
        cld(emm.6c) #Get group letter assignments for plotting
        
        write.csv(emm.6c.contrasts, file = "Survival comparison by Popsex 24 hours contrasts.csv", row.names = FALSE)
        write.csv(confint(emm.6c.contrasts), file = "Survival comparison by Popsex 24 hours contrasts CIs.csv", row.names = FALSE)
        
        
        #No stats on controls at this level because one replicate each
      
          
        #Make boxplots for 6 hrs, 15 hrs, 24 hrs, and controls by pop.sex
          survival.popsex.6 <- datum.surv |>
            full_join(datum.surv.popsex.mean) |>
            filter(Hours == "6") |>
            ggplot(aes(x=Pop.sex, y=Surv.prop, fill=Pop.sex, data_id=Pop.sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
                {levels(datum.surv$Pop.sex)[Pop.sex]}\n
                {n} Replicates\n
                Mean survival: {mean_surv}\n
                Range: {range}\n
                IQR: {iqr}
                '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=2.5)+
            scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Groups:",values=my_pal)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1), 
                  axis.title.y = element_text(size = 12))+
            ggtitle(label = "6 hours")+
            annotate("text", 1, -0.05, label = "a", size = 4)+
            annotate("text", 2, -0.05, label = "a", size = 4)+
            annotate("text", 3, -0.05, label = "a", size = 4)+
            annotate("text", 4, -0.05, label = "a", size = 4)+
            annotate("text", 5, -0.05, label = "a", size = 4)+
            annotate("text", 6, -0.05, label = "a", size = 4)+
            annotate("text", 7, -0.05, label = "a", size = 4)+
            annotate("text", 8, -0.05, label = "a", size = 4)
          survival.popsex.6
          
          
          survival.popsex.15 <- datum.surv |>
            full_join(datum.surv.popsex.mean) |>
            filter(Hours == "15") |>
            ggplot(aes(x=Pop.sex, y=Surv.prop, fill=Pop.sex, data_id=Pop.sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
                {levels(datum.surv$Pop.sex)[Pop.sex]}\n
                {n} Replicates\n
                Mean survival: {mean_surv}\n
                Range: {range}\n
                IQR: {iqr}
                '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=2.5)+
            scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Groups:",values=my_pal)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1))+
            ggtitle(label = "15 hours")+
            annotate("text", 1, -0.05, label = "a", size = 4)+
            annotate("text", 2, -0.05, label = "a", size = 4)+
            annotate("text", 3, -0.05, label = "b", size = 4)+
            annotate("text", 4, -0.05, label = "a", size = 4)+
            annotate("text", 5, -0.05, label = "a", size = 4)+
            annotate("text", 6, -0.05, label = "a", size = 4)+
            annotate("text", 7, -0.05, label = "a", size = 4)+
            annotate("text", 8, -0.05, label = "a", size = 4)
          survival.popsex.15
          
          
          survival.popsex.24 <- datum.surv |>
            full_join(datum.surv.popsex.mean) |>
            filter(Hours == "24") |>
            ggplot(aes(x=Pop.sex, y=Surv.prop, fill=Pop.sex, data_id=Pop.sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
                {levels(datum.surv$Pop.sex)[Pop.sex]}\n
                {n} Replicates\n
                Mean survival: {mean_surv}\n
                Range: {range}\n
                IQR: {iqr}
                '
              )))+
            geom_point_interactive()+
            stat_summary(fun=mean, geom="point", shape=15, size=2.5)+
            scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_fill_manual("Groups:",values=my_pal)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1))+
            ggtitle(label = "24 hours")+
            annotate("text", 1, -0.05, label = "ab", size = 4)+
            annotate("text", 2, -0.05, label = "a", size = 4)+
            annotate("text", 3, -0.05, label = "b", size = 4)+
            annotate("text", 4, -0.05, label = "a", size = 4)+
            annotate("text", 5, -0.05, label = "a", size = 4)+
            annotate("text", 6, -0.05, label = "a", size = 4)+
            annotate("text", 7, -0.05, label = "a", size = 4)+
            annotate("text", 8, -0.05, label = "a", size = 4)
          survival.popsex.24
          
          
          survival.popsex.control.24 <- datum.surv |>
            full_join(datum.surv.popsex.mean) |>
            filter(Hours == "control") |>
            ggplot(aes(x=Pop.sex, y=Surv.prop, col=Pop.sex, data_id=Pop.sex))+
            geom_point_interactive(aes(
              tooltip = glue(
                '
                {levels(datum.surv$Pop.sex)[Pop.sex]}\n
                {n} Replicates\n
                Mean survival: {mean_surv}\n
                Range: {range}\n
                IQR: {iqr}
                '
              )), size =4.5, shape=16)+
            scale_y_continuous(name="", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_color_manual("Groups:",values=my_pal)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
                  axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1))+
            ggtitle(label = "Control - 24 hours")
          survival.popsex.control.24
          
          
          survival.popsex.control.6 <- datum.surv |>
            full_join(datum.surv.popsex.mean) |>
            filter(Hours == "control6") |>
            ggplot(aes(x=Pop.sex, y=Surv.prop, col=Pop.sex, data_id=Pop.sex))+
            geom_point_interactive(aes(
              tooltip = glue(
                '
                {levels(datum.surv$Pop.sex)[Pop.sex]}\n
                {n} Replicates\n
                Mean survival: {mean_surv}\n
                Range: {range}\n
                IQR: {iqr}
                '
              )), size =4.5, shape=16)+
            scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits=c(-0.05,1))+
            scale_color_manual("Groups:",values=my_pal)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
                  axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1), axis.title.y = element_text(size=12))+
            ggtitle(label = "Control - 6 hours")
          survival.popsex.control.6
          
          
          #Export figure as png
          png(filename = "KO and Survival by pop and sex.png", width = 11, height = 5, units = "in", res = 300)
          KO.by.popsex + (survival.popsex.6 + survival.popsex.15 + survival.popsex.24) + 
            plot_layout(widths = c(0.4, 1)) +
            plot_annotation(tag_levels = 'A')
          dev.off()
          
          tiff(filename = "KO and Survival by pop and sex.tiff", width = 11, height = 5, units = "in", res = 300)
          KO.by.popsex + (survival.popsex.6 + survival.popsex.15 + survival.popsex.24) + 
            plot_layout(widths = c(0.4, 1)) +
            plot_annotation(tag_levels = 'A')
          dev.off()
        
          
          #/ (survival.popsex.control + survival.popsex.control)
          
          #Make interactive figure 
          ggiraph(
            ggobj = KO.by.popsex + (survival.popsex.6 + survival.popsex.15 + survival.popsex.24) + plot_layout(widths = c(0.5, 1)),
            options = list(
              opts_hover_inv(css = "opacity:0.1;"),
              opts_tooltip(offx = 0, offy = 0, css = 'font-size: larger;')
            ),
            hover_css = "",
            height_svg = 9,
            width_svg = 16
          )
      
    

          
      ##Experimental vs control survival
          mod.control.vs.exp.surv <- betareg(Surv.trans ~ Group, data = datum.surv)
          summary(mod.control.vs.exp.surv)
          plot(mod.control.vs.exp.surv)
          
          emm.exp.con.surv <- emmeans(mod.control.vs.exp.surv, ~Group,  mode = "response") #Create em_grid object on response scale (odds ratio)
          emm.exp.con.surv #Time.trend is the increase in the proportion of copepods with LOE per 1 hour increase
          
          emm.exp.con.surv.contrasts <- contrast(emm.exp.con.surv, "pairwise", type = "response") #Do pairwise contrasts between pops
          emm.exp.con.surv.contrasts #Estimate is the increase/decrease in the proportion of copepods with LOE between groups per hour increase
          confint(emm.exp.con.surv.contrasts)
          cld(emm.exp.con.surv)
          
          
          write.csv(emm.exp.con.surv.contrasts, file = "Survival comparison controls vs experimental contrasts.csv", row.names = FALSE)
          write.csv(confint(emm.exp.con.surv.contrasts), file = "Survival comparison controls vs experimental contrasts CIs.csv", row.names = FALSE)
          
          
          
          #Make plot comparing experimental vs control LOE proportion after 6 hours anoxia exposure
          exp.vs.control.surv <- datum.surv |>
            ggplot(aes(x=Group, y=Surv.prop, fill=Group))+
            geom_boxplot()+
            geom_point()+
            stat_summary(fun=mean, geom="point", shape=15, size=4)+
            scale_y_continuous(name="Proportion of surviving copepods", breaks = seq(0, 1, by = 0.1), limits = c(-0.05, 1))+
            scale_fill_manual("Groups:",values=my_pal5)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(), 
                  axis.title.y = element_text(size = 14), title = element_text(size = 13), 
                  axis.text.x = element_text(size=11, angle=45, vjust=1, hjust=1))+
            ggtitle(label = "Survival")+
            annotate("text", 1, -0.05, label = "a", size = 5)+
            annotate("text", 2, -0.05, label = "a", size = 5)+
            annotate("text", 3, -0.05, label = "a", size = 5)+
            annotate("text", 4, -0.05, label = "b", size = 5)+
            annotate("text", 5, -0.05, label = "b", size = 5)
            
          exp.vs.control.surv
          
          
    ####Export figure with exp vs control comparisons for both LOE and survival
          png(filename = "Control vs Experimental Summary Figure.png", width = 8, height = 5, units = "in", res = 300)
          
          exp.vs.control.LOE + exp.vs.control.surv +
            plot_layout(widths = c(0.4, 1)) +
            plot_annotation(tag_levels = 'A')
            
          dev.off()
          
          tiff(filename = "Control vs Experimental Summary Figure.tiff", width = 8, height = 5, units = "in", res = 300)
          
          exp.vs.control.LOE + exp.vs.control.surv +
            plot_layout(widths = c(0.4, 1)) +
            plot_annotation(tag_levels = 'A')
          
          dev.off()
          
        
          
 
#### Time until anoxia among populations and sexes ####
      #Analyze differences among groups and check for effect of rate at which groups reach anoxia on LOE 
      #Use datum.surv data frame since it has the KO test data within it (the 6 hours grouping).
      
      #Differences among groups 
      #Sex
          mod.7 <- lmer(Time_minutes~Sex + (1|Pop), data = datum.surv)
          summary(mod.7)
          plot(mod.7)
          emmeans(mod.7, pairwise~Sex)
          #Export data
          comp.7 <- emmeans(mod.7, pairwise~Sex)
          write.csv(comp.7$contrasts, file = "Time until anoxia by sex contrasts.csv", row.names = FALSE)
          write.csv(confint(comp.7$contrasts), file = "Time until anoxia by sex contrasts CIs.csv", row.names = FALSE)
          
        #Make boxplot
          tua.sex <- datum.surv |>
            full_join(datum.tua.sex.mean) |>
            filter(Hours != "control") |>
            ggplot(aes(x=Sex, y=Time_minutes, fill=Sex, data_id=Sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Sex)[Sex]}\n
            {n} Replicates\n
            Mean time: {mean_tua}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            geom_point_interactive(aes(x=Sex, y=mean_tua), color='black', shape=15, size=4, show.legend = F)+
            scale_y_continuous(name="Minutes", breaks = seq(0, 350, by = 25), limits = c(0, 350))+
            scale_fill_manual("Sexes:",values=my_pal3)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank())+
            ggtitle(label = "Time until anoxia")+
            annotate("text", 1, 1, label = "a", size = 5)+
            annotate("text", 2, 1, label = "a", size = 5)
            
          tua.sex
      
      
      #Population
          mod.8 <- lmer(Time_minutes~Pop + (1|Sex), data = datum.surv)
          summary(mod.8)
          plot(mod.8)
          emmeans(mod.8, pairwise~Pop)
          #Export data
          comp.8 <- emmeans(mod.8, pairwise~Pop)
          cld(comp.8)
          write.csv(comp.8$contrasts, file = "Time until anoxia by Pop contrasts.csv", row.names = FALSE)
          write.csv(confint(comp.8$contrasts), file = "Time until anoxia by Pop contrasts CIs.csv", row.names = FALSE)
          
          
          #Relevel group variable to put in the desired order for plotting
          datum.surv$Pop<- factor(datum.surv$Pop,
                             levels = c("SD", "BR","BOB", "SH"))
        #Make boxplot
          tua.pop <- datum.surv |>
            full_join(datum.tua.pop.mean) |>
            filter(Hours != "control") |>
            ggplot(aes(x=Pop, y=Time_minutes, fill=Pop, data_id=Pop))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Pop)[Pop]}\n
            {n} Replicates\n
            Mean time: {mean_tua}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            geom_point_interactive(aes(x=Pop, y=mean_tua), color='black', shape=15, size=4, show.legend = F)+
            scale_y_continuous(name="", breaks = seq(0, 350, by = 25), limits = c(0, 350))+
            scale_fill_manual("Populations:",values=my_pal2)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank())+
            ggtitle(label = "")+
            annotate("text", 1, 1, label = "a", size = 5)+
            annotate("text", 2, 1, label = "a", size = 5)+
            annotate("text", 3, 1, label = "a", size = 5)+
            annotate("text", 4, 1, label = "a", size = 5)
          tua.pop
      
      
      #Population and sex
          mod.9 <- lm(Time_minutes~Pop.sex, data = datum.surv)
          summary(mod.9)
          plot(mod.9)
          emmeans(mod.9, pairwise~Pop.sex)
          #Export data
          comp.9 <- emmeans(mod.9, pairwise~Pop.sex)
          cld(comp.9)
          write.csv(comp.9$contrasts, file = "Time until anoxia by Pop and sex contrasts.csv", row.names = FALSE)
          write.csv(confint(comp.9$contrasts), file = "Time until anoxia by Pop and sex contrasts CIs.csv", row.names = FALSE)
          
          
          #Make boxplot
          #Relevel group variable to put in the desired order for plotting
          datum.surv$Pop.sex<- factor(datum.surv$Pop.sex,
                                 levels = c("Female SD", "Male SD","Female BR", "Male BR",
                                            "Female BOB", "Male BOB", "Female SH", "Male SH"))
          #Make boxplot
          tua.popsex <- datum.surv |>
            full_join(datum.tua.popsex.mean) |>
            filter(Hours != "control") |>
            ggplot(aes(x=Pop.sex, y=Time_minutes, fill=Pop.sex, data_id=Pop.sex))+
            geom_boxplot_interactive(aes(
              tooltip = glue(
                '
            {levels(datum.surv$Pop.sex)[Pop.sex]}\n
            {n} Replicates\n
            Mean time: {mean_tua}\n
            Range: {range}\n
            IQR: {iqr}
            '
              )))+
            geom_point_interactive()+
            geom_point_interactive(aes(x=Pop.sex, y=mean_tua), shape=15, size=4, show.legend = F)+
            scale_y_continuous(name="", breaks = seq(0, 350, by = 25), limits = c(0, 350))+
            scale_fill_manual("Groups:",values=my_pal)+
            theme(legend.position = "none", legend.direction = "horizontal", axis.title.x.bottom = element_blank(),
                  axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
            ggtitle(label = "")+
            annotate("text", 1, 1, label = "a", size = 5)+
            annotate("text", 2, 1, label = "a", size = 5)+
            annotate("text", 3, 1, label = "a", size = 5)+
            annotate("text", 4, 1, label = "a", size = 5)+
            annotate("text", 5, 1, label = "a", size = 5)+
            annotate("text", 6, 1, label = "a", size = 5)+
            annotate("text", 7, 1, label = "a", size = 5)+
            annotate("text", 8, 1, label = "a", size = 5)
          tua.popsex
      
      
      ##Combine plots into one figure
          #Export figure as png
          png(filename = "Time until anoxia among pops.png", width = 10, height = 6, units = "in", res = 300)
          ggarrange(tua.sex, tua.pop, tua.popsex,
                    nrow = 1,ncol = 3,
                    labels = c("A", "B", "C")
          ) 
          dev.off()
      
####  Effect of rate to anoxia on LOE?   ####
          
          
      #Does rate at which copepods reached anoxia change the effect of time on LOE? 
      #poisson distribution link function used because it's count data in the response variable
      mod.10 <- betareg(LOE.trans~Time*Time.minutes, data=datum.KO2)
      summary(mod.10)
      plot(mod.10)
      # Coefficients (mean model with logit link):
      #                     Estimate Std. Error z value Pr(>|z|)    
      # (Intercept)       -1.767929   0.470900  -3.754 0.000174 ***
      #   Time             0.087793   0.132001   0.665 0.505989    
      # Time.minutes       0.002185   0.003633   0.601 0.547585    
      # Time:Time.minutes  0.001757   0.001023   1.717 0.086054 .
      
      exp(c(0.001757, 0.001023)) #To get effect of time until plateau on time's effect on LOE (...whew!)
      confint(mod.10)
      exp(c(-0.0002490821, 0.003762805)) #To get the confidence interval around effect.
      
      
      #According to numbers above, effect of anoxia on LOE increases by 0.18% per extra minute it takes a group to reach anoxia, 
      #but this effect is not statistically significant
    
      #Run this before making plots so pops are in correct order
      datum.KO$Pop<- factor(datum.KO$Pop,
                             levels = c("SD", "BR","BOB", "SH"))
      
      
####Does time until anoxia affect survival? ####
      
    #At 6 hours
      #Without controlling for pop differences
      mod.11 <- betareg(Surv.trans~Time_minutes, data = subset(datum.surv, Hours == "6"))
      summary(mod.11)
      plot(mod.11)
      
      exp(c(-0.004836, 0.004551)) #To get effect of time until anoxia on survival plus SE
      confint(mod.11)
      exp(c(-0.0137551, 0.004083485)) #To get the confidence interval around effect. 
      
      
    #At 15 hours
      #Without controlling for pop differences
      mod.12 <- betareg(Surv.trans~Time_minutes, data = subset(datum.surv, Hours == "15"))
      summary(mod.12)
      plot(mod.12)
      
      exp(c(-0.004562, 0.003713)) #To get effect of time until anoxia on survival plus SE
      confint(mod.12)
      exp(c(-0.01183976, 0.002715262)) #To get the confidence interval around effect.
      
      
    #At 24 hours
      #Without controlling for pop differences
      mod.13 <- betareg(Surv.trans~Time_minutes, data = subset(datum.surv, Hours == "24"))
      summary(mod.13)
      plot(mod.13)
      
      exp(c(-0.001678, 0.002686)) #To get effect of time until anoxia on survival plus SE
      confint(mod.13)
      exp(c(-0.006942897, 0.003586315)) #To get the confidence interval around effect.
      
      
  