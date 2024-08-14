#Comparison of resp statistics from populations
#Matthew Powers 
#powerm3@oregonstate.edu
#Statistics and figure creation
#Last edited 06-27-24
#Tested on R v.4.3.3 "Angel Food Cake"

#load required packages
library(devtools)
library(MESS) #data wrangling and modeling
library(stats) #data wrangling and modeling
library(respirometry) #pcrit, alpha, nlr
library(ggpubr) #Also loads basic ggplot2
library(cowplot) #Pretty ggplots!
library(reshape2) #Data wrangling
library(dplyr) #Data wrangling
library(tidyverse) #Data wrangling
library(stringr) #data wrangling for splitting column characters
library(MetBrewer) #Pretty art colors!
library(MexBrewer) #Mexican art pretty colors!
library(palettetown) #Pokemon pretty colors!
library(runner) #Do functions on sliding windows of data
library(scales) #Preview color palettes
library(lme4) #statistical analysis
library(lmerTest) #Statistical analysis
library(agricolae) #data wrangling and modeling
library(emmeans) #statistical analysis
library(RColorBrewer) #plotting
library(car) #data wrangling and modeling
library(sf) #data wrangling and modeling
library(MuMIn) #For getting R squared values from mixed models
library(plotrix) #data wrangling and modeling
library(ggridges) #plotting ridgelines
library(HH) #data wrangling and modeling
library(grid) #for adding tables to plots and making multiple panels
library(gridExtra) #for adding tables to plots and making multiple panels
library(glue) #Stick stuff in other stuff! Like pasting text in data frames and plot elements
library(patchwork) #stitching graphs together
library(ggiraph) #Connected interactive graphs
library(multcomp) #automatically assign pairwise significance groupings
library(multcompView) #See groupings from multcomp

#Set theme globally
  theme_set(theme_cowplot())

#Read in data 
  datum <- read.csv(file = "Pop resp statistics for R 06-10-24.csv", header = TRUE)

  datum <- subset(datum, notes != "bubbles") #Remove wells with bubbles
  datum <- subset(datum, notes != "crushed") #Remove wells with crushed individuals

  datum <- subset(datum, Plateau == "yes") #Remove wells with no plateau

  str(datum)  # check data coding

#Reassign data columns to proper coding (as factored categories)
  datum$Date <- as.factor(datum$Date)
  datum$Pop <- as.factor(datum$Pop)
  datum$Mom.ID <- as.factor(datum$Mom.ID)
  datum$Sex <- as.factor(datum$Sex)
  datum$Plate <- as.factor(datum$Plate)
  datum$Down <- as.factor(datum$Down)
  datum$Surv <- as.factor(datum$Surv)
  
  str(datum) #recheck data coding

#Relabel sex factor
  datum %>% 
    mutate(Sex = fct_recode(Sex, "Female" = "F",
                            "Male" = "M")) -> datum

#Combine sex and population factors for plotting or stats groupings
  datum$Pop.sex = paste(datum$Sex, datum$Pop)

#Combine Date and Plate factors for plotting or stats groupings
  datum$Date.plate = paste(datum$Date, datum$Plate)

#Relevel factor
  datum$Pop.sex <- factor(datum$Pop.sex, levels = c("Female SD", "Male SD","Female BR", "Male BR",
                                                    "Female BOB", "Male BOB", "Female SH", "Male SH")) 
  table(datum$Pop.sex) #Check factor level order
  
  datum$Pop <- factor(datum$Pop, levels = c("SD", "BR","BOB", "SH")) 
  
  table(datum$Pop) #Check factor level order
  
  table(datum$Pop.sex) #Check factor level order

##### Figure creation #####

#Color pallete tweaking for boxplots (optional)
  br_pal <- met.brewer("Signac")
  br_pal2 <- met.brewer("Renoir")
  my_pal <- c(br_pal2[c(8,6)], br_pal[c(3,1)], br_pal2[c(2,4, 12,11)])
  # just for displaying old and new palette - not required for solution
  show_col(br_pal)
  show_col(br_pal2)
  show_col(my_pal)

#Color pallete tweaking for boxplots (optional)
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


#Alpha statistic among populations

      #Statistic summary
        #Pop
        datum.alpha.mean.pop <- datum[datum$Alpha < 25,] |> 
          group_by(Pop) |> 
          summarise(mean = mean(Alpha) |> round(4),
                    sd = sd(Alpha) |> round(4),
                    n = n(),
                    iqr = IQR(Alpha),
                    range = paste(range(Alpha) |> round(4), collapse = ' - ')
          ) 
        
        #Sex
        datum.alpha.mean.sex <- datum[datum$Alpha < 25,] |> 
          group_by(Sex) |> 
          summarise(mean = mean(Alpha) |> round(4),
                    sd = sd(Alpha) |> round(4),
                    n = n(),
                    iqr = IQR(Alpha),
                    range = paste(range(Alpha) |> round(4), collapse = ' - ')
          ) 
        
        #Popsex
        datum.alpha.mean.popsex <- datum[datum$Alpha < 25,] |> 
          group_by(Pop.sex) |> 
          summarise(mean = mean(Alpha) |> round(4),
                    sd = sd(Alpha) |> round(4),
                    n = n(),
                    iqr = IQR(Alpha),
                    range = paste(range(Alpha) |> round(4), collapse = ' - ')
          ) 
  
  
        #ridgeline pop.sex
        Alphas.ridge.popsex <- ggplot(datum[datum$Alpha < 25,], aes(x=Alpha, y=Pop.sex, fill = Pop.sex)) +
          geom_density_ridges_gradient(scale=1, bandwidth = 0.004, rel_min_height =0.01,
                                       jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                       point_shape = "|", point_size = 4, point_color = "black",
                                       quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
          scale_x_continuous(breaks = seq(0,0.04, by =0.01), limits = c(0,0.04)) +
          scale_fill_manual("Groups:", values = my_pal)+
          theme_ridges(center_axis_labels = TRUE)+
          xlab(expression(paste("Alpha ", " (", "umol ", O[2]," / mm / hr / kPa)"))) +ylab("")+
          theme(legend.position = "none")+
          annotate("text", 0.04, 1.5, label = "a", size = 5)+
          annotate("text", 0.04, 2.5, label = "a", size = 5)+
          annotate("text", 0.04, 3.5, label = "a", size = 5)+
          annotate("text", 0.04, 4.5, label = "a", size = 5)+
          annotate("text", 0.04, 5.5, label = "a", size = 5)+
          annotate("text", 0.04, 6.5, label = "a", size = 5)+
          annotate("text", 0.04, 7.5, label = "a", size = 5)+
          annotate("text", 0.04, 8.5, label = "b", size = 5)
          
        Alphas.ridge.popsex
        
        #ridgeline pop
        Alphas.ridge.pop <- ggplot(datum[datum$Alpha < 25,], aes(x=Alpha, y=Pop, fill = Pop)) +
          geom_density_ridges_gradient(scale=1, bandwidth = 0.003, rel_min_height =0.01,
                                       jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                       point_shape = "|", point_size = 4, point_color = "black",
                                       quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
          scale_x_continuous(breaks = seq(0,0.04, by =0.01), limits = c(0,0.04)) +
          scale_fill_manual("Groups:", values = my_pal2)+
          theme_ridges(center_axis_labels = TRUE)+
          xlab(expression(paste("Alpha ", " (", "umol ", O[2]," / mm / hr / kPa)"))) +ylab("")+
          theme(legend.position = "none")+
          annotate("text", 0.04, 1.5, label = "a", size = 5)+
          annotate("text", 0.04, 2.5, label = "a", size = 5)+
          annotate("text", 0.04, 3.5, label = "a", size = 5)+
          annotate("text", 0.04, 4.5, label = "a", size = 5)
        
        Alphas.ridge.pop
        
        #ridgeline sex
        Alphas.ridge.sex <- ggplot(datum[datum$Alpha < 25,], aes(x=Alpha, y=Sex, fill = Sex)) +
          geom_density_ridges(scale=1, bandwidth = 0.002, rel_min_height =0.01,
                                       jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                       point_shape = "|", point_size = 4, point_color = "black",
                                       quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
          scale_x_continuous(breaks = seq(0,0.04, by =0.01), limits = c(0,0.04)) +
          scale_fill_manual("Groups:", values = my_pal3)+
          theme_ridges(center_axis_labels = TRUE)+
          xlab(expression(paste("Alpha ", " (", "umol ", O[2]," / mm / hr / kPa)"))) +ylab("")+
          theme(legend.position = "none")+
          annotate("text", 0.04, 1.5, label = "a", size = 5)+
          annotate("text", 0.04, 2.5, label = "a", size = 5)
        
        Alphas.ridge.sex
        
        #Combine plots
        png(file = "Alpha Combined.png", units = "in", width = 10, height = 5, res = 300)
        ggarrange(Alphas.ridge.sex, Alphas.ridge.pop, Alphas.ridge.popsex,
                  nrow = 1,ncol = 3,
                  labels = c("A", "B", "C"), label.x = 0, label.y = 1,
                  widths = c(1, 1, 1.5), heights = 1
        ) 
        dev.off()



#PCrit statistic among populations

        #Statistic summary
        #Pop
          datum.pcrit.mean.pop <- datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,] |> 
            group_by(Pop) |> 
            summarise(mean = mean(Breakpoint) |> round(2),
                      sd = sd(Breakpoint) |> round(2),
                      n = n(),
                      iqr = IQR(Breakpoint),
                      range = paste(range(Breakpoint) |> round(2), collapse = ' - ')
            ) 
          
          write.csv(datum.pcrit.mean, file = "Mean Pcrit across pops.csv", row.names = FALSE) #Save final frame as a csv
          
          #Sex
          datum.pcrit.mean.sex <- datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,] |> 
            group_by(Sex) |> 
            summarise(mean = mean(Breakpoint) |> round(2),
                      sd = sd(Breakpoint) |> round(2),
                      n = n(),
                      iqr = IQR(Breakpoint),
                      range = paste(range(Breakpoint) |> round(2), collapse = ' - ')
            ) 
          
          #Popsex
          datum.pcrit.mean.popsex <- datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,] |> 
            group_by(Pop.sex) |> 
            summarise(mean = mean(Breakpoint) |> round(2),
                      sd = sd(Breakpoint) |> round(2),
                      n = n(),
                      iqr = IQR(Breakpoint),
                      range = paste(range(Breakpoint) |> round(2), collapse = ' - ')
            ) 
          
          
          
          #ridgeline pop.sex
          pcrit.ridge.popsex <- ggplot(datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,], aes(x=Breakpoint, y=Pop.sex, fill = Pop.sex)) +
            geom_density_ridges(scale=3, bandwidth = 0.8, rel_min_height =0.01,
                                         jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                         point_shape = "|", point_size = 4, point_color = "black",
                                         quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(0,14, by =2), limits = c(0,14)) +
            scale_fill_manual("Groups:", values = my_pal)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Pcrit ", " (", "kPa ", O[2],")"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 14, 1.5, label = "a", size = 5)+
            annotate("text", 14, 2.5, label = "ab", size = 5)+
            annotate("text", 14, 3.5, label = "abc", size = 5)+
            annotate("text", 14, 4.5, label = "abc", size = 5)+
            annotate("text", 14, 5.5, label = "bcd", size = 5)+
            annotate("text", 14, 6.5, label = "cd", size = 5)+
            annotate("text", 14, 7.5, label = "cd", size = 5)+
            annotate("text", 14, 8.5, label = "d", size = 5)
          
          pcrit.ridge.popsex
          
          #ridgeline pop
          pcrit.ridge.pop <- ggplot(datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,], aes(x=Breakpoint, y=Pop, fill = Pop)) +
            geom_density_ridges(scale=2, bandwidth = 0.8, rel_min_height =0.01,
                                         jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                         point_shape = "|", point_size = 4, point_color = "black",
                                         quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(0,14, by =2), limits = c(0,14)) +
            scale_fill_manual("Groups:", values = my_pal2)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Pcrit ", " (", "kPa ", O[2],")"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 14, 1.5, label = "a", size = 5)+
            annotate("text", 14, 2.5, label = "b", size = 5)+
            annotate("text", 14, 3.5, label = "c", size = 5)+
            annotate("text", 14, 4.5, label = "c", size = 5)
          
          pcrit.ridge.pop
          
          #ridgeline sex
          pcrit.ridge.sex <- ggplot(datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,], aes(x=Breakpoint, y=Sex, fill = Sex)) +
            geom_density_ridges(scale=2, bandwidth = 0.6, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(0,14, by =2), limits = c(0,14)) +
            scale_fill_manual("Groups:", values = my_pal3)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Pcrit ", " (", "kPa ", O[2],")"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 14, 1.5, label = "a", size = 5)+
            annotate("text", 14, 2.5, label = "a", size = 5)
          
          pcrit.ridge.sex
          
          #Combine plots
          png(file = "PCrit Combined.png", units = "in", width = 10, height = 5, res = 300)
          ggarrange(pcrit.ridge.sex, pcrit.ridge.pop, pcrit.ridge.popsex,
                    nrow = 1,ncol = 3,
                    labels = c("A", "B", "C"), label.x = 0, label.y = 1,
                    widths = c(1, 1, 1.5), heights = 1
          ) 
          dev.off()



#NLR PCrit statistic among populations

# #boxplot pop.sex
# 
# NLR.popsex <- ggplot(datum[datum$NLR > 0 & datum$NLR < 25,], aes(x=Pop.sex, y=NLR, fill=Pop.sex))+
#   geom_boxplot()+
#   stat_summary(fun=mean, geom="point", shape=15, size=5, col="black")+
#   geom_point()+
#   scale_fill_manual("Groups:",values=my_pal)+
#   theme(axis.title.x = element_blank())+
#   theme(legend.position = "none", legend.direction = "horizontal",
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
#   scale_y_continuous(name = expression(paste("NLR ", P[Crit], " (", DO[2], " in mg/L ", O[2], ")")), breaks = seq(0,25, by =3), limits = c(-0.2,25))+
#   annotate("text", 1, -0.2, label = "a", size = 5)+
#   annotate("text", 2, -0.2, label = "a", size = 5)+
#   annotate("text", 3, -0.2, label = "ac", size = 5)+
#   annotate("text", 4, -0.2, label = "ab", size = 5)+
#   annotate("text", 5, -0.2, label = "b", size = 5)+
#   annotate("text", 6, -0.2, label = "ac", size = 5)+
#   annotate("text", 7, -0.2, label = "d", size = 5)+
#   annotate("text", 8, -0.2, label = "bc", size = 5)
# NLR.popsex
# 
# #boxplot pop
# NLR.pop <- ggplot(datum[datum$NLR > 0 & datum$NLR < 25,], aes(x=Pop, y=NLR, fill=Pop))+
#   geom_boxplot()+
#   stat_summary(fun=mean, geom="point", shape=15, size=5, col="black")+
#   geom_point()+
#   scale_fill_manual("Groups:",values=my_pal2)+
#   theme(axis.title.x = element_blank())+
#   theme(legend.position = "none", legend.direction = "horizontal")+
#   scale_y_continuous(name = expression(paste("NLR ", P[Crit], " (", DO[2], " in mg/L ", O[2], ")")), breaks = seq(0,25, by =3), limits = c(-0.2,25))+
#   annotate("text", 1, -0.2, label = "a", size = 5)+
#   annotate("text", 2, -0.2, label = "a", size = 5)+
#   annotate("text", 3, -0.2, label = "b", size = 5)+
#   annotate("text", 4, -0.2, label = "c", size = 5)
# NLR.pop
# 
# #boxplot sex
# NLR.sex <- ggplot(datum[datum$NLR > 0 & datum$NLR < 25,], aes(x=Sex, y=NLR, fill=Sex))+
#   geom_boxplot()+
#   stat_summary(fun=mean, geom="point", shape=15, size=5, col="black")+
#   geom_point()+
#   scale_fill_manual("Groups:",values=my_pal3)+
#   theme(axis.title.x = element_blank())+
#   theme(legend.position = "none", legend.direction = "horizontal")+
#   scale_y_continuous(name = expression(paste("NLR ", P[Crit], " (", DO[2], " in mg/L ", O[2], ")")), breaks = seq(0,25, by =3), limits = c(-0.2,25))+
#   annotate("text", 1, -0.2, label = "a", size = 5)+
#   annotate("text", 2, -0.2, label = "a", size = 5)
# NLR.sex
# 
# #Combine plots
# png(file = "NLR Combined.png", units = "in", width = 10, height = 5, res = 300)
# ggarrange(NLR.sex, NLR.pop, NLR.popsex,
#           nrow = 1,ncol = 3,
#           labels = c("A", "B", "C"), label.x = 0, label.y = 1,
#           widths = c(1, 1, 1.5), heights = 1
# )
# dev.off()



#RI statistic among populations

          
        #Statistic summary
          #Pop
          datum.RI.mean.pop <- datum |> 
            group_by(Pop) |> 
            summarise(mean = mean(RI) |> round(2),
                      sd = sd(RI) |> round(2),
                      n = n(),
                      iqr = IQR(RI),
                      range = paste(range(RI) |> round(2), collapse = ' - ')
            ) 
          
          #Sex
          datum.RI.mean.sex <- datum |> 
            group_by(Sex) |> 
            summarise(mean = mean(RI) |> round(2),
                      sd = sd(RI) |> round(2),
                      n = n(),
                      iqr = IQR(RI),
                      range = paste(range(RI) |> round(2), collapse = ' - ')
            ) 
          
          #Popsex
          datum.RI.mean.popsex <- datum |> 
            group_by(Pop.sex) |> 
            summarise(mean = mean(RI) |> round(2),
                      sd = sd(RI) |> round(2),
                      n = n(),
                      iqr = IQR(RI),
                      range = paste(range(RI) |> round(2), collapse = ' - ')
            ) 
          
          
          
          #ridgeline pop.sex
          RI.ridge.popsex <- ggplot(datum, aes(x=RI, y=Pop.sex, fill = Pop.sex)) +
            geom_density_ridges(scale=2, bandwidth = 0.03, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(-0.2,1, by =0.2), limits = c(-0.2,1)) +
            scale_fill_manual("Groups:", values = my_pal)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Regulation index"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 1, 1.5, label = "a", size = 5)+
            annotate("text", 1, 2.5, label = "ab", size = 5)+
            annotate("text", 1, 3.5, label = "ab", size = 5)+
            annotate("text", 1, 4.5, label = "ab", size = 5)+
            annotate("text", 1, 5.5, label = "ab", size = 5)+
            annotate("text", 1, 6.5, label = "ab", size = 5)+
            annotate("text", 1, 7.5, label = "b", size = 5)+
            annotate("text", 1, 8.5, label = "ab", size = 5)
          
          RI.ridge.popsex
          
          #ridgeline pop
          RI.ridge.pop <- ggplot(datum, aes(x=RI, y=Pop, fill = Pop)) +
            geom_density_ridges(scale=2, bandwidth = 0.03, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(-0.2,1, by =0.2), limits = c(-0.2,1)) +
            scale_fill_manual("Groups:", values = my_pal2)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Regulation index"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 1, 1.5, label = "a", size = 5)+
            annotate("text", 1, 2.5, label = "a", size = 5)+
            annotate("text", 1, 3.5, label = "a", size = 5)+
            annotate("text", 1, 4.5, label = "a", size = 5)
          
          RI.ridge.pop
          
          #ridgeline sex
          RI.ridge.sex <- ggplot(datum, aes(x=RI, y=Sex, fill = Sex)) +
            geom_density_ridges(scale=2, bandwidth = 0.03, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(-0.2,1, by =0.2), limits = c(-0.2,1)) +
            scale_fill_manual("Groups:", values = my_pal3)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Regulation index"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 1, 1.5, label = "a", size = 5)+
            annotate("text", 1, 2.5, label = "a", size = 5)
          
          RI.ridge.sex
          
          #Combine plots
          png(file = "RI Combined.png", units = "in", width = 10, height = 5, res = 300)
          ggarrange(RI.sex, RI.pop, RI.popsex,
                    nrow = 1,ncol = 3,
                    labels = c("A", "B", "C"), label.x = 0, label.y = 1,
                    widths = c(1, 1, 1.5), heights = c(1)
          ) 
          dev.off()
      
      
#Body lengths among pops and sexes
      
        #Statistic summary
          #Pop
          datum.length.mean.pop <- datum[-c(9),] |> 
            group_by(Pop) |> 
            summarise(mean = mean(Totlen) |> round(2),
                      sd = sd(Totlen) |> round(2),
                      n = n(),
                      iqr = IQR(Totlen),
                      range = paste(range(Totlen) |> round(2), collapse = ' - ')
            ) 
          
          #Sex
          datum.length.mean.sex <- datum[-c(9),] |> 
            group_by(Sex) |> 
            summarise(mean = mean(Totlen) |> round(2),
                      sd = sd(Totlen) |> round(2),
                      n = n(),
                      iqr = IQR(Totlen),
                      range = paste(range(Totlen) |> round(2), collapse = ' - ')
            ) 
          
          #Popsex
          datum.length.mean.popsex <- datum[-c(9),] |> 
            group_by(Pop.sex) |> 
            summarise(mean = mean(Totlen) |> round(2),
                      sd = sd(Totlen) |> round(2),
                      n = n(),
                      iqr = IQR(Totlen),
                      range = paste(range(Totlen) |> round(2), collapse = ' - ')
            ) 
          
          
          #ridgeline pop.sex
          length.ridge.popsex <- ggplot(datum[-c(9),], aes(x=Totlen, y=Pop.sex, fill = Pop.sex)) +
            geom_density_ridges(scale=2, bandwidth = 0.04, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(1,2.5, by =0.5), limits = c(1,2.5)) +
            scale_fill_manual("Groups:", values = my_pal)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Body Length (mm)"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 2.5, 1.5, label = "a", size = 5)+
            annotate("text", 2.5, 2.5, label = "ab", size = 5)+
            annotate("text", 2.5, 3.5, label = "bc", size = 5)+
            annotate("text", 2.5, 4.5, label = "c", size = 5)+
            annotate("text", 2.5, 5.5, label = "c", size = 5)+
            annotate("text", 2.5, 6.5, label = "c", size = 5)+
            annotate("text", 2.5, 7.5, label = "d", size = 5)+
            annotate("text", 2.5, 8.5, label = "d", size = 5)
          
          length.ridge.popsex
          
          #ridgeline pop
          length.ridge.pop <- ggplot(datum[-c(9),], aes(x=Totlen, y=Pop, fill = Pop)) +
            geom_density_ridges(scale=2, bandwidth = 0.04, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(1,2.5, by =0.5), limits = c(1,2.5)) +
            scale_fill_manual("Groups:", values = my_pal2)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Body Length (mm)"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 2.5, 1.5, label = "a", size = 5)+
            annotate("text", 2.5, 2.5, label = "b", size = 5)+
            annotate("text", 2.5, 3.5, label = "b", size = 5)+
            annotate("text", 2.5, 4.5, label = "c", size = 5)
          
          length.ridge.pop
          
          #ridgeline sex
          length.ridge.sex <- ggplot(datum, aes(x=Totlen, y=Sex, fill = Sex)) +
            geom_density_ridges(scale=2, bandwidth = 0.04, rel_min_height =0.01,
                                jittered_points = TRUE, position = position_points_jitter(width = 0.00000001, height = 0), 
                                point_shape = "|", point_size = 4, point_color = "black",
                                quantile_lines = TRUE, quantile_fun = mean, vline_color = "snow", color = 'white')+
            scale_x_continuous(breaks = seq(1,2.5, by =0.5), limits = c(1,2.5)) +
            scale_fill_manual("Groups:", values = my_pal3)+
            theme_ridges(center_axis_labels = TRUE)+
            xlab(expression(paste("Body Length (mm)"))) +ylab("")+
            theme(legend.position = "none")+
            annotate("text", 2.5, 1.5, label = "a", size = 5)+
            annotate("text", 2.5, 2.5, label = "b", size = 5)
          
          length.ridge.sex
      
      #Combine plots
      png(file = "Length Combined.png", units = "in", width = 10, height = 5, res = 300)
      ggarrange(length.sex, length.pop, length.popsex,
                nrow = 1,ncol = 3,
                labels = c("A", "B", "C"), label.x = 0, label.y = 1,
                widths = c(1, 1, 1.5), heights = c(1)
      ) 
      dev.off()
      
      
#### Combine pop, sex, and pop.sex plots into separate figures for alpha, pcrit, and RI
      
      #Pop
      png(file = "Pop resp statistics.png", units = "in", width = 4, height = 8, res = 500)
      ggarrange(pcrit.ridge.pop, Alphas.ridge.pop, RI.ridge.pop,
                nrow = 3,ncol = 1,
                labels = c("A", "B", "C")
      ) 
      dev.off()
      
      
      #Sex
      png(file = "Sex resp statistics.png", units = "in", width = 4, height = 8, res = 300)
      ggarrange(pcrit.ridge.sex, Alphas.ridge.sex, RI.ridge.sex,
                nrow = 3,ncol = 1,
                labels = c("A", "B", "C")
      ) 
      dev.off()
      
      
      #Pop.sex
      png(file = "Popsex resp statistics.png", units = "in", width = 5, height = 9, res = 300)
      ggarrange(pcrit.ridge.popsex, Alphas.ridge.popsex, RI.ridge.popsex,
                nrow = 3,ncol = 1,
                labels = c("A", "B", "C")
      ) 
      dev.off()
      
      
#### Combine pop, sex, and pop.sex plots into separate figures for respiratory rates
      
      # #Pop
      # png(file = "Pop resp rates.png", units = "in", width = 9, height = 6, res = 300)
      # ggarrange(Upper_rate.pop, Lower_rate.pop,
      #           nrow = 1,ncol = 2,
      #           labels = c("A", "B")
      # ) 
      # dev.off()
      # 
      # 
      # #Sex
      # png(file = "Sex resp rates.png", units = "in", width = 9, height = 6, res = 300)
      # ggarrange(Upper_rate.sex, Lower_rate.sex,
      #           nrow = 1,ncol = 2,
      #           labels = c("A", "B")
      # ) 
      # dev.off()
      # 
      # 
      # #Pop.sex
      # png(file = "Popsex resp rates.png", units = "in", width = 9, height = 6, res = 300)
      # ggarrange(Upper_rate.popsex, Lower_rate.popsex, 
      #           nrow = 1,ncol = 2,
      #           labels = c("A", "B")
      # ) 
      # dev.off()
      # 
      # 
      

      
##### Statistical analysis ######

#####Is there a difference in each plate resp statistic among populations?
      
  #Alpha
      mod.alpha.popsex <- lm(Alpha ~ Pop.sex, data = datum[datum$Alpha < 25,])
      mod.alpha.pop <- lmer(Alpha ~ Pop + (1|Sex), data = datum[datum$Alpha < 25,])
      mod.alpha.sex <- lmer(Alpha ~ Sex + (1|Pop), data = datum[datum$Alpha < 25,])
      cld(emmeans(mod.alpha.popsex, pairwise ~ Pop.sex)) #Get CLDs 
      cld(emmeans(mod.alpha.pop, pairwise ~ Pop)) #Get CLDs
      cld(emmeans(mod.alpha.sex, pairwise ~ Sex)) #Get CLDs
      #Export data
      comp.1 <- emmeans(mod.alpha.popsex, pairwise ~ Pop.sex)
      write.csv(comp.1$emmeans, file = "alphas by popsex emmeans.csv", row.names = FALSE)
      write.csv(comp.1$contrasts, file = "alphas by popsex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.1$contrasts), file = "alphas by popsex contrasts CIs.csv", row.names = FALSE)
      
      comp.1b <- emmeans(mod.alpha.pop, pairwise ~ Pop)
      write.csv(comp.1b$emmeans, file = "alphas by pop emmeans.csv", row.names = FALSE)
      write.csv(comp.1b$contrasts, file = "alphas by pop contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.1b$contrasts), file = "alphas by pop contrasts CIs.csv", row.names = FALSE)
      
      comp.1c <- emmeans(mod.alpha.sex, pairwise ~ Sex)
      write.csv(comp.1c$emmeans, file = "alphas by sex emmeans.csv", row.names = FALSE)
      write.csv(comp.1c$contrasts, file = "alphas by sex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.1c$contrasts), file = "alphas by sex contrasts CIs.csv", row.names = FALSE)
      
  #PCrit
      mod.Pcrit.popsex <- lm(Breakpoint ~ Pop.sex, data = datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,])
      mod.Pcrit.pop <- lmer(Breakpoint ~ Pop + (1|Sex), data = datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,])
      mod.Pcrit.sex <- lmer(Breakpoint ~ Sex + (1|Pop), data = datum[datum$Breakpoint > 0 & datum$Breakpoint < 25,])
      cld(emmeans(mod.Pcrit.popsex, pairwise ~ Pop.sex)) #Get CLDs
          cld(emmeans(mod.Pcrit.pop, pairwise ~ Pop)) #Get CLDs
              cld(emmeans(mod.Pcrit.sex, pairwise ~ Sex)) #Get CLDs
      #Export data
      comp.2 <- emmeans(mod.Pcrit.popsex, pairwise ~ Pop.sex)
      write.csv(comp.2$emmeans, file = "Pcrit by popsex emmeans.csv", row.names = FALSE)
      write.csv(comp.2$contrasts, file = "Pcrit by popsex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.2$contrasts), file = "Pcrit by popsex contrasts CIs.csv", row.names = FALSE)
      
      comp.2b <- emmeans(mod.Pcrit.pop, pairwise ~ Pop)
      write.csv(comp.2b$emmeans, file = "Pcrit by pop emmeans.csv", row.names = FALSE)
      write.csv(comp.2b$contrasts, file = "Pcrit by pop contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.2b$contrasts), file = "Pcrit by pop contrasts CIs.csv", row.names = FALSE)
      
      comp.2c <- emmeans(mod.Pcrit.sex, pairwise ~ Sex)
      write.csv(comp.2c$emmeans, file = "Pcrit by sex emmeans.csv", row.names = FALSE)
      write.csv(comp.2c$contrasts, file = "Pcrit by sex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.2c$contrasts), file = "Pcrit by sex contrasts CIs.csv", row.names = FALSE)
      
  # #NLR
  #     mod.NLR.popsex <- lm(NLR ~ Pop.sex, data = datum[datum$NLR > 0 & datum$NLR < 25,])
  #     mod.NLR.pop <- lmer(NLR ~ Pop + (1|Sex), data = datum[datum$NLR > 0 & datum$NLR < 25,])
  #     mod.NLR.sex <- lmer(NLR ~ Sex + (1|Pop), data = datum[datum$NLR > 0 & datum$NLR < 25,])
  #     cld(emmeans(mod.NLR.popsex, pairwise ~ Pop.sex)) #Get CLDs
  #         cld(emmeans(mod.NLR.pop, pairwise ~ Pop)) #Get CLDs
  #             cld(emmeans(mod.NLR.sex, pairwise ~ Sex)) #Get CLDs
  #     #Export data
  #     comp.3 <- emmeans(mod.NLR.popsex, pairwise ~ Pop.sex)
  #     write.csv(comp.3$emmeans, file = "NLR by popsex emmeans.csv", row.names = FALSE)
  #     write.csv(comp.3$contrasts, file = "NLR by popsex contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.3$contrasts), file = "NLR by popsex contrasts CIs.csv", row.names = FALSE)
  # 
  #     comp.3b <- emmeans(mod.NLR.pop, pairwise ~ Pop)
  #     write.csv(comp.3b$emmeans, file = "NLR by pop emmeans.csv", row.names = FALSE)
  #     write.csv(comp.3b$contrasts, file = "NLR by pop contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.3b$contrasts), file = "NLR by pop contrasts CIs.csv", row.names = FALSE)
  # 
  #     comp.3c <- emmeans(mod.NLR.sex, pairwise ~ Sex)
  #     write.csv(comp.3c$emmeans, file = "NLR by sex emmeans.csv", row.names = FALSE)
  #     write.csv(comp.3c$contrasts, file = "NLR by sex contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.3c$contrasts), file = "NLR by sex contrasts CIs.csv", row.names = FALSE)
      
  #RI
      mod.RI.popsex <- lm(RI ~ Pop.sex, data = datum)
      mod.RI.pop <- lmer(RI ~ Pop + (1|Sex), data = datum)
      mod.RI.sex <- lmer(RI ~ Sex + (1|Pop), data = datum)
      cld(emmeans(mod.RI.popsex, pairwise ~ Pop.sex)) #Get CLDs
          cld(emmeans(mod.RI.pop, pairwise ~ Pop)) #Get CLDs
              cld(emmeans(mod.RI.sex, pairwise ~ Sex)) #Get CLDs
      #Export data
      comp.4 <- emmeans(mod.RI.popsex, pairwise ~ Pop.sex)
      write.csv(comp.4$emmeans, file = "RI by popsex emmeans.csv", row.names = FALSE)
      write.csv(comp.4$contrasts, file = "RI by popsex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.4$contrasts), file = "RI by popsex contrasts CIs.csv", row.names = FALSE)
      
      comp.4b <- emmeans(mod.RI.pop, pairwise ~ Pop)
      write.csv(comp.4b$emmeans, file = "RI by pop emmeans.csv", row.names = FALSE)
      write.csv(comp.4b$contrasts, file = "RI by pop contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.4b$contrasts), file = "RI by pop contrasts CIs.csv", row.names = FALSE)
      
      comp.4c <- emmeans(mod.RI.sex, pairwise ~ Sex)
      write.csv(comp.4c$emmeans, file = "RI by sex emmeans.csv", row.names = FALSE)
      write.csv(comp.4c$contrasts, file = "RI by sex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.4c$contrasts), file = "RI by sex contrasts CIs.csv", row.names = FALSE)
      
  # #Upper resp rate
  #     mod.Upper_rate.popsex <- lm(Upper_rate ~ Pop.sex, data = datum)
  #     mod.Upper_rate.pop <- lmer(Upper_rate ~ Pop +  (1|Sex), data = datum)
  #     mod.Upper_rate.sex <- lmer(Upper_rate ~ Sex + (1|Pop), data = datum)
  #     cld(emmeans(mod.Upper_rate.popsex, pairwise ~ Pop.sex)) #Get CLDs
  #         cld(emmeans(mod.Upper_rate.pop, pairwise ~ Pop)) #Get CLDs
  #             cld(emmeans(mod.Upper_rate.sex, pairwise ~ Sex)) #Get CLDs
  #     #Export data
  #     comp.5 <- emmeans(mod.Upper_rate.popsex, pairwise ~ Pop.sex)
  #     write.csv(comp.5$emmeans, file = "Upper by popsex emmeans.csv", row.names = FALSE)
  #     write.csv(comp.5$contrasts, file = "Upper by popsex contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.5$contrasts), file = "Upper by popsex contrasts CIs.csv", row.names = FALSE)
  #     
  #     comp.5b <- emmeans(mod.Upper_rate.pop, pairwise ~ Pop)
  #     write.csv(comp.5b$emmeans, file = "Upper by pop emmeans.csv", row.names = FALSE)
  #     write.csv(comp.5b$contrasts, file = "Upper by pop contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.5b$contrasts), file = "Upper by pop contrasts CIs.csv", row.names = FALSE)
  #     
  #     comp.5c <- emmeans(mod.Upper_rate.sex, pairwise ~ Sex)
  #     write.csv(comp.5c$emmeans, file = "Upper by sex emmeans.csv", row.names = FALSE)
  #     write.csv(comp.5c$contrasts, file = "Upper by sex contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.5c$contrasts), file = "Upper by sex contrasts CIs.csv", row.names = FALSE)
  #     
  # #Lower resp rate
  #     mod.Lower_rate.popsex <- lm(Lower_rate ~ Pop.sex, data = datum)
  #     mod.Lower_rate.pop <- lmer(Lower_rate ~ Pop +  (1|Sex), data = datum)
  #     mod.Lower_rate.sex <- lmer(Lower_rate ~ Sex +  (1|Pop), data = datum)
  #     cld(emmeans(mod.Lower_rate.popsex, pairwise ~ Pop.sex)) #Get CLDs
  #         cld(emmeans(mod.Lower_rate.pop, pairwise ~ Pop)) #Get CLDs
  #             cld(emmeans(mod.Lower_rate.sex, pairwise ~ Sex)) #Get CLDs
  #     #Export data
  #     comp.6 <- emmeans(mod.Lower_rate.popsex, pairwise ~ Pop.sex)
  #     write.csv(comp.6$emmeans, file = "Lower by popsex emmeans.csv", row.names = FALSE)
  #     write.csv(comp.6$contrasts, file = "Lower by popsex contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.6$contrasts), file = "Lower by popsex contrasts CIs.csv", row.names = FALSE)
  #     
  #     comp.6b <- emmeans(mod.Lower_rate.pop, pairwise ~ Pop)
  #     write.csv(comp.6b$emmeans, file = "Lower by pop emmeans.csv", row.names = FALSE)
  #     write.csv(comp.6b$contrasts, file = "Lower by pop contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.6b$contrasts), file = "Lower by pop contrasts CIs.csv", row.names = FALSE)
  #     
  #     comp.6c <- emmeans(mod.Lower_rate.sex, pairwise ~ Sex)
  #     write.csv(comp.6c$emmeans, file = "Lower by sex emmeans.csv", row.names = FALSE)
  #     write.csv(comp.6c$contrasts, file = "Lower by sex contrasts.csv", row.names = FALSE)
  #     write.csv(confint(comp.6c$contrasts), file = "Lower by sex contrasts CIs.csv", row.names = FALSE)
      
  #Body length (remove one row with one copepod)
      mod.length.popsex <- lm(Totlen ~ Pop.sex, data = datum[-c(9),])
      mod.length.pop <- lmer(Totlen ~ Pop +  (1|Sex), data = datum[-c(9),])
      mod.length.sex <- lmer(Totlen ~ Sex +  (1|Pop), data = datum[-c(9),])
      cld(emmeans(mod.length.popsex, pairwise ~ Pop.sex)) #Get CLDs
          cld( emmeans(mod.length.pop, pairwise ~ Pop)) #Get CLDs
               cld(emmeans(mod.length.sex, pairwise ~ Sex)) #Get CLDs
      #Export data
      comp.7 <- emmeans(mod.length.popsex, pairwise ~ Pop.sex)
      write.csv(comp.7$emmeans, file = "length by popsex emmeans.csv", row.names = FALSE)
      write.csv(comp.7$contrasts, file = "length by popsex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.7$contrasts), file = "length by popsex contrasts CIs.csv", row.names = FALSE)
      
      comp.7b <- emmeans(mod.length.pop, pairwise ~ Pop)
      write.csv(comp.7b$emmeans, file = "length by pop emmeans.csv", row.names = FALSE)
      write.csv(comp.7b$contrasts, file = "length by pop contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.7b$contrasts), file = "length by pop contrasts CIs.csv", row.names = FALSE)
      
      comp.7c <- emmeans(mod.length.sex, pairwise ~ Sex)
      write.csv(comp.7c$emmeans, file = "length by sex emmeans.csv", row.names = FALSE)
      write.csv(comp.7c$contrasts, file = "length by sex contrasts.csv", row.names = FALSE)
      write.csv(confint(comp.7c$contrasts), file = "length by sex contrasts CIs.csv", row.names = FALSE)
      
      
#### Stat summary across all populations ####
      
      datum.total.summary <- datum |> 
        filter(Breakpoint < 25 & Breakpoint > 0) |>
        filter(Alpha < 25) |>
        summarise(n = n(),
                  mean.alpha = mean(Alpha, na.rm=TRUE) |> round(4), #Alpha
                  iqr.alpha = IQR(Alpha, na.rm=TRUE),
                  range.alpha = paste(range(Alpha, na.rm=TRUE) |> round(4), collapse = ' - '),
                  mean.pcrit = mean(Breakpoint, na.rm=TRUE) |> round(4), #Pcrit
                  iqr.pcrit = IQR(Breakpoint, na.rm=TRUE),
                  range.pcrit = paste(range(Breakpoint, na.rm=TRUE) |> round(4), collapse = ' - '),
                  mean.RI = mean(RI, na.rm=TRUE) |> round(4), #RI
                  iqr.RI = IQR(RI, na.rm=TRUE),
                  range.RI = paste(range(RI, na.rm=TRUE) |> round(4), collapse = ' - '),
        ) 
      
      datum.total.summary2 <- data.frame(t(datum.total.summary))
      colnames(datum.total.summary2) <- "Summary value"
      
      write.csv(datum.total.summary2, file = "Mean statistics across all populations.csv") #Save final frame as a csv
      
      
  #### Resp statistics regression plots (extra) ####
      
      #All variables square root transformed to conform to assumptions of normality using log transformation 
        
      
    #Alpha vs others
      plot.7 <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Alpha), y=sqrt(Breakpoint), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7
      
      
      plot.7b <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Alpha), y=sqrt(NLR), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7b
      
      plot.7c <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Alpha), y=sqrt(RI), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "horizontal")
      plot.7c
      
      plot.7d <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Alpha), y=sqrt(Upper_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7d
      
      plot.7e <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Alpha), y=sqrt(Lower_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7e

      
    #Pcrit vs others
      plot.7f <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Breakpoint), y=sqrt(NLR), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7f
      
      plot.7g <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Breakpoint), y=sqrt(RI), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7g
      
      plot.7h <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Breakpoint), y=sqrt(Upper_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7h
      
      plot.7i <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Breakpoint), y=sqrt(Lower_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7i
      
    #NLR vs others
      plot.7j <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(NLR), y=sqrt(RI), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7j
      
      plot.7k <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(NLR), y=sqrt(Upper_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7k
      
      plot.7l <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(NLR), y=sqrt(Lower_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "bottom", legend.direction = "horizontal")
      plot.7l
      
   #RI vs others
      plot.7m <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(RI), y=sqrt(Upper_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7m
      
      plot.7n <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(RI), y=sqrt(Lower_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "vertical")
      plot.7n
      
      #Upper vs lower
      plot.7o <- datum |>
        filter(NLR < 8) |>
        ggplot(aes(x=sqrt(Upper_rate), y=sqrt(Lower_rate), col=Pop))+
        geom_point( pch=16, size=5)+
        geom_line(stat='smooth', method = "lm",linewidth = 1)+
        geom_ribbon(aes(col = NULL, group = Pop), stat='smooth', method = "lm", se=TRUE, alpha=0.1)+
        scale_color_manual("Treatment:", values=my_pal2)+
        theme(axis.text.x = element_text(size = 11),
              axis.title = element_text(size = 13),
              axis.text.y = element_text(size=10),
              legend.position = "none", legend.direction = "horizontal")
      plot.7o
      
      png(file = "Statistics correlations.png", units = "in", width = 14, height = 14, res = 300)
      plot.7 + plot.7b + plot.7c + plot.7d + plot.7e + 
      plot_spacer() + plot.7f + plot.7g + plot.7h + plot.7i +
      plot_spacer() + plot_spacer() + plot.7j + plot.7k + plot.7l +
      plot_spacer() + plot_spacer() + plot_spacer() + plot.7m + plot.7n +
      plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot.7o + plot_layout(ncol = 5, nrow = 5, byrow=FALSE) 
      dev.off()
  