#Inbred Line Survival Analysis - hypoxia in vials
#Matthew Powers 
#powerm3@oregonstate.edu
#Combines analysis of survival assay data and generates figures for inbred lines
#Last edited 10-31-24 (Happy Halloween!)


#load required packages
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
library(ggridges) #For making ridgeline plots
library(MuMIn) #For getting R squared values from mixed models
library(gganimate) #Make animated plots
library(rptR) #To calculate the intraclass correlation coefficient for repeatability estimates among pops
library(patchwork) #stitching graphs together
library(multcomp) #automatically assign pairwise significance groupings
library(multcompView) #See groupings from multcomp
library(betareg) #To model proportions
library(memisc) #To export betareg model output
library(stargazer) #To export betareg model


#Set theme globally
theme_set(theme_cowplot())

#Read in data file
datum=read.csv(file="Vial Survival Data.csv", header = TRUE)

#Order data frame by line ID
datum.ordered <- datum[order(datum$Line), ]

#Use regex to pull out population name from line column
datum.ordered$Population <- str_extract(datum.ordered$Line, '\\D*')


####Survival 

    #Transform proportions to be between 0 and 1 but not include 0 and 1 according to DOI: 10.1037/1082-989X.11.1.54 
    #Smithson, M. & Verkuilen, J. Psychol. Methods 11, 54–71 (2006). 
    datum.ordered$Survival.trans <- ((datum.ordered$Survival.prop*107)+0.5)/108 
    
  #Create data frame with the means and sd's for each lines survival data
    datum.stat <- datum.ordered %>% group_by(Line) %>% summarize(
      count = n(),
      mean.surv = mean(Survival.prop, na.rm = TRUE), 
      sd.surv = sd(Survival.prop, na.rm = TRUE)) 
    
    datum.stat.pop <- datum.ordered %>% group_by(Population) %>% summarize(
      count = n(),
      mean.surv = mean(Survival.prop, na.rm = TRUE), 
      sd.surv = sd(Survival.prop, na.rm = TRUE),
      sum.surv = sum(na.omit(Survival)),
      total.tested = sum(na.omit(Total.copepods)))
    
    
  #Use regex to pull out population name from line column
    datum.stat$Population <- str_extract(datum.stat$Line, '\\D*')
    
  #Relevel group variable to put in the desired order for plotting
    datum.stat$Population<- factor(datum.stat$Population,
                        levels = c("SD", "BOB", "SH"))
    
  
    
    
  #Get mean across all populations
    Total.mean <- mean(na.omit(datum.stat$mean.surv))
    
    
    #Color pallete tweaking for ggplot (optional)
    br_pal <- met.brewer("Signac")
    br_pal2 <- met.brewer("Renoir")
    my_pal2 <- c(br_pal2[8],br_pal2[c(2,12)])
    # just for displaying old and new palette - not required for solution
    show_col(br_pal)
    show_col(br_pal2)
    show_col(my_pal2)
    
    
  #Make plot
   survival.plot <- ggplot(data=datum.stat, aes(x=Line, y=mean.surv, label=count)) +
      geom_hline(yintercept=Total.mean, lty=2, linewidth =1, color="grey") +
      geom_point(aes(x=Line, y=mean.surv, color=Population), shape = 16, size = 6)+ 
      geom_errorbar(aes(ymin=mean.surv-sd.surv, ymax=mean.surv+sd.surv, color=Population), width=0.5, cex=1)+ 
      geom_text(color="white")+
      coord_flip() + 
      scale_color_manual("Populations:",values = my_pal2) +
      ggtitle("Survival after 72 hours hypoxia exposure")+ 
      theme(legend.position = "bottom", legend.direction = "horizontal", title = element_text(size=13))+
      xlab("Inbred Lines") + 
      scale_y_continuous(name="Proportion of surviving individuals", breaks = seq(0, 1, by = 0.1))
      
   survival.plot
    
    png(filename = "Survival across lines.png", width = 8, height = 9, units = "in", res = 300)
    survival.plot
    dev.off()

#### KO 
    
    #Transform proportions to be between 0 and 1 but not include 0 and 1 according to DOI: 10.1037/1082-989X.11.1.54 
    #Smithson, M. & Verkuilen, J. Psychol. Methods 11, 54–71 (2006). 
    datum.ordered$LOE72.trans <- ((datum.ordered$KO72.prop*107)+0.5)/108 
    datum.ordered$LOE48.trans <- ((datum.ordered$KO48.prop*107)+0.5)/108 
    datum.ordered$LOE24.trans <- ((datum.ordered$KO24.prop*107)+0.5)/108 
    
    #Create data frame with the means and sd's for each lines KO data
    datum.stat.KO <- datum.ordered %>% group_by(Line) %>% summarize(
      count = n(),
      mean.KO24 = mean(KO72.prop, na.rm = TRUE), 
      sd.KO24 = sd(KO72.prop, na.rm = TRUE),
      mean.KO48 = mean(KO48.prop, na.rm = TRUE), 
      sd.KO48 = sd(KO48.prop, na.rm = TRUE),
      mean.KO72 = mean(KO72.prop, na.rm = TRUE), 
      sd.KO72 = sd(KO72.prop, na.rm = TRUE),) 
    
    
    #Use regex to pull out population name from line column
    datum.stat.KO$Population <- str_extract(datum.stat.KO$Line, '\\D*')
    
    #Relevel group variable to put in the desired order for plotting
    datum.stat.KO$Population<- factor(datum.stat.KO$Population,
                                   levels = c("SD", "BOB", "SH"))
    
    
    #Get mean across all populations for each time point
    Total.meanKO.24 <- mean(na.omit(datum.stat.KO$mean.KO24))
    Total.meanKO.48 <- mean(na.omit(datum.stat.KO$mean.KO48))
    Total.meanKO.72 <- mean(na.omit(datum.stat.KO$mean.KO72))

    
    #Melt data frame into lengthwise order for means
    datum.stat.KO.molten <- melt(datum.stat.KO[,c(1,2,3,5,7, 9)], id = c("Line", "count", "Population"))
    
    colnames(datum.stat.KO.molten) <- c("Line", "Count", "Population", "Timepoint", "Mean")
    
    #Melt data frame into lengthwise order for means
    datum.stat.KO.molten.sds <- melt(datum.stat.KO[,c(1,4,6,8,9)], id = c("Line", "Population"))
    colnames(datum.stat.KO.molten.sds) <- c("Line", "Population", "Timepoint", "SD")

    #Add sd data to molten data frame
    datum.stat.KO.molten$SD = datum.stat.KO.molten.sds$SD
    
    #Relabel timepoint factor
    datum.stat.KO.molten %>% 
      mutate(Timepoint = fct_recode(Timepoint, "24 hours" = "mean.KO24",
                                   "48 hours" = "mean.KO48",
                                   "72 hours" = "mean.KO72")) -> datum.stat.KO.molten
    
    #Get number of each time point in the timepoint column
    table(datum.stat.KO.molten$Timepoint)
    
    #Add mean values to column using table values from line above
    datum.stat.KO.molten$meanline <- c(rep(Total.meanKO.24, 45), 
                                       rep(Total.meanKO.48, 45), 
                                       rep(Total.meanKO.72, 45))
    
    

    #Make combined KO plot for just 72 hours
      KO72 <- datum.stat.KO.molten |>
        filter(Timepoint == "72 hours") |>
        ggplot(aes(x=Line, y=Mean, label=Count)) +
        geom_hline(aes(yintercept=meanline), lty=2, size =1, color="grey") +
        geom_point(aes(x=Line, y=Mean, color=Population), shape = 16, size = 6)+ 
        geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color=Population), width=0.5, cex=1)+ 
        geom_text(color="white")+
        coord_flip(ylim = c(-0.1, 1)) + 
        #geom_point(data=datum.ordered, aes(x=Line, y=Survival), shape = 15, size = 1.5) + 
        scale_color_manual("Populations:",values = my_pal2) +
        ggtitle("Knockdown over 72 hours of hypoxia exposure")+ 
        theme(legend.position = "bottom", legend.direction = "horizontal", title = element_text(size=15))+
        xlab("Inbred Lines") + 
        scale_y_continuous(name="Proportion of individuals knocked down", breaks = seq(0, 1, by = 0.1))
    
      KO72
    
      png(filename = "KO at 72 across lines.png", width = 8, height = 9, units = "in", res = 300)
      KO72
      dev.off()
    
    
    #Make animated plot
   KO.animation<- ggplot(data=datum.stat.KO.molten, aes(x=Line, y=Mean, label=Count)) +
      geom_hline(data=datum.stat.KO.molten, aes(yintercept=meanline), lty=2, size =1, color="grey") +
      geom_point(aes(x=Line, y=Mean, color=Population), shape = 16, size = 6)+ 
      geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD, color=Population), width=0.5, cex=1)+ 
      geom_text(color="white")+
      geom_text(aes(x=length(datum.stat$Line), y=0.95, label = Timepoint), alpha = 0.9,  col = "black", size = 5) +
      coord_flip(ylim = c(-0.1, 1)) + 
      #geom_point(data=datum.ordered, aes(x=Line, y=Survival), shape = 15, size = 1.5) + 
      scale_color_manual("Populations:",values = my_pal2) +
      ggtitle("Knockdown over 72 hours of hypoxia exposure")+ 
      theme(legend.position = "bottom", legend.direction = "horizontal", title = element_text(size=15))+
      xlab("Inbred Lines") + 
      scale_y_continuous(name="Proportion of individuals knocked down", breaks = seq(0, 1, by = 0.1))+
      #Start gganimate code
      transition_states(
        Timepoint,
        transition_length = 2,
        state_length = 3
        ) +
        enter_fade() + 
        exit_shrink() +
        ease_aes('sine-in-out')

   anim <- animate(KO.animation, nframes = 200, fps = 20, height = 11, width = 7.5, units="in", res=300)
   
   anim_save("KO across timepoints.gif", anim) 
        

##### How repeatable are survival and KO measurements across populations? ####
   
  #Survival
   #Repeatability in survival among pops
     rep1 <- rpt(cbind(Survival, Death) ~ (1 | Population), grname = "Population", 
                 data = datum.ordered, datatype = "Proportion", link = "probit", nboot = 1000, npermut = 0)
     print(rep1) #Show basic ICC, SE around ICC estimate, CI around ICC estimate and P-value for repeatability test
    
     #Repeatability in survival among lines
     rep1b <- rpt(cbind(Survival, Death) ~ (1 | Line), grname = "Line", 
                 data = datum.ordered, datatype = "Proportion", link = "probit", nboot = 1000, npermut = 0)
     print(rep1b) #Show basic ICC, SE around ICC estimate, CI around ICC estimate and P-value for repeatability test
     
    #Repeatability in LOE among pops
     rep2 <- rpt(cbind(KO72, Swim72) ~ (1 | Population), grname = "Population", 
                 data = datum.ordered, datatype = "Proportion", link = "probit", nboot = 1000, npermut = 0)
     print(rep2) #Show basic ICC, SE around ICC estimate, CI around ICC estimate and P-value for repeatability test
     
    #Repeatability in LOE among lines
     rep2b <- rpt(cbind(KO72, Swim72) ~ (1 | Line), grname = "Line", 
                  data = datum.ordered, datatype = "Proportion", link = "probit", nboot = 1000, npermut = 0)
     print(rep2b) #Show basic ICC, SE around ICC estimate, CI around ICC estimate and P-value for repeatability test
     
   
#Within populations, how well does KO correlate with survival after 72 hours exposure to hypoxia?
   #Control for non-independence of data from population and line groupings
     #All populations
     #24 hours
   mod.1 <- betareg(Survival.trans ~ LOE24.trans + Population, data=datum.ordered)
    summary(mod.1)
    plot(mod.1)
    
    emm.1 <- emtrends(mod.1, ~LOE24.trans, var = "LOE24.trans", mode = "response")
    emm.1
    
    summary(mod.1)$pseudo.r.squared
    
    #48 hours
    mod.1b <- betareg(Survival.trans ~ LOE48.trans + Population, data=datum.ordered)
    summary(mod.1b)
    plot(mod.1b)
    
    emm.2 <- emtrends(mod.1b, ~LOE48.trans, var = "LOE48.trans", mode = "response")
    emm.2
    
    summary(mod.1b)$pseudo.r.squared
    
    #72 hours
    mod.1c <- betareg(Survival.trans ~ LOE72.trans + Population, data=datum.ordered)
    summary(mod.1c)
    plot(mod.1c)
    
    emm.1c <- emtrends(mod.1c, ~LOE72.trans, var = "LOE72.trans", mode = "response")
    emm.1c
    
    summary(mod.1c)$pseudo.r.squared
    
    
    
    #Vs 24
    KOvsSurvival.24 <- datum.ordered |>
      ggplot(aes(x=KO24.prop, y=Survival.prop, fill=Population, shape = Population))+ 
      geom_smooth(method='glm', color = "black")+
      geom_point(aes(y=Survival.prop), size=5)+
      scale_x_continuous(name="Proportion of individuals with LOE", breaks = seq(0,1, by = 0.1))+
      scale_y_continuous(name="Proportion of surviving individuals", breaks = seq(0,1, by = 0.1))+
      scale_shape_manual(name="Populations:", values = c(24,22,21))+
      coord_cartesian(ylim = c(0,1))+
      annotate("text", x = 0.7, y = 0.9, label = expression(italic(beta)*"= -0.0896,"~italic(p)*"<0.0001"), 
               size = 4, color ="black")+
      annotate("text", x = 0.7, y = 0.85, label = bquote("R"^2~"= 0.29"), size = 4, color ="black")+
      scale_fill_manual(name="Populations:",values = my_pal2)+
      ggtitle(label = "Survival vs LOE at 24 hours")+
      theme(legend.position = "none")
    
    KOvsSurvival.24
    
    #Vs 48
    KOvsSurvival.48 <- datum.ordered |>
      ggplot(aes(x=KO48.prop, y=Survival.prop, fill=Population, shape = Population))+ 
      geom_smooth(method='glm', color = "black")+
      geom_point(aes(y=Survival.prop), size=5)+
      scale_x_continuous(name="Proportion of individuals with LOE", breaks = seq(0,1, by = 0.1))+
      scale_y_continuous(name="Proportion of surviving individuals", breaks = seq(0,1, by = 0.1))+
      scale_shape_manual(name="Populations:", values = c(24,22,21))+
      coord_cartesian(ylim = c(0,1))+
      annotate("text", x = 0.7, y = 0.9, label = expression(italic(beta)*"= -0.109,"~italic(p)*"<0.0001"), 
               size = 4, color ="black")+
      annotate("text", x = 0.7, y = 0.85, label = bquote("R"^2~"= 0.52"), size = 4, color ="black")+
      scale_fill_manual(name="Populations:",values = my_pal2)+
      ggtitle(label = "Survival vs LOE at 48 hours")+
      theme(legend.position = "none")
    
    KOvsSurvival.48
    
    #vs 72
    KOvsSurvival.72 <- datum.ordered |>
    ggplot(aes(x=KO72.prop, y=Survival.prop, fill=Population, shape = Population))+ 
      geom_smooth(method='glm', color = "black")+
      geom_point(aes(y=Survival.prop), size=5)+
      scale_x_continuous(name="Proportion of individuals with LOE", breaks = seq(0,1, by = 0.1))+
      scale_y_continuous(name="Proportion of surviving individuals", breaks = seq(0,1, by = 0.1))+
      scale_shape_manual(name="Populations:", values = c(24,22,21))+
      coord_cartesian(ylim = c(0,1))+
      annotate("text", x = 0.3, y = 0.1, label = expression(italic(beta)*"= -0.091,"~italic(p)*"<0.0001"), 
               size = 4, color ="black")+
      annotate("text", x = 0.3, y = 0.05, label = bquote("R"^2~"= 0.43"), size = 4, color ="black")+
      scale_fill_manual(name ="Populations:",values = my_pal2)+
      ggtitle(label = "Survival vs LOE at 72 hours")+
      theme(legend.position = "bottom", legend.direction = "horizontal")+
      guides()

    KOvsSurvival.72
    
    png("KO vs Survival in vials.png", units = "in", height = 14, width = 6, res=300)
    KOvsSurvival.24 / KOvsSurvival.48 / KOvsSurvival.72 + plot_annotation(tag_levels = 'A')
    dev.off()
    
    
    ## Reviewer 1 asks if the slope for BOB is different than the other pops at 72 hours
    mod.2 <- lm(Survival.prop ~ KO72.prop*Population, data = datum.ordered)
    plot(mod.2)
    emm.2 <- emtrends(mod.2, ~Population, var="KO72.prop",  mode = "response") #Create em_grid object on response scale (odds ratio)
    emm.2 #KO72.prop.trend  is the increase in the proportion of copepods that survived per 1 unit increase in LOE proportion
    emm.2.contrasts <- contrast(emm.2,"pairwise", type = "response") #Do pairwise contrasts between pops
    emm.2.contrasts #Estimate is the increase/decrease in the proportion of surviving copepods between pops
    confint(emm.1.contrasts) #Show pairwise contrasts with confidence intervals
    cld(emm.1) #Get group letter assignments for plotting
    
    
    
    