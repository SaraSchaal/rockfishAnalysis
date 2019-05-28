# Analysis of Rockfish Phenotype Data
# SM Schaal
# September 25, 2015

#### Temperature Data ####

  # Install packages
    if(!("plyr" %in% installed.packages())){
      install.packages("plyr")
    }
    if(!("dplyr" %in% installed.packages())){
      install.packages("dplyr")
    }
    if(!("reshape2" %in% installed.packages())){
      install.packages("reshape2")
    }
    if(!("AICcmodavg" %in% installed.packages())){
      install.packages("AICcmodavg")
    }
    if(!("ggplot2" %in% installed.packages())){
      install.packages("ggplot2")
    }

    library(plyr)
    library(dplyr)
    library(reshape2)
    library(AICcmodavg)
    library(ggplot2)

  ## Black Rockfish ##

    # Load Data Frames
      temp.data <- read.csv("data/Temp_Data_Black.csv")
      head(temp.data)
      temp.data2 <- temp.data[, -(14:15)]

    # Turn data frame into tlb_df object so that text wraps so they don't fill screen
      temp.df <- tbl_df(temp.data2)
      levels(temp.df$Bucket_ID)

    # average temperatures measured by GAMMA and pH Meter and put values in new column
      temp.df$means <- rowMeans(subset(temp.df, select = c(Gamma, pH_Meter)), na.rm = TRUE)

    # get mean and standard deviation of bucket temps
      summary.data <- select(temp.df, Bucket_ID, Temperature_Treatment, means)
      melted.temp.data <- melt(summary.data, id.vars = c("Bucket_ID", "Temperature_Treatment"), 
                              na.rm = TRUE)
      summary.stats.black <- ddply(melted.temp.data, c("Bucket_ID", "Temperature_Treatment"), 
                                  summarise, mean = mean(value), sd = sd(value))

      treatment.buckets.summ.blk <- summary.stats.black[c(1:5, 7, 9, 10:16), ]
      all.treatment.buckets.blk <- summary.stats.black[c(1:16),]
      write.csv(all.treatment.buckets.blk, "data/black_exp_temps.csv", row.names = FALSE)

  ## Copper Rockfish ##

    # Load Data
      temp.data.cp <- read.csv("data/Temp_Data_Copper.csv")
      head(temp.data.cp)

    # Turn data fram into a tbl_df to wrap text and calculate Mean of Gamma and pH Meter temps
      temp.df.cp <- tbl_df(temp.data.cp)
      temp.df.cp$means <- rowMeans(subset(temp.df.cp, select = c(Gamma, pH_Meter)), 
                                  na.rm = TRUE)

    # Calculate summary statistics
      summary.data.cp <- select(temp.df.cp, Bucket_ID, Temperature_Treatment, means)
      melted.temp.data.cp <- melt(summary.data.cp, id.vars = c("Bucket_ID", "Temperature_Treatment"), 
                                  na.rm = TRUE)
      summary.stats.copper <- ddply(melted.temp.data.cp, c("Bucket_ID","Temperature_Treatment"), 
                                    summarise, mean = mean(value), sd = sd(value))
  
    # Separate out just treatment values
      treatment.buckets.summ.cp <- summary.stats.copper[c(1:6, 9:16), ]
      all.treatment.buckets.cp <- summary.stats.copper[c(1:16),]
      write.csv(all.treatment.buckets.cp, "data/copper_exp_temps.csv", row.names = FALSE)
      
    # Factor Temperature Treatment data  
      temp.df$Temperature_Treatment <- as.factor(temp.df$Temperature_Treatment)
      temp.df.cp$Temperature_Treatment <- as.factor(temp.df.cp$Temperature_Treatment)
      
 ## Boxplot of Bucket Temperatures ##
      par(mfrow = c(2,1))
      par(oma = c(4, 4, 1, 1))
      par(mar = c(2, 2, 2, 1))
      bp <- boxplot(means~Temperature_Treatment, data = temp.df, xlab = expression(paste('Treatment Temperature (',~degree,'C)',sep='')), 
              ylab = expression(paste('Temperature Across Experiment (',~degree,'C)',sep='')))
      text(12, 9, labels = "BY Complex Experiment")
      mtext(text = "A", side = 3, adj = 0.01, padj = 0.01)
      boxplot(means~Temperature_Treatment, data = temp.df.cp, xlab = expression(paste('Treatment Temperature (',~degree,'C)',sep='')), 
              ylab = expression(paste('Temperature Across Experiment (',~degree,'C)',sep='')), col = "orange")
      text(12, 9, labels = "CQ Complex Experiment")
      mtext(text = "B", side = 3, adj = 0.01, padj = 0.01)
      mtext(side = 2, outer = TRUE, cex = 1, line = 2.2, text = expression(paste('Temperature Across Experiment (',~degree,'C)',sep='')))
      mtext(side = 1, outer = TRUE, cex = 1, line = 2.2, text = expression(paste('Treatment Temperature (',~degree,'C)',sep='')))
     
      
  ## Violin Plots of Bucket Temperature ##     
      Buck.temp.plot <- ggplot(temp.df, aes(x=Temperature_Treatment, y=means)) + 
        geom_violin()
      black.temp.plot <- Buck.temp.plot + stat_summary(fun.y=mean, geom="point", shape=19, size=1) + 
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      
      Buck.temp.cp.plot <- ggplot(temp.df.cp, aes(x=Temperature_Treatment, y=means)) + 
        geom_violin()
      copper.temp.plot <- Buck.temp.cp.plot + stat_summary(fun.y=mean, geom="point", shape=19, size=1) + 
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      ggarrange(black.temp.plot, copper.temp.plot, nrow = 2)

#### Growth Data ####
  
  # Load Data Frame
    df.blacktemp <- read.csv("Temp_Data_Black.csv")
    df.coppertemp <- read.csv("Temp_Data_Copper.csv")
    df.growth <- read.csv("Fish_Measurement_Data_w_IDs_seq_4.csv")

  # Subset Dataframe to Include only Those Necessary for Analysis
    df.subgrowth  <- subset(df.growth, select = c("Date", "Species", "Length_mm", "Weight_g",
                                                  "Experimental_Treatment_C", "Bucket"))

  # Remove columns with any NAs
    df.completegrowth <- na.omit(df.subgrowth)


#### Analysis for Black Rockfish Experiment ####

  ## Length Data Manipulation ##

    # Get measurement data from first day and last day of experiment
      correct.day.blk <- which(df.completegrowth$Date==20150620)
      start.length.blk <- tapply(df.completegrowth$Length_mm[correct.day.blk], df.completegrowth$Bucket[correct.day.blk], mean)
      last.day.blk <- which(df.completegrowth$Date==20150703)
      end.length.blk <- tapply(df.completegrowth$Length_mm[last.day.blk], df.completegrowth$Bucket[last.day.blk], mean, na.rm=TRUE)

    # Create data frames for starting data and ending data and merge them
      df.start.length.blk <- data.frame(bucket=names(start.length.blk), start.length.blk)
      df.end.length.blk <- data.frame(bucket=names(end.length.blk), end.length.blk)
      df.2.length.blk <- merge(df.start.length.blk, df.end.length.blk)

    # Get temperature data for each bucket
      df.temp.blk <- tapply(df.completegrowth$Experimental_Treatment_C, df.completegrowth$Bucket, mean)

    # Put temperature information into a dataframe and merge with measurement data
      df.temp2.blk <- data.frame(bucket=names(df.temp.blk), df.temp.blk)
      df.3.length.blk <- merge(df.temp2.blk, df.2.length.blk)

    # Create data frame of length data to merge with temp data
      df.3.length.blk$length.change <- (df.3.length.blk$end.length.blk-df.3.length.blk$start.length.blk)
      blk.growth.data <- select(df.3.length.blk, bucket, length.change)

  ## Weight Data Manipulation ##

    # Get weight data from first day and last day of experiment
      start.weight.blk <- tapply(df.completegrowth$Weight_g[correct.day.blk], df.completegrowth$Bucket[correct.day.blk], mean)
      end.weight.blk <- tapply(df.completegrowth$Weight_g[last.day.blk], df.completegrowth$Bucket[last.day.blk], mean, na.rm=TRUE)

    # Create data frames for starting data and ending data and merge them
      df.start.weight.blk <- data.frame(bucket=names(start.weight.blk), start.weight.blk)
      df.end.weight.blk <- data.frame(bucket=names(end.weight.blk), end.weight.blk)
      df.2.weight.blk <- merge(df.start.weight.blk, df.end.weight.blk)

    # Put temperature information into a dataframe and merge with measurement data
      df.3.weight.blk <- merge(df.temp2.blk, df.2.weight.blk)

    # Create data frame of growth data to merge with temp and length
      df.3.weight.blk$weight.change <- (df.3.weight.blk$end.weight.blk-df.3.weight.blk$start.weight.blk)    
      blk.weight <- select(df.3.weight.blk, weight.change)
      blk.growth.data2 <- cbind(blk.growth.data, blk.weight)
    
    # cbind actual bucket temperatures from averages
      blk.growth.data3 <- cbind(treatment.buckets.summ.blk$mean, 
                                treatment.buckets.summ.blk$sd, 
                                df.3.weight.blk$start.weight.blk, 
                                df.3.weight.blk$end.weight.blk, blk.growth.data2)
      names(blk.growth.data3)[1] <- "mean"
      names(blk.growth.data3)[2] <- "sd"
      names(blk.growth.data3)[3] <- "start.weight"
      names(blk.growth.data3)[4] <- "end.weight"
  
    ## Condition Factor Before and After ##
      blk.growth.data3$CF.start <- ((df.3.weight.blk$start.weight.blk)/(df.3.length.blk$start.length.blk)^3)*100
      blk.growth.data3$CF.end <- ((df.3.weight.blk$end.weight.blk)/(df.3.length.blk$end.length.blk)^3)*100

    # Change in CF
      blk.growth.data3$CF.change <- (blk.growth.data3$CF.end-blk.growth.data3$CF.start)
  
## Plots ##

      # Make sure data transferred correctly and plots are same as before
        plot(blk.growth.data3$mean, blk.growth.data3$weight.change, pch = 20,
             xlab = "Temperature Treatment (Celcius)", ylab = "Mean Weight Change (g)", 
             main = "Black Rockfish Growth Rate - Weight ", bty = "l")
  
        plot(blk.growth.data3$mean, blk.growth.data3$length.change, pch = 20,
             xlab = "Temperature Treatment (Celcius)", ylab = "Mean Length Change (mm)", 
             main = "Black Rockfish Growth Rate - Length", bty = "l")
        
      # % Weight gain calculation
        blk.growth.data3$weight.change.percent <- ((blk.growth.data3$weight.change/df.3.weight.blk$start.weight.blk)*100)
       

#### Analysis for Copper Rockfish ####

  ## Length Data Manipulation ##

    # Get measurement data from first day and last day of experiment
      correct.day.cp <- which(df.completegrowth$Date==20150704)
      start.length.cp <- tapply(df.completegrowth$Length_mm[correct.day.cp], df.completegrowth$Bucket[correct.day.cp], mean)
      last.day.cp <- which(df.completegrowth$Date==20150718)
      end.length.cp <- tapply(df.completegrowth$Length_mm[last.day.cp], df.completegrowth$Bucket[last.day.cp], mean, na.rm=TRUE)

    # Create data frames for starting data and ending data and merge them
      df.start.length.cp <- data.frame(bucket=names(start.length.cp), start.length.cp)
      df.end.length.cp <- data.frame(bucket=names(end.length.cp), end.length.cp)
      df.2.length.cp <- merge(df.start.length.cp, df.end.length.cp)

    # Get temperature data for each bucket
      df.temp.cp <- tapply(df.completegrowth$Experimental_Treatment_C, df.completegrowth$Bucket, mean)

    # Put temperature information into a dataframe and merge with measurement data
      df.temp2.cp<- data.frame(bucket=names(df.temp.cp), df.temp.cp)
      df.3.length.cp <- merge(df.temp2.cp, df.2.length.cp)

    # Create data frame of length data to merge with temp data
      df.3.length.cp$length.change <- (df.3.length.cp$end.length.cp-df.3.length.cp$start.length.cp)
      cp.growth.data <- select(df.3.length.cp, bucket, length.change)

  ## Weight Data Manipulation ##
  
    # Get weight data from first day and last day of experiment
      start.weight.cp <- tapply(df.completegrowth$Weight_g[correct.day.cp], df.completegrowth$Bucket[correct.day.cp], mean)
      end.weight.cp <- tapply(df.completegrowth$Weight_g[last.day.cp], df.completegrowth$Bucket[last.day.cp], mean, na.rm=TRUE)

    # Create data frames for starting data and ending data and merge them
      df.start.weight.cp <- data.frame(bucket=names(start.weight.cp), start.weight.cp)
      df.end.weight.cp <- data.frame(bucket=names(end.weight.cp), end.weight.cp)
      df.2.weight.cp <- merge(df.start.weight.cp, df.end.weight.cp)
   
    # Put temperature information into a dataframe and merge with measurement data
      df.3.weight.cp <- merge(df.temp2.cp, df.2.weight.cp)

    # Create data frame of growth data to merge with temp and length
      df.3.weight.cp$weight.change <- (df.3.weight.cp$end.weight.cp-df.3.weight.cp$start.weight.cp)    
      cp.weight <- select(df.3.weight.cp, weight.change)
      cp.growth.data2 <- cbind(cp.growth.data, cp.weight)

    # cbind actual bucket temperatures from averages
      cp.growth.data3 <- cbind(treatment.buckets.summ.cp$mean, treatment.buckets.summ.cp$sd, 
                               df.3.weight.cp$start.weight.cp, df.3.weight.cp$end.weight.cp, cp.growth.data2)
      names(cp.growth.data3)[1] <- "mean"
      names(cp.growth.data3)[2] <- "sd"
      names(cp.growth.data3)[3] <- "start.weight"
      names(cp.growth.data3)[4] <- "end.weight"

  ## Condition Factor Before and After ##
      
    # start and end condition factor calculation
      cp.growth.data3$CF.start <- ((df.3.weight.cp$start.weight.cp)/(df.3.length.cp$start.length.cp)^3)*100
      cp.growth.data3$CF.end <- ((df.3.weight.cp$end.weight.cp)/(df.3.length.cp$end.length.cp)^3)*100

    # Change in CF
      cp.growth.data3$CF.change <- (cp.growth.data3$CF.end-cp.growth.data3$CF.start)

  ## Plots ##

    # Make sure data transferred correctly and plots are same as before
      plot(cp.growth.data3$mean, cp.growth.data3$weight.change, pch = 20, 
           xlab = "Temperature Treatment (Celcius)", ylab = "Mean Weight Change (g)", 
           main = "Copper Rockfish Growth Rate - Weight", bty = "l")
  
      plot(cp.growth.data3$mean, cp.growth.data3$length.change, pch = 20, 
           xlab = "Temperature Treatment (Celcius)", ylab = "Mean Length Change (mm)", 
           main = "Copper Rockfish Growth Rate - Length", bty = "l")

    # Condition factor calculation
      cp.growth.data3$condition.factor <- ((cp.growth.data3$weight.change/(cp.growth.data3$length.change)^3)*100)

    # Plot condition factor
      plot(cp.growth.data3$mean, cp.growth.data3$CF.change, pch = 18,
           bty = "l", main = "Condition Factor - Copper",
           ylab = "Condition Factor", 
           xlab = "Temperature Treatment (Celcius)", col = "blue")
           # ylim =c(-0.00025, 0.00005)
  
      ggplot(blk.growth.data3, aes(x=mean, y=CF.change)) + labs(x = "Temperature (Celcius)", 
            y = "Condition Factor") + ggtitle("Condition Factor - Black Rockfish") + geom_point() + geom_smooth()

      plot(blk.growth.data3$mean, blk.growth.data3$CF.change, pch = 20,
          bty = "l", main = "Condition Factor - Black",
          ylab = "Condition Factor", 
          xlab = "Temperature Treatment (Celcius)")

      ggplot(cp.growth.data3, aes(x=mean, y=CF.change)) + labs(x = "Temperature (Celcius)", 
             y = "Condition Factor") + ggtitle("Condition Factor - Copper Rockfish") + geom_point() + geom_smooth()
    
# Full model with squared term and initial weight as a predictor

    full.blk.CF <- lm(blk.growth.data3$CF.change~blk.growth.data3$mean + I(blk.growth.data3$mean^2) + blk.growth.data3$start.weight)
    full.cp.CF <- lm(cp.growth.data3$CF.change~cp.growth.data3$mean + I(cp.growth.data3$mean^2) + cp.growth.data3$start.weight)
    summary(full.blk.CF)
    summary(full.cp.CF)

# dropping start.weight 

    drop.stweight.blk.CF <- lm(blk.growth.data3$CF.change~blk.growth.data3$mean + I(blk.growth.data3$mean^2))
    drop.stweight.cp.CF <- lm(cp.growth.data3$CF.change~cp.growth.data3$mean + I(cp.growth.data3$mean^2))
    summary(drop.stweight.blk.CF)    
    summary(drop.stweight.cp.CF)

# dropping linear term

    drop.linear.blk.CF <- lm(blk.growth.data3$CF.change~I(blk.growth.data3$mean^2) + blk.growth.data3$start.weight)  
    drop.linear.cp.CF <- lm(cp.growth.data3$CF.change~I(cp.growth.data3$mean^2) + cp.growth.data3$start.weight)
    summary(drop.linear.blk.CF)
    summary(drop.linear.cp.CF)

# dropping both linear and start.weight

    drop.lin.stwe.blk.CF  <- lm(blk.growth.data3$CF.change~I(blk.growth.data3$mean^2))
    drop.lin.stwe.cp.CF <- lm(cp.growth.data3$CF.change~I(cp.growth.data3$mean^2))
    summary(drop.lin.stwe.blk.CF)
    summary(drop.lin.stwe.cp.CF)

#comparing models

  #black
    anova(full.blk.CF, drop.stweight.blk.CF)
    anova(full.blk.CF, drop.linear.blk.CF)
    
    AICc(full.blk.CF)
    AICc(drop.stweight.blk.CF)  
    AICc(drop.linear.blk.CF)
    AIC()
  
  #copper
    anova(full.cp.CF, drop.stweight.cp.CF)
    anova(full.cp.CF, drop.linear.cp.CF)

  # % Weight gain calculation
      cp.growth.data3$weight.change.percent <- ((cp.growth.data3$weight.change/df.3.weight.cp$start.weight.cp)*100)

    # Plot % weight gain
      plot(cp.growth.data3$mean, cp.growth.data3$weight.change.percent, pch = 18, 
           bty = "l", main = "% Weight Gain - Copper Rockfish", ylab = "% Weight Gain", 
           xlab = "Treatment Temperature (Celcius)", col = "blue")

      ggplot(cp.growth.data3, aes(x=mean, y=weight.change.percent)) + labs(x = "Temperature (Celcius)", 
            y = "% Weight Gain") + ggtitle("% Weight Gain - Copper Rockfish") + geom_point() + geom_smooth()

      plot(blk.growth.data3$mean, blk.growth.data3$weight.change.percent, pch = 20, 
          bty = "l", main = "% Weight Gain - Black Rockfish", ylab = "% Weight Gain", 
          xlab = "Treatment Temperature")
  
      ggplot(blk.growth.data3, aes(x=mean, y=weight.change.percent)) + labs(x = "Temperature (Celcius)", 
             y = "% Weight Gain") + ggtitle("% Weight Gain - Black Rockfish") + geom_point() + geom_smooth()

      ggplot(blk.growth.data3, aes(x=mean, y=end.weight)) + labs(x = "Temperature (Celcius)", 
           y = " End Weight") + ggtitle("END Weight - Black Rockfish") + geom_point() + geom_smooth()

# Initial simple models

      sim.blk.WG <- lm(blk.growth.data3$weight.change.percent~blk.growth.data3$mean)
      sim.cp.WG <- lm(cp.growth.data3$weight.change.percent~cp.growth.data3$mean)

      quad.blk.WG <- lm(blk.growth.data3$weight.change.percent~I(blk.growth.data3$mean^2))
      quad.cp.WG  <- lm(cp.growth.data3$weight.change.percent~I(cp.growth.data3$mean^2))

# Full model with squared term and initial weight as a predictor

    full.blk.WG <- lm(blk.growth.data3$weight.change.percent~blk.growth.data3$start.weight + poly(blk.growth.data3$mean, 2, raw=TRUE))
    full.cp.WG <- lm(cp.growth.data3$weight.change.percent~cp.growth.data3$mean + I(cp.growth.data3$mean^2) + cp.growth.data3$start.weight)
    summary(full.blk.WG)
    summary(full.cp.WG)
            
   # dropping start.weight 
            
    drop.stweight.blk.WG <- lm(blk.growth.data3$weight.change.percent~blk.growth.data3$mean + I(blk.growth.data3$mean^2))
    drop.stweight.cp.WG <- lm(cp.growth.data3$weight.change.percent~cp.growth.data3$mean + I(cp.growth.data3$mean^2))
    summary(drop.stweight.blk.WG)    
    summary(drop.stweight.cp.WG)

    
    # dropping linear term
            
    drop.linear.blk.WG <- lm(blk.growth.data3$weight.change.percent~I(blk.growth.data3$mean^2) + blk.growth.data3$start.weight)  
    drop.linear.cp.WG <- lm(cp.growth.data3$weight.change.percent~I(cp.growth.data3$mean^2) + cp.growth.data3$start.weight)
    summary(drop.linear.blk.WG)
    summary(drop.linear.cp.WG)
            
    # endweight 
      
      full.endweight.blk.WG <- lm(blk.growth.data3$end.weight~blk.growth.data3$mean+I(blk.growth.data3$mean^2))
  # comparing models
            
      #black
        anova(full.blk.WG, drop.stweight.blk.WG)
        anova(full.blk.WG, drop.linear.blk.WG)
           
        AICc(full.blk.WG)
        AICc(drop.stweight.blk.WG)
        AICc(drop.linear.blk.WG)
        AICc(sim.blk.WG)
        AICc(quad.blk.WG)
    #copper
        anova(full.cp.WG, drop.stweight.cp.WG)
        anova(full.cp.WG, drop.linear.cp.WG) 

#### Mortality Data ####

      mort <- read.csv("Mortality_Black.csv")

      mort.black <- which(mort$Species == "Black")
      mort.black2 <- tapply(mort$Days_Survived[mort.black], mort$Treatment_Temperature[mort.black],mean)
      df.mort.black <- data.frame(temp=names(mort.black2), mort.black2)

      mort.copper <- which(mort$Species == "Copper")
      mort.copper2 <- tapply(mort$Days_Survived[mort.copper], mort$Treatment_Temperature[mort.copper],mean)
      df.mort.copper <- data.frame(temp=names(mort.copper2), mort.copper2)

      boxplot(mort$Days_Survived[mort.copper]~mort$Treatment_Temperature[mort.copper], 
              xlab = "Temperature Treatment (Celcius)", ylab = "Days Survived", 
              ylim = c(1, 14), main = "Copper Rockfish Mortality")
      boxplot(mort$Days_Survived[mort.black]~mort$Treatment_Temperature[mort.black], 
              xlab = "Temperature Treatment (Celcius)", ylab = "Days Survived", 
              ylim = c(1, 14), main = "Black Rockfish Mortality")

#### Respiration Rate Data ####

resp  <- read.csv("Fish_Measurement_Data_2.csv")
head(resp)
df.resp <- subset(resp, select = c("Species", "Length_mm", "Weight_g", "Starting_DO_.ppm", "DO_30min_ppm", 
                                   "Experimental_Treatment_C", "Bucket"))
df.respiration <- na.omit(df.resp)

# Calculate Respiration Rate
  df.respiration$respirationrate <- (df.respiration$Starting_DO_.ppm-df.respiration$DO_30min_ppm)/30

# Take only positive values
  df.respiration_pos <- df.respiration[df.respiration$respirationrate > 0, ]

# subset data for the two species
  copper <- df.respiration_pos[df.respiration_pos$Species=="Copper", ] 
  black <- df.respiration_pos[df.respiration_pos$Species %in% c("Black", "Yellowtail", "Silver"), ]

# put in experimental treatment temperatures
  black$exp_temp  <- c(10.33846, 10.33846, 10.33846, 10.45769, 10.45769, 18.48462,
                       18.48462, 18.48462, 20.18269, 20.18269, 20.18269, 13.46111,
                       13.46111, 20.71346, 20.71346, 20.71346, 20.97308, 20.97308, 
                       22.61731, 22.61731, 19.35962, 19.35962, 19.35962, 15.23462,
                       15.23462, 15.23462, 19.98462, 19.98462, 17.86923, 17.86923,
                       17.86923, 13.89808, 13.89808, 22.35682, 22.35682, 19.45192,
                       19.45192, 19.45192)

  copper$exp_temp <- c(10.58704, 10.58704, 10.51111, 10.51111, 10.51111, 10.51111,
                       19.22222, 19.22222, 19.22222, 19.91852, 19.91852, 19.91852, 
                       21.03333, 21.03333, 13.46111, 13.46111, 13.46111, 13.46111, 
                       22.01296, 22.01296, 22.01296, 21.40741, 21.40741, 21.40741, 
                       22.85417, 17.97037, 17.97037, 17.97037, 17.97037, 15.10000, 
                       15.10000, 18.90926, 18.90926, 18.90926, 17.15000, 17.15000, 
                       17.15000, 13.71667, 19.98889, 19.98889)




# Plot log of respiration against the log of growth
  plot(log(copper$respirationrate)~log(copper$Weight_g), bty = "l",
       xlim=c(-2.5, 1.5), ylim = c(-7,-3), pch = 18, col = "blue", 
       main = "Linear Regression of Respiration Rate and Weight", 
       xlab = "log10(Weight)", ylab = "log10(Respiration Rate)")
  points(log(black$respirationrate)~log(black$Weight_g), pch = 20, col = "black")

 
  abline(lm(log(copper$respirationrate)~log(copper$Weight_g)), col = "blue")
  abline(lm(log(black$respirationrate)~log(black$Weight_g)), col = "black")

  text(-1, -6.5, adj = c(0,0), labels = "copper: p = 0.14,  R^2 = 0.17", col = "blue")
  text(-1, -7, adj = c(0,0), labels = "black: p = 0.0059*,  R^2 = 0.54", col = "black")

# Get linear model data and print the summary
  copper_linear_model <- lm(log(copper$respirationrate)~log(copper$Weight_g)+copper$exp_temp)
  black_linear_model <- lm(log(black$respirationrate)~log(black$Weight_g)+black$exp_temp)

  fish  <- rbind(copper, black)

  fish$Species[fish$Species=="Yellowtail"] <-"Black"
  fish$Species[fish$Species=="Silver"] <-"Black"

  fish_linear_model <- lm(log(fish$respirationrate)~log(fish$Weight_g)+fish$exp_temp*fish$Species)

  summary(fish_linear_model)
  summary(copper_linear_model)
  summary(black_linear_model)

# Plot both species
 # plot(log(fish$respirationrate)~log(fish$Weight_g), bty = "l", pch = 20,
 #      ylab = "log10(Respiration Rate)", xlab = "log10(Weight)", 
 #      main = "Linear Regression of Respiration Rate and Fish Weight")
 #abline(lm(log(fish$respirationrate)~log(fish$Weight_g)))
  
# Full data set - dividing by weight
  fish$adjrespirationrate = (fish$respirationrate/(fish$Weight_g))

  # Plot and Model
  #  plot(fish$exp_temp, fish$adjrespirationrate, bty = "l", pch = 20,
  #       main = "Respiration Rate for Full Dataset", ylab = "Respiration Rate (mg g-1 min-1)",
  #       xlab = "Experimental Temperature (Celcius)")
  #  abline(lm(fish$adjrespirationrate~fish$exp_temp))
  #  text(13, 0.08, labels = "Interaction: p = 0.74")
  #  text(13, 0.07, labels = "Slope: p = 0.086")
  #  fish_linear_model2 <- lm(fish$adjrespirationrate~fish$exp_temp + fish$Species*fish$exp_temp)
    
  #  summary(fish_linear_model2)

# Individual Species Model - Use slope and intercept to parameterize the scaling equation
  copper$adjrespirationrate = (copper$respirationrate/(copper$Weight_g))
  black$adjrespirationrate = (black$respirationrate/(black$Weight_g))

# Plot the adjusted respiration rate against the experimental temperature
  plot(copper$exp_temp, copper$adjrespirationrate, bty = "l", pch = 18,
       main = "Copper Rockfish Respiration Rate", ylab = "Respiration Rate (mg g-1 min-1)",
       xlab = "Experimental Temperature (Celcius)", col = "blue")
  abline(lm(copper$adjrespirationrate~copper$exp_temp), col = "blue")
  text(15, 0.07, labels = "p = 0.298, R^2 = 0.03")
  summary(lm(copper$adjrespirationrate~copper$exp_temp))

  plot(black$exp_temp, black$adjrespirationrate, bty = "l", pch = 20,
       main = "Black Rockfish Respiration Rate", ylab = "Respiration Rate (mg g-1 min-1)",
       xlab = "Experimental Temperature (Celcius)", col = "black")
  abline(lm(black$adjrespirationrate~black$exp_temp), col = "black")
  text(15, 0.04, labels = "p = 0.0050*, R^2 = 0.20")
  summary(lm(black$adjrespirationrate~black$exp_temp))

  plot(copper$exp_temp, copper$respirationrate, bty = "l", pch = 20,
       main = "Copper Rockfish Respiration Rate", ylab = "Respiration Rate",
       xlab = "Experimental Temperature")

   plot(black$exp_temp, black$respirationrate, bty = "l", pch = 18,
       main = "Black Rockfish Respiration Rate", ylab = "Respiration Rate",
       xlab = "Experimental Temperature", col = "blue")

   
  