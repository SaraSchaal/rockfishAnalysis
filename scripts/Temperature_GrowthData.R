# Analysis of Growth Data
# SM Schaal
# September 10, 2015

# Set Working Directory
setwd("~/Documents/Northeastern/Lotterhos_Lab/Field Work/2015/TPC_Experiment/Analysis")

# Load Data Frame
df.blacktemp <- read.csv("Temp_Data_Black.csv")
df.coppertemp <- read.csv("Temp_Data_Copper.csv")
df.growth <- read.csv("Fish_Measurement_Data.csv")

# Subset Dataframe to Include only Those Necessary for Analysis
df.subgrowth  <- subset(df.growth, select = c("Date", "Length_mm", "Weight_g","Experimental_Treatment_C", "Bucket" ))

# Remove columns with any NAs
df.completegrowth <- na.omit(df.subgrowth)

### Analysis for Black Rockfish Experiment ###

## Length data ##
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

  # Plot Temperature versus change in length
    plot(df.3.length.blk$df.temp.blk, df.3.length.blk$end.length.blk-df.3.length.blk$start.length.blk,
         xlab = "Temperature Treatment (Celcius)", ylab = "Length (mm)", main = "Black Rockfish Growth Rate - Length",
         bty = "l", pch = 20)

## Weight Data ##
  # Get weight data from first day and last day of experiment
    start.weight.blk <- tapply(df.completegrowth$Weight_g[correct.day.blk], df.completegrowth$Bucket[correct.day.blk], mean)
    end.weight.blk <- tapply(df.completegrowth$Weight_g[last.day.blk], df.completegrowth$Bucket[last.day.blk], mean, na.rm=TRUE)

  # Create data frames for starting data and ending data and merge them
    df.start.weight.blk <- data.frame(bucket=names(start.weight.blk), start.weight.blk)
    df.end.weight.blk <- data.frame(bucket=names(end.weight.blk), end.weight.blk)
    df.2.weight.blk <- merge(df.start.weight.blk, df.end.weight.blk)

  # Put temperature information into a dataframe and merge with measurement data
#   df.temp2.cp<- data.frame(bucket=names(df.temp.cp), df.temp.cp)
    df.3.weight.blk <- merge(df.temp2.blk, df.2.weight.blk)

  # Plot Temperature versus change in weight
    plot(df.3.weight.blk$df.temp.blk, df.3.weight.blk$end.weight.blk-df.3.weight.blk$start.weight.blk,
        xlab = "Temperature Treatment (Celcius)", ylab = expression(paste("Mean ", Delta , " Weight (g)")), main = "Black Rockfish Growth Rate",
        bty = "l", pch = 20)


### Analysis for Copper Rockfish ###

## Length Data ##
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

  # Plot Temperature versus change in length
    plot(df.3.length.cp$df.temp.cp, df.3.length.cp$end.length.cp-df.3.length.cp$start.length.cp,
        xlab = "Temperature Treatment (Celcius)", ylab = "Length (mm)", main = "Copper Rockfish Growth Rate - Length",
        bty = "l", pch = 20)


## Weight Data ##
  # Get weight data from first day and last day of experiment
    start.weight.cp <- tapply(df.completegrowth$Weight_g[correct.day.cp], df.completegrowth$Bucket[correct.day.cp], mean)
    end.weight.cp <- tapply(df.completegrowth$Weight_g[last.day.cp], df.completegrowth$Bucket[last.day.cp], mean, na.rm=TRUE)

  # Create data frames for starting data and ending data and merge them
    df.start.weight.cp <- data.frame(bucket=names(start.weight.cp), start.weight.cp)
    df.end.weight.cp <- data.frame(bucket=names(end.weight.cp), end.weight.cp)
    df.2.weight.cp <- merge(df.start.weight.cp, df.end.weight.cp)

  # Get temperature data for each bucket
#  df.temp.cp <- tapply(df.completegrowth$Experimental_Treatment_C, df.completegrowth$Bucket, mean)

  # Put temperature information into a dataframe and merge with measurement data
#   df.temp2.cp<- data.frame(bucket=names(df.temp.cp), df.temp.cp)
    df.3.weight.cp <- merge(df.temp2.cp, df.2.weight.cp)

  # Plot Temperature versus change in length
   plot(df.3.weight.cp$df.temp.cp, df.3.weight.cp$end.weight.cp-df.3.weight.cp$start.weight.cp,
         xlab = "Temperature Treatment (Celcius)", ylab = "Mean ", Delta ," Weight (g)", main = "Copper Rockfish Growth Rate",
         bty = "l", pch = 20)

## Mortality Data ##

mort <- read.csv("Mortality_Black.csv")

mort.black <- which(mort$Species == "Black")
mort.black2 <- tapply(mort$Days_Survived[mort.black], mort$Treatment_Temperature[mort.black],mean)
df.mort.black <- data.frame(temp=names(mort.black2), mort.black2)

mort.copper <- which(mort$Species == "Copper")
mort.copper2 <- tapply(mort$Days_Survived[mort.copper], mort$Treatment_Temperature[mort.copper],mean)
df.mort.copper <- data.frame(temp=names(mort.copper2), mort.copper2)

boxplot(mort$Days_Survived[mort.copper]~mort$Round_Temp[mort.copper], 
        xlab = expression(paste("Temperature Treatment (",degree,"C)")), 
        ylab = "Days Survived", ylim = c(1, 14),
        main = "Copper Rockfish Mortality")
boxplot(mort$Days_Survived[mort.black]~mort$Round_Temp[mort.black], 
        xlab = expression(paste("Temperature Treatment (",degree,"C)")), 
        ylab = "Days Survived", ylim = c(1, 14), 
        main = "Black Rockfish Mortality")
## Analysis ##


