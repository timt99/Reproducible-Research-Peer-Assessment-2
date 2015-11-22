---
title: "Reproducible Research : Peer Assessement 2"
author: "Tim Tran"
date: "November 21, 2015"
output: html_document
---

# Impact of Severe Weather Events across the United States from 1955 to 2011

## Synopsis

### In this analysis, we will show the impact of different type of severe weather events on public health and economic conditions across communities and municipalities. We will use data collected from the US National Oceaninc and Atmosphere Adminstration's (NOAA) from 1950- 2011. Many of these severe events may result in fatalities, injuries, and property damage. We will gather these fatalites, injuries, and property damage for each weather events and report which types of event are most harmful to the population health and economy. From these data, we found that excessive heat and tornado are most harmful with respect to population health, while flood, drought, and hurricane/typhoon have the greatest economic consequences.

## Tools used



```r
library(R.utils)
library(plyr)
library(ggplot2)
library(gridExtra)
```

##Data Reading and Processing
### We download the data and unzip it


```r
fileUrl <-  "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"

download.file(fileUrl, destfile= "StormData.csv.bz2")
bunzip2("StormData.csv.bz2")
```

```
## Error in decompressFile.default(filename = filename, ..., ext = ext, FUN = FUN): File already exists: StormData.csv
```

### Then we read the data and generate a csv file. We also get the dimension of the data read in.  

```{ r, echo= TRUE}

Storm_data <- read.csv("StormData.csv")
dim(Storm_data)


```

### There are 902297 rows and 37 columns total in this dataset. The events in the database start in the year 1950 and end in November 2011.  Since in the earlier years of the database, there are generally fewer events recorded, and most are likely due to a lack of good records.  We would like to analyze data only for recent years.

## Exploring the data
```{ r, echo= TRUE}
head(Storm_data, n= 3)

Storm_data$YEAR <- as.numeric(format(as.Date(Storm_data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

hist(Storm_data$YEAR, col= "blue", breaks = 40)


```

### Based on the above histogram, we see that the number of events begins to increase from 1990 to 2011. Thus, we subset the data from 1990 to 2011 to get the most of our data for complete analysis. 

##Subset our data from 1990 to 2011

```{ r, echo=TRUE}
Storm_data <- Storm_data[Storm_data$YEAR >= 1995, ]

dim(Storm_data)


```

###By subsetting, there are now 681500 rows and 38 columns in total.

## Impact on Public Health

### In this section, we will check the number of fatalities and injuries that are caused by the severe weather events. We would like to get the first 15 most severe types of weather events.

```{ r, echo=TRUE}

sort_Most <- function(keyName, top = 15, dataset= Storm_data)
{   index <- which(colnames(dataset) == keyName)

    field <- aggregate(dataset[, index], by = list(dataset$EVTYPE), FUN = "sum")
    names(field) <- c("EVTYPE", keyName)
    field <- arrange(field, field[, 2], decreasing = T)
    field <- head(field, n = top)
    field <- within(field, EVTYPE <- factor(x = EVTYPE, levels = field$EVTYPE))
    return(field)
}

  fatalities <- sort_Most("FATALITIES", dataset= Storm_data)
  injuries <- sort_Most("INJURIES", dataset = Storm_data)




```

## Impact on Economy
  
###  Convert property damage and crop damage into comparable numerical forms according to the meaning of units described in the code book(Storm Events). Both PROPDMGEXP and CROPDMGEXP columns record a multiplier for each observation where we have Hundred(H), Thousand(K), Million (M), and Billion (B).

```{ r, echo= TRUE}

 convert <- function(dataset = Storm_data, keyName, newFieldName) {
    totalLen <- dim(dataset)[2]
    index <- which(colnames(dataset) == keyName)
    dataset[, index] <- as.character(dataset[, index])
    logic <- !is.na(toupper(dataset[, index]))
    dataset[logic & toupper(dataset[, index]) == "B", index] <- "9"
    dataset[logic & toupper(dataset[, index]) == "M", index] <- "6"
    dataset[logic & toupper(dataset[, index]) == "K", index] <- "3"
    dataset[logic & toupper(dataset[, index]) == "H", index] <- "2"
    dataset[logic & toupper(dataset[, index]) == "", index] <- "0"
    dataset[, index] <- as.numeric(dataset[, index])
    dataset[is.na(dataset[, index]), index] <- 0
    dataset <- cbind(dataset, dataset[, index - 1] * 10^dataset[, index])
    names(dataset)[totalLen + 1] <- newFieldName
    return(dataset)
  }
  
Storm_data <- convert(Storm_data, "PROPDMGEXP", "propertyDamage") 

Storm_data <- convert(Storm_data, "CROPDMGEXP", "cropDamage")
names(Storm_data)

property_damage <- sort_Most("propertyDamage", dataset= Storm_data)

crop_damage <- sort_Most("cropDamage", dataset = Storm_data)



  
```

## Results

### The impact on public health is shown below by two sorted lists of severe weather events by number of people badly affected.

```{ r, echo= TRUE}

fatalities
injuries

```

### Graphs of total fatalities and total injuries affected by these severe weather events
###Plots  showing the number of fatalities with respect to Severe weather condition

```{ r, echo= TRUE}

g_fatalities <- ggplot(fatalities, aes(EVTYPE, FATALITIES))
 
facilitiesplot <- g_fatalities + geom_point(size = 4, col= "red")+ 
labs(x = "Severe Weather Type")+ labs(y = "NUmber of Fatalities")+ 
labs(title="Total Fatalities by Severe Weather\n Events in the U.S.\n from 1995 - 2011" )+ 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 
 
 g_injuries <- ggplot(injuries, aes(EVTYPE, INJURIES))
 
 injuriesplot <- g_injuries + geom_point(size = 4, col= "blue") + labs(x = "Severe Weather Type")+ 
 labs(y = "NUmber of Injuries")+ 
 labs(title="Total Fatalities by Severe Weather\n Events in the U.S.\n from 1995 - 2011" )+ 
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 
 grid.arrange(facilitiesplot, injuriesplot, ncol = 2)
 



```

### Based on the plots above, excessive head and tornado cause most fatalities, while tornados cause most injuries in the US from 1995- 2011. 
### As for the impact on economy, we got two sorted lists below by the amount money cost by damages.

```{ r, echo= TRUE}

property_damage

crop_damage 


```


### This is a pair of grapsh illustrating total property damage and total crop damage affected by severe events.

``` { r, echo= TRUE}

g_property <- ggplot(property_damage, aes(EVTYPE, propertyDamage))
 
Property_plot <- g_property + geom_point(size = 4, col= "red") + labs(x = "Severe Weather Type")+ 
labs(y = "NUmber of Property Damages in US dollars")+ 
labs(title="Total Property Damage by\n Events in the U.S.\n from 1995 - 2011" )+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 

 g_crop <- ggplot(crop_damage, aes(EVTYPE, cropDamage))
 
Crop_plot <- g_crop + geom_point(size = 4, col= "blue") + labs(x = "Severe Weather Type")+ 
labs(y = "NUmber of CROP Damages in US dollars")+ 
labs(title="Total Crop Damage by\n Events in the U.S.\n from 1995 - 2011" )+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 
grid.arrange(Property_plot, Crop_plot, ncol = 2)


```

### Based on the above graphs, the events that cause the most property damage are flood and hurricane/typhoon. 
### Drought and flood causes the most crop damage in the US from 1995 to 2011.


## Conclusion

### From these analysis, we found that excessive heat and tornado are the most harmful with respect to population health, while flood,drought,and hurricane/typhoon have the greatest econcomic consequences. 

## Run and publishing the analysis

```{ r, echo=TRUE}

library(knitr)
knit2html("RePResearchPeerAssessment2.Rmd")
browseURL("RePResearchPeerAssessment2.html")

```







