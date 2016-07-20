---
title: "Assignment 9"
author: "Karen Clark"
date: "July 20, 2016"
output: html_document
---

#Questions for Each Dataset:

Dataset = hsales

##Plot the time series. Can you Indentify seasonal fluctuations and/or a trend?

```{r}
#first load the fpp package to obtain the dataset
library(fpp)

#load data into R
data("hsales")

#Time series plot
plot(hsales)

```

The data shows strong seasonality within each year. There is no apparant trend in the data.

##Use a classical decomposition to calculate the trend-cycle and seasonal indices.

```{r}
#decomposition of hsales
 hsalesd <- decompose(hsales)

#trend-cycle plot from decomposition
plot(hsalesd)
```

##Do the results support the graphical interpretation of part (a)?
Based on the seasonal trend plot - yes. The seasonal plot again displays the yearly seasonality.

##Compute and plot the seasonally adjusted data

```{r}
#store the seasonal information for the dataset in the 
hsalesadj <- seasadj(hsalesd)

#plot seasonally adjusted data
plot(hsalesadj)
```

##Change one observation to be an outlieer, and recompute the seasonally adjusted data.  What is the effect of the outlier?

```{r}
#create an outlier
hsales3 <- ts(c(hsales[1:54],hsales[55]+200,hsales[56:275]),start=c(1980,1),frequency=12)

#plot data
plot(hsales3)
```
The outlier creates a spike is sales in 1985 that does not follow the pattern of the yearly data.

##Does it make a difference if the outlier is near the end rather than in the middle of the time series?

```{r}
#move the outlier toward the end of the timeseries
hsales4 <- ts(c(hsales[1:200],hsales[201]+200,hsales[202:275]),start=c(2000,1),frequency=12)

#plot the outlier
plot(hsales4)
```

The only difference with the outlier at the end is the large spike in sales moves to that year.  The rest of the sales data follows the same pattern.

##Use STL to decompose the series

```{r}
#decompose data with stL function
hsalesfit <- stl(hsales, s.window = 5)

#plot data

plot(hsales, col="gray",
  main="Monthly Housing sales (millions)",
  ylab="Housing Sales", xlab="")
lines(hsalesfit$time.series[,2],col="red",ylab="Trend")

```
#volatility Assignment

S&P Stock Assigned:AG10

```{r}
#load library for tseries package
library(tseries)

#Get data for AG10 stock
 SNPdata <- get.hist.quote(instrument = "agio",quote="Close", provider = "yahoo")
```

##Calculate Log Returns
```{r}
#Calculate log returns for data
SNPret <- log(lag(SNPdata)) - log(SNPdata)

#Display Log Data
SNPret
```

##Calculate volatility measure
```{r}
#Calculate volatility
SNPvol <- sd(SNPret) * sqrt(250) * 100

#Display Volatility
SNPvol


```

The volatility of the AGIO stock is 81%. Which indicates the stock returns vary greatly from year to year.

##Calculate volatility measure with a continuous lookback window
```{r}
#Volatility function
get
Vol <- function(d, logrets)
{

	var = 0

	lam = 0

	varlist <- c()

	for (r in logrets) {

		lam = lam*(1 - 1/d) + 1
	
	var = (1 - 1/lam)*var + (1/lam)*r^2

		varlist <- c(varlist, var)

	}

	sqrt(varlist)
}
```

#Plot results

```{r}
volest <- Vol(10,SNPret)

volest2 <- Vol(30,SNPret)

volest3 <- Vol(100,SNPret)

plot(volest,type="l")

lines(volest2,type="l",col="red")
 
lines(volest3, type = "l", col="blue")
```


##Interpretation
The Volest2 with a weight of 2/3 seems to follow the original data the closest.