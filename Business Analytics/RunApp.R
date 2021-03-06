install.packages("lattice", dependencies = TRUE)
install.packages("ggvis", dependencies = TRUE)
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(lattice)
library(ggvis)
library(ggplot2)

superstore <- read.csv("SuperStoreUS.csv",header = TRUE)
names(superstore) = c("RowID","OrderPriority","Discount","UnitPrice","ShippingCost","CustomerID","CustomerName","ShipMode","CustomerSegment","ProductCategory","ProductSubCategory","ProductContainer","ProductName","ProductBaseMargin","Country","Region","StateorProvince","City","PostalCode","OrderDate","ShipDate","Profit","QuantityOrderedNew","Sales","OrderID")  
superstore$OrderDate = as.Date(superstore$OrderDate, "%m/%d/%Y")
superstore$ShipDate = as.Date(superstore$ShipDate, "%m/%d/%Y")
superstore$StateorProvince = as.character(superstore$StateorProvince)
superstore$ProductName = as.character(superstore$ProductName)
superstore.states <- levels(superstore$StateorProvince)
superstoreDates.startEnd <- range(superstore$OrderDate)

SriAppsDir <- "/Users/srigovind/Desktop/Personal/Hadoop/BigDataStack/RProgramming/Week7/shiny/SriApps"
c2_try <-paste(SriAppsDir,"final",sep="/")
runApp(c2_try)
