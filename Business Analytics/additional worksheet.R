install.packages("shiny", dependencies = TRUE)
install.packages("ggvis", dependencies = TRUE)
install.packages("shiny", dependencies = TRUE)
install.packages("sqldf")
library(sqldf)
library(shiny)
library(ggvis)
iris_data <- as.data.frame(iris)

for(index in 1:nrow(iris_data))
{
  if(iris_data$Species[index] == "setosa") {
    iris_data[index , "FlowerGroup"] <- 1
  } else if(iris_data$Species[index] == "versicolor") {
    iris_data[index , "FlowerGroup"] <- 2
  } else if(iris_data$Species[index] == "virginica") {
    iris_data[index , "FlowerGroup"] <- 3
  } 
  index = index + 1
} 

SriAppsDir <- "/Users/srigovind/Desktop/Personal/Hadoop/BigDataStack/RProgramming/Week7/shiny/SriApps"
c2 <-paste(SriAppsDir,"C2_with_navbar",sep="/")
runApp(c2)

SriAppsDir <- "/Users/srigovind/Desktop/Personal/Hadoop/BigDataStack/RProgramming/Week7/shiny/SriApps"
c2_try <-paste(SriAppsDir,"C2_with_navbar_try",sep="/")
runApp(c2_try)

SriAppsDir <- "/Users/srigovind/Desktop/Personal/Hadoop/BigDataStack/RProgramming/Final_Project"
Final <-paste(SriAppsDir,"shiny_project",sep="/")
runApp(Final)

setwd("/Users/srigovind/Desktop/Personal/Hadoop/BigDataStack/RProgramming/Final_Project/shiny_project")
list.files()

superstore <- read.csv("SuperStoreUS.csv",header = TRUE)
str(superstore)
rownames(superstore)
names(superstore) = c("RowID","OrderPriority","Discount","UnitPrice","ShippingCost","CustomerID","CustomerName","ShipMode","CustomerSegment","ProductCategory","ProductSubCategory","ProductContainer","ProductName","ProductBaseMargin","Country","Region","StateorProvince","City","PostalCode","OrderDate","ShipDate","Profit","QuantityOrderedNew","Sales","OrderID")  

Region <- sqldf("select distinct Region from superstore where  ")
sqldf("select * from superstore where OrderPriority = 'Low' and Region = 'West'")

Region$Region

install.packages("lattice")
library(lattice)

xyplot(Profit ~ Sales | Region, superstore, layout = c(4, 1),
       main = "Store Profit as a Function of Sales",
       panel = function(x, y, ...) 
       {
         panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
         panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
         panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
       })
superstore

mtcarsOrder<-mtcars[order(mtcars$cyl),]
counts <-table(mtcarsOrder$cyl,
               cut(mtcarsOrder$hp,breaks=5))

all_values<-function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br/>")
}
iris_data[Sepal.Length,]

iris_data[,Sepal.Length][iris_data$FlowerGroup == 1]
superstore[superstore$Order.Priority == "Low",] [superstore$Order.Priority == "Low",][superstore$Region == "West",]

superstore.west <- superstore[superstore$Region == "West",]

trees.ordered <- trees[order(trees$Height),]

superstore[superstore$Region == "West",] %>% 
  ggvis(~Sales, ~Profit, fill:= "blue", size = ~Profit) %>%
  layer_points() %>%
  add_tooltip(all_values, "hover")


if (PriorityInput == "all") {
  superstore.priority <- superstore
}else if (PriorityInput == "Not_Specified") {
  superstore.priority <- superstore[superstore$Order.Priority == "Not Specified",]
}else {
  superstore.priority <- superstore[superstore$Order.Priority == PriorityInput,]
}
if (input$RegionType == "all") {
  
  xyplot(Profit ~ Sales | Region, superstore.priority, layout = c(4, 1),
         main = "Store Profit as a Function of Sales",
         panel = function(x, y, ...) 
         {
           panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
           panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
           panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
         })
}else {
  superstore.priority[superstore.priority$Region == input$RegionType,] %>% 
    ggvis(~Sales, ~Profit, fill:= "blue", size = ~Profit) %>%
    layer_points() %>%
    add_tooltip(all_values, "hover")
}


superstore.priority <- superstore[superstore$OrderPriority == input$Priority,]
output$plot  <- renderPlot({ 
  superstore.priority[superstore.priority$Region == input$RegionType,] %>% 
    ggvis(~Sales, ~Profit, fill:= "blue", size = ~Profit) %>%
    layer_points() %>% 
    bind_shiny("gg") 
})


#------ For runApp ------

us.dat <- map_data("state")
us.dat$region = paste(toupper(substr(us.dat$region, 1, 1)), substr(us.dat$region, 2, nchar(us.dat$region)), sep="")


SriAppsDir <- "/Users/srigovind/Desktop/Personal/Hadoop/BigDataStack/RProgramming/Week7/shiny/SriApps"
c2_try <-paste(SriAppsDir,"C2_with_navbar_try",sep="/")
runApp(c2_try)

superstore[(superstore$OrderDate >= superstoreDates.startEnd[1]) & (superstore$OrderDate <= "2015-01-02"),]$OrderDate
str(superstore)
format(superstoreDates.startEnd)

superstore %>% 
  ggvis(superstore, ~OrderDate) %>% 
  layer_bars() %>% 
  add_axis("x", title = "Date", title_offset = 50) %>%
  add_axis("y", title = "Number Of products", title_offset = 50)  %>%
  add_tooltip(function(x){if(is.null(x)){return(NULL)}else{paste0(format(x), "<br>")}}, "hover") 

superstore %>% ggvis(~Profit, fill= ~factor(Region)) %>%
  group_by(OrderDate) %>%
  layer_histograms(fill := "blue", binwidth= input_slider(min=1000, max=50000, value=250)) %>%
  layer_smooths(span = 100) # = input_slider(-15000, 10000, value = 100, step = 300, label = "span"))

superstore %>% ggvis(x = ~Sales) %>%
  layer_densities(
    adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment"),
    kernel = input_select(
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel"))

us.dat <- map_data("state")
names(us.dat)
str(superstore)
str(us.dat)

superstore %>% 
  ggvis(~ProductName) %>%
  layer_bars(width=0.09)

superstore %>%
  ggvis(~OrderDate) %>% 
  layer_bars(width=0.09)

superstore %>%
  ggvis(x = ~ProductName) %>%
  layer_histograms(fill := "blue", binwidth = input_slider(min=0, max=15, value=5))

ggplot(superstore, aes(x=ProductName)) + geom_bar()

superstore$OrderID
superstore[superstore$ProductName == "SANFORD Liquid Accent\x99 Tank-Style Highlighters",]$ProductName

fill= ~factor(cyl))
install.packages("plotly")
library(plotly)  
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

plot_ly(superstore[superstore$StateorProvince == "Washington",], x = StateorProvince, y = Profit, text = paste("OrderID: ", OrderID),
        mode = "markers", color = Profit, size = Profit)

plot_ly(superstore, x = OrderDate, autobinx = F, type = "histogram",
        xbins = list(start = range(superstore$OrderDate)[1], end = range(superstore$OrderDate)[2]))
