shinyServer(function(input, output, session) 
  {
  output$plot <- renderPlot({
    if (input$Priority == "all" & input$RegionType == "all") {
    xyplot(Profit ~ Sales | Region, superstore, layout = c(4, 1),
           main = "Store Profit as a Function of Sales",
           panel = function(x, y, ...) 
           {
             panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
             panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
             panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
           })
    }else if(input$Priority != "all" & input$RegionType == "all") {
      superstore.priority <- superstore[superstore$OrderPriority == input$Priority,]
      xyplot(Profit ~ Sales | Region, superstore.priority, layout = c(4, 1),
             main = "Store Profit as a Function of Sales",
             panel = function(x, y, ...) 
             {
               panel.xyplot(x, y, ...) ## First call the default panel function for 'xyplot'
               panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
               panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
             })
    }
    else{
      if (input$Priority != "all") {
        superstore.priority <- superstore[superstore$OrderPriority == input$Priority,]
      }else {
        superstore.priority <- superstore
      }
      superstore.priority[superstore.priority$Region == input$RegionType,] %>% 
        ggvis(~Sales, ~Profit, size = ~Profit, fill = ~Profit ) %>%
        layer_points() %>% hide_legend('fill') %>%
        bind_shiny("ggvis1","ggvis1_ui")
      }
    })
  
  gv <- reactive({
    state.superstore <- superstore[superstore$StateorProvince == input$states,] 
    state.superstore
    }) 
  
  gv %>% 
    ggvis(~CustomerSegment, fill = ~CustomerSegment) %>% 
    layer_bars(width=0.09) %>% 
    add_axis("x", title = "Business Type", title_offset = 50) %>%
    add_axis("y", title = "Quantity", title_offset = 50)  %>%
    # layer_smooths(span = input_slider(0, 50, value = 1, step = 1, label = "span")) %>%
    add_tooltip(function(x){if(is.null(x)){return(NULL)}else{paste0(format(x), collapse = "<br/>")}}, "hover") %>%  
    bind_shiny("ggvis2","ggvis2_ui")
  
  output$trendPlot <- renderPlotly({ 
    plot_ly(superstore[superstore$StateorProvince == input$states,], x = StateorProvince, y = Profit, text = paste("OrderID: ", OrderID),
            mode = "markers", color = Profit, size = Profit)
    })
  
  gv1 <- reactive({
    date.superstore <- superstore[(superstore$OrderDate >= input$superstoreDates[1]) & (superstore$OrderDate <= input$superstoreDates[2]),] }) %>% 
    ggvis(~OrderDate) %>% 
    layer_bars(width=0.09) %>% 
    add_axis("x", title = "Date", title_offset = 50) %>%
    add_axis("y", title = "Number Of products", title_offset = 50)  %>%
    add_tooltip(function(x){if(is.null(x)){return(NULL)}else{paste0(format(x), "<br>")}}, "hover") %>%  
    bind_shiny("ggvis3","ggvis3_ui")
  
  output$title <- renderText({ 
    paste("Plotting ", input$RegionType," Regions")
  })
  output$summary <- renderPrint({summary(superstore)})
  output$table <- renderDataTable({superstore}, options=list(pageLength=5))
 })
