library(ggplot2)
library(janitor)
library(shiny)
library(gtrendsR)
data("countries")
data("categories")

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({ 

  s1<-input$s1
  s2<-input$s2
  gtype<-input$gtype
  countr<-input$countr
  input$subm  
  
  trend<-gtrends(c(input$s1,input$s2), geo = c(input$countr, input$countr),gprop=input$gtype)
  trend_city<-data.frame(trend$interest_by_city)
  
  hit<-as.integer(trend_city$hits)
  df_hit<-data.frame("location"=trend_city$location,"hits"=hit,"keyword"=trend_city$keyword)
  for(i in 1:nrow(df_hit))
  {
    if(is.na(df_hit$hits[i])){
      df_hit$location[i]=NA
      df_hit$keyword[i]=NA
    }
  }
  df_hit<-remove_empty(df_hit, which = c("rows", "cols"))
  df <- data.frame(city=df_hit$location,
                   hits=df_hit$hits,key=df_hit$keyword)
  ggplot(data=df, aes(x=city, y=hits,fill=key)) +
    geom_bar(stat="identity")+ coord_flip()+theme_minimal()
  })

})