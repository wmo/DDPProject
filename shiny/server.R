library(shiny)

countryData<-readRDS("countrydata.rds")
yrange=range(cbind(countryData$le_birth_m, countryData$le_birth_f, countryData$le_birth))
yrange[2]=1.1*yrange[2] # keep some space for the legend

shinyServer(
  function(input, output) {
    output$countryChart <- renderPlot({ 
        le1<-countryData[countryData$country==input$country1,]
        le2<-countryData[countryData$country==input$country2,]

        col1="#787A40"
        col2="#b2577a"
        par(mar=c(5,3,2,3))
        with( le1, plot( year, le_birth_f, col="salmon",type="b", pch=19, ylim=yrange,
                         ylab='Life Expectancy', xlab='', main='Life expectancy at birth' ))
        par(new=T)
        with( le1, plot( year, le_birth_m, col="blue",type="b", pch=19, ylim=yrange,
                         axes=F, xlab='', ylab=''))
        par(new=T)
        with( le1, plot( year, le_birth, col=col1,type="b", pch=19, lwd=3, ylim=yrange,
                         axes=F, xlab='', ylab=''))

        par(new=T)
        with( le2, plot( year, le_birth_f, col="salmon",type="b", pch=8, ylim=yrange,
                     axes=F, xlab='', ylab=''))
        par(new=T)
        with( le2, plot( year, le_birth_m, col="blue",type="b", pch=8, ylim=yrange,
                         axes=F, xlab='', ylab=''))
        par(new=T)
        with( le2, plot( year, le_birth, col=col2,type="b", pch=8, lwd=3, ylim=yrange,
                         axes=F, xlab='', ylab=''))

        legend("topright", pch=19, lty=1, lwd=2, col = c(col1,"blue","salmon"), 
               legend = c(input$country1, "♂","♀") ) 
        legend("topleft", pch=8, lty=1, lwd=2, col = c(col2,"blue","salmon"), 
               legend = c(input$country2, "♂","♀") ) 

        },width=700, height=500)

    output$datatable <- renderDataTable({
        le<-countryData[countryData$country %in% c(input$country1, input$country2),1:8] 
        names(le)<-c("Country","Year","Birth ♂","Birth ♀","Birth","60 ♂","60 ♀","60")
        le
        }, options = list( pageLength = 6, paging=F,searching=F)  ) 
  }
)
