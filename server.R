# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)

library(DT)



options(shiny.maxRequestSize=100*1024^2) # max file size is 100Mb
shinyServer(function(input,output,session){

 datasetInput <- reactive({ 
                            
example<-read.table("data/TBtools.level2.counts.table.xls",header=T,sep="\t")
#example<-read.table("/srv/shiny-server/PCAonline/test.txt",header=F,sep="\t",row.names=1)
 	inFile <- input$file1
  if (!is.null(inFile)){
       
      data<-read.table(inFile$datapath, header=input$header, sep=input$sep, stringsAsFactors=F) 
      data<-factor2numeric(data)
   			
   
  }
 switch(input$dataset,
          "example" = example,
          "upload" = data
          )	
	})

 
 ##enrichment GO data 
 processedGOdata<-reactive({ 
   tbtoolsCounts=datasetInput()
   if(input$Issorted){
     
     tbtoolsCounts<-tbtoolsCounts[order(tbtoolsCounts$Counts,decreasing=F),]
     tbtoolsCounts<-tbtoolsCounts[order(tbtoolsCounts$Class,decreasing=T),]
   }
   tbtoolsCounts
   })
 
 # output upload dataset or example dataset  
output$summaryGO <- DT::renderDataTable(processedGOdata())


################
##plot goanalysis outputfunction
################

GOplotfunction<-function(){
  tbtoolsCounts<-processedGOdata()
  tbtoolsCounts$Description<-factor(tbtoolsCounts$Description,unique(as.character(tbtoolsCounts$Description)))
  
  GeneCount=tbtoolsCounts[2,5]
  p<-ggplot(tbtoolsCounts)+
    geom_bar(aes(x=Description,y=Counts/GeneCount*100,fill=Class),stat="identity")+
    geom_text(aes(x=Description,y=(Counts+max(tbtoolsCounts$Counts)*0.06)/GeneCount*100,label=Counts))+
    
    coord_flip()+theme_bw()+theme(
      legend.position = c(.8, .1),
      panel.grid.major=element_blank() ,
      panel.grid.minor=element_blank() ,
      panel.background=element_blank() # ,
      
    )+ xlab("GO Decription")+ylab("Percent (%)")
    if(input$theme=="Tufte"){
      p=p+ theme_tufte()
    }else if(input$theme=="Economist"){
      p=p+  scale_fill_economist()
    }else if(input$theme=="Solarized"){
      p=p+  scale_fill_solarized("blue")
    }else if(input$theme=="Stata"){
      p=p+ scale_fill_stata()
    }else if(input$theme=="Excel 2003"){
      p=p+  scale_fill_excel()
    }else if(input$theme=="Inverse Gray"){
      p=p+ theme_igray()
    }else if(input$theme=="Fivethirtyeight"){
      p=p+theme_fivethirtyeight()
    }else if(input$theme=="Tableau"){
      p=p+ scale_fill_tableau()
    }else if(input$theme=="Stephen"){
      p=p+ scale_fill_few()
    }else if(input$theme=="Wall Street"){
      p=p+ scale_fill_wsj("colors6", "")
    }else if(input$theme=="GDocs"){
      p=p+ scale_color_gdocs()
    }else if(input$theme=="Calc"){
      p=p+ scale_color_calc()
    }else if(input$theme=="Pander"){
      p=p+ scale_fill_pander()
    }else if(input$theme=="Highcharts"){
      p=p+ scale_fill_hc()
    }
    
   
}

output$GOplotRender<-renderPlot({
  print(GOplotfunction())
},width = 800,height=800
)

 

################################
#download 2D plot option    
      output$downloadDataPNG <- downloadHandler(
        filename = function() {
          paste("output", Sys.time(), '.png', sep='')
        },
        
        content = function(file) {
          png(file, type="cairo",units="in",width = 10, height = 10,pointsize=5.2,res=300)
          #png(file=file, width = 600, height = 600, units = "px", pointsize = 12, bg = "white", res = NA)
          print(GOplotfunction())
          dev.off()
        },
        contentType = 'image/png'
      )
      
      
      output$downloadDataPDF <- downloadHandler(
        filename = function() {
          paste("output", Sys.time(), '.pdf', sep='')
        },
        
        content = function(file) {
          pdf(file)
          print(GOplotfunction())
          dev.off()
        },
        contentType = 'image/pdf'
      )
      
      output$downloadDataEPS <- downloadHandler(
        filename = function() {
          paste("output", Sys.time(), '.eps', sep='')
        },
        
        content = function(file) {
          setEPS()
          postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special")
          print(GOplotfunction())
          dev.off()
        },
        contentType = 'image/eps'
      )
#################################





})



