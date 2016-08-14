# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)

library(DT)



options(shiny.maxRequestSize=100*1024^2) # max file size is 100Mb
shinyServer(function(input,output,session){

  
####################
#GOenrichment plotting functiuon
####################
datasetInput <- reactive({ 
                            
example<-read.table("data/TBtools.level2.counts.table.xls",header=T,sep="\t")
 	inFile <- input$file1
  if (!is.null(inFile)){
       
      data<-read.table(inFile$datapath, header=input$header, sep=input$sep, stringsAsFactors=F) 
   
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
  
  GeneCount=tbtoolsCounts[2,6]
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
#download  plot option    
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
          postscript(file)
          print(GOplotfunction())
          dev.off()
        },
        contentType = 'image/eps'
      )
#################################



      
      
####################
#KEGG plotting functiuon
####################
      datasetInput2 <- reactive({ 
        
        example<-read.table("data/TBtools.KEGG.enrichment.result.xls",header=T,sep="\t")
        inFile <- input$file2
        if (!is.null(inFile)){
          
          data<-read.table(inFile$datapath, header=input$header2, sep=input$sep2, stringsAsFactors=F) 
          
        }
        switch(input$dataset2,
               "example" = example,
               "upload" = data
        )	
        
      })
      
      
      ##enrichment KEGG data 
      
      # output upload dataset or example dataset  
      output$summaryKegg <- DT::renderDataTable(datasetInput2()[,-8])
        
      
      ################
      ##plot kegganalysis outputfunction
      ################
      
      KEGGplotfunction<-function(){
        enrichmentInfo=datasetInput2()
        enrichmentInfoMod<-enrichmentInfo[enrichmentInfo$corrected.p.value.BH.method.<=5e-2 & enrichmentInfo$GeneHitsInSelectedSet>=5,]
        colnames(enrichmentInfoMod)<-c("ID","NumberOfGeneInSelectedSet","AllGenesInSelectedSet","GeneHitsInBackground","AllGenesInBackground","p-value","EnrichmentFactor","GeneListInSelectedSets","p_adjust")
        enrichmentInfoMod$ID<-gsub("^\\s*\\d{5}\\s+","",enrichmentInfoMod$ID,perl=T)
        p<-ggplot(enrichmentInfoMod)+
          geom_point(aes(x=EnrichmentFactor,y=ID,size=NumberOfGeneInSelectedSet,color=p_adjust)) +theme_bw()+
          theme(
            plot.title=element_text(face="bold",vjust=1.0),
            axis.title.x=element_text(face="bold",vjust=-0.2),
            axis.title.y=element_text(face="bold"),
            axis.text.y=element_text(hjust=1.0,colour="black"),
            axis.text.x=element_text(angle=0,colour="black")
          )+ggtitle("Statistics of KEGG Pathway Enrichment")+
          ylab("Pathway")+
          xlab("Enrichment Factor")
        if(input$theme=="Tufte"){
          p=p+ theme_tufte()
        }else if(input$theme=="Economist"){
          p=p+  scale_color_economist()
        }else if(input$theme=="Solarized"){
          p=p+  scale_color_solarized("blue")
        }else if(input$theme=="Stata"){
          p=p+ scale_color_stata()
        }else if(input$theme=="Excel 2003"){
          p=p+  scale_color_excel()
        }else if(input$theme=="Inverse Gray"){
          p=p+ theme_igray()
        }else if(input$theme=="Fivethirtyeight"){
          p=p+theme_fivethirtyeight()
        }else if(input$theme=="Tableau"){
          p=p+ scale_color_tableau()
        }else if(input$theme=="Stephen"){
          p=p+ scale_color_few()
        }else if(input$theme=="Wall Street"){
          p=p+ scale_color_wsj("colors6", "")
        }else if(input$theme=="GDocs"){
          p=p+ scale_color_gdocs()
        }else if(input$theme=="Calc"){
          p=p+ scale_color_calc()
        }else if(input$theme=="Pander"){
          p=p+ scale_color_pander()
        }else if(input$theme=="Highcharts"){
          p=p+ scale_color_hc()
        }
        
        
      }
      
      output$KEGGplotRender<-renderPlot({
        print(KEGGplotfunction())
      },width = 800,height=800
      )
      
      
      
      ################################
      #download  plot option    
      output$downloadDataKEGGPNG <- downloadHandler(
        filename = function() {
          paste("output", Sys.time(), '.png', sep='')
        },
        
        content = function(file) {
          png(file, type="cairo",units="in",width = 10, height = 10,pointsize=5.2,res=300)
          #png(file=file, width = 600, height = 600, units = "px", pointsize = 12, bg = "white", res = NA)
          print(KEGGplotfunction())
          dev.off()
        },
        contentType = 'image/png'
      )
      
      
      output$downloadDataKEGGPDF <- downloadHandler(
        filename = function() {
          paste("output", Sys.time(), '.pdf', sep='')
        },
        
        content = function(file) {
          pdf(file)
          print(KEGGplotfunction())
          dev.off()
        },
        contentType = 'image/pdf'
      )
      
      output$downloadDataKEGGEPS <- downloadHandler(
        filename = function() {
          paste("output", Sys.time(), '.eps', sep='')
        },
        
        content = function(file) {
          setEPS()
          postscript(file)
          print(KEGGplotfunction())
          dev.off()
        },
        contentType = 'image/eps'
      )
      #################################      


})



