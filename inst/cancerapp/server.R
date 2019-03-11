library(shiny)
data("n1", package="htxapp")  # n1 is the serialized environment set built from 20 cancer studies
data("studyTitles", package="htxapp")
nstud = length(ls(n1$studenv))
nexp = length(ls(n1$expenv))
hh = htxcomp::loadHtxcomp()
server = function(input, output) {
  output$b = renderTable(data.frame(st=input$cart, title=studyTitles[input$cart], stringsAsFactors=FALSE))
  output$c = renderPrint(unique(get(input$a, env=n1$kwstenv)))
  output$e = renderDataTable( {
              targs = mget(input$a, env=n1$kwstenv)
              if (length(targs[[1]])>1) message("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first")
              targ = targs[[1]][1]
              z = read.csv(system.file(paste0("cancercsv/", targ,".csv"), package="htxapp"), stringsAsFactors=FALSE)
              z })
     observe({
              if(input$btnSend > 0)
                isolate({
                  ans = hh[,which(hh$study_accession %in% input$cart)]
                  md = lapply(input$cart, function(x)
                  read.csv(system.file(paste0("cancercsv/", x,".csv"), package="htxapp"), stringsAsFactors=FALSE))
                  names(md) = paste0("sampleAtts", input$cart)
                  metaids = unlist(lapply(md, function(x) x$experiment.accession))
                  okids = intersect(colnames(ans), metaids)
                  md = lapply(md, function(x) x[which(x$experiment.accession %in% okids),])
                  ans = ans[,okids]
                  S4Vectors::metadata(ans) = c(S4Vectors::metadata(ans), md)
                  stopApp(returnValue=ans)
                  })  
              })  
    output$downloadData <- downloadHandler(
              filename = function() {
                paste('filteredSE-', Sys.Date(), '.rds', sep='')
                },  
              content = function(con) {
                ans = hh[,which(hh$study_accession %in% input$cart)]
                md = lapply(input$cart, function(x)
                read.csv(system.file(paste0("cancercsv/", x,".csv"), package="htxapp"), stringsAsFactors=FALSE))
                names(md) = paste0("sampleAtts", input$cart)
                metaids = unlist(lapply(md, function(x) x$experiment.accession))
                okids = intersect(colnames(ans), metaids)
                md = lapply(md, function(x) x[which(x$experiment.accession %in% okids),])
                ans = ans[,okids]
                S4Vectors::metadata(ans) = c(S4Vectors::metadata(ans), md)
                saveRDS(ans, file=con)
                }   
               )

}
