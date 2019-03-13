library(shiny)
data("cancer75indices", package="htxapp")  # cancer75indices is the serialized environment set built from 68 cancer studies
data("studyTitles", package="htxapp")
nstud = length(ls(cancer75indices$studenv))
nexp = length(ls(cancer75indices$expenv))
library(restfulSE)  # this is slow
hh = readRDS("rangedHtxGeneSE.rds")  # this is slow
server = function(input, output) {

  output$alltitles = renderDataTable({
   allst = ls(cancer75indices$studenv)
   hlf = hh[,which(hh$study_accession %in% allst)]
   hl = hlf[,-which(duplicated(hlf$study_accession))]
   nn = data.frame(acc=hl$study_accession, title=hl$study_title)
   tt = table(hlf$study_accession)
   tdf = data.frame(acc=names(tt), nsamp=as.numeric(tt))
   merge(nn, tdf, by="acc")
   })

  output$b = renderTable(data.frame(st=input$cart, title=studyTitles[input$cart], stringsAsFactors=FALSE))
  output$c = renderPrint(unique(get(input$a, env=cancer75indices$kwstenv)))
  output$e = renderDataTable( {
              targs = mget(input$a, env=cancer75indices$kwstenv)
              if (length(targs[[1]])>1) {
                 message("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first")
                 showNotification(paste0("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first"), type="message")
                 }
              targ = targs[[1]][1]
              z = read.csv(system.file(paste0("can10kcsv/", targ,".csv"), package="htxapp"), stringsAsFactors=FALSE)
              z })
     observe({
              if(input$btnSend > 0)
                isolate({
                  ans = hh[,which(hh$study_accession %in% input$cart)]
                  md = lapply(input$cart, function(x)
                  read.csv(system.file(paste0("can10kcsv/", x,".csv"), package="htxapp"), stringsAsFactors=FALSE))
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
                read.csv(system.file(paste0("can10kcsv/", x,".csv"), package="htxapp"), stringsAsFactors=FALSE))
                names(md) = paste0("sampleAtts", input$cart)
                metaids = unlist(lapply(md, function(x) x$experiment.accession))
                okids = intersect(colnames(ans), metaids)
                md = lapply(md, function(x) x[which(x$experiment.accession %in% okids),])
                ans = ans[,okids]
                S4Vectors::metadata(ans) = c(S4Vectors::metadata(ans), md)
                saveRDS(ans, file=con)
                }, contentType="application/octet-stream"
               )
    output$sessinf = renderPrint(sessionInfo())

}
