library(shiny)
data("n1", package="htxapp")
data("studyTitles", package="htxapp")
nstud = length(ls(n1$studenv))
nexp = length(ls(n1$expenv))
hh = htxcomp::loadHtxcomp()
server = function(input, output) {
  output$b = renderDataTable(data.frame(st=input$cart, title=studyTitles[input$cart], stringsAsFactors=FALSE))
  output$c = renderPrint(unique(get(input$a, env=n1$kwstenv)))
#  output$d = renderPrint(unique(get(input$a, env=n1$kwexenv)))
  output$e = renderDataTable( {
              targs = mget(input$a, env=n1$kwstenv)
              if (length(targs[[1]])>1) message("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first")
              targ = targs[[1]][1]
              z = read.csv(system.file(paste0("csv/", targ,".csv"), package="htxapp"), stringsAsFactors=FALSE)
              z })
     observe({
                    if(input$btnSend > 0)
                        isolate({
                           stopApp(returnValue=hh[,which(hh$study_accession %in% input$cart)])
                        })  
           })  

}
