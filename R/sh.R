z = function() {
#library(shiny)
#load("n1.rda")
#load("studyTitles.rda")
#nstud = length(ls(n1$studenv))
#nexp = length(ls(n1$expenv))
#hh = htxcomp::loadHtxcomp()
#ui = fluidPage(
#  sidebarLayout(
#   sidebarPanel(
#     helpText(h2("htxcomp study harvester")),
#     helpText(sprintf("%d experiments from %d studies", nexp, nstud)),
#     helpText("Sample-level data is derived from a March 2019 snapshot of all human transcriptomic studies with
#metadata available through SRAdbV2."),
#     helpText("Field sets and field names vary across studies and even within studies.  Field sets
#were lightly harmonized using the sampleAttr function of htxcomp."),
#     helpText("Use the search field to query the metadata snapshot for studies of interest.
#Type the study identifier in the 'add to cart' field and press return to save the study for
#retrieval."),
#     selectInput("a", "Search all sample.attribute fields", choices=c(ls(n1$kwexenv)), 
#       selected=ls(n1$kwexenv)[1], multiple=FALSE),
#     selectInput("cart", "add to cart", choices=ls(n1$studenv),
#       selected=ls(n1$studenv)[1], multiple=TRUE),
#     actionButton("btnSend", "get SummarizedExperiment")
#     ),
#   mainPanel(
##    verbatimTextOutput("b"),
#    dataTableOutput("b"),
#    helpText("studies"),
#    verbatimTextOutput("c"),
##    helpText("expts"),
##    verbatimTextOutput("d")
#    dataTableOutput("e")
#   )
#  )
#  )
#server = function(input, output) {
#  output$b = renderDataTable(data.frame(st=input$cart, title=studyTitles[input$cart], stringsAsFactors=FALSE))
#  output$c = renderPrint(unique(get(input$a, env=n1$kwstenv)))
##  output$d = renderPrint(unique(get(input$a, env=n1$kwexenv)))
#  output$e = renderDataTable( {
#              targs = mget(input$a, env=n1$kwstenv)
#              if (length(targs[[1]])>1) message("keyword hits studies ", paste(unlist(targs), collapse=", "), "; using first")
#              targ = targs[[1]][1]
#              z = read.csv(paste0(targ,".csv"), stringsAsFactors=FALSE)
#              z })
#     observe({
#                    if(input$btnSend > 0)
#                        isolate({
#                           stopApp(returnValue=hh[,which(hh$study_accession %in% input$cart)])
#                        })  
#           })  
#
#}
#runApp(list(ui=ui, server=server))
}
