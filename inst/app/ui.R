library(shiny)
library(htxapp)
data(n1)
data(studyTitles)
nstud = length(ls(n1$studenv))
nexp = length(ls(n1$expenv))
ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
     helpText(h2("htxcomp study harvester")),
     helpText(sprintf("%d experiments from %d studies", nexp, nstud)),
     helpText("Sample-level data is derived from a March 2019 snapshot of all human transcriptomic studies with
metadata available through SRAdbV2."),
     helpText("Field sets and field names vary across studies and even within studies.  Field sets
were lightly harmonized using the sampleAttr function of htxcomp."),
     helpText("Use the search field to query the metadata snapshot for studies of interest.
Type the study identifier in the 'add to cart' field and press return to save the study for
retrieval."),
     selectInput("a", "Search all sample.attribute fields", choices=c(ls(n1$kwexenv)), 
       selected=ls(n1$kwexenv)[1], multiple=FALSE),
     selectInput("cart", "add to cart", choices=ls(n1$studenv),
       selected=ls(n1$studenv)[1], multiple=TRUE),
     actionButton("btnSend", "get SummarizedExperiment")
     ),
   mainPanel(
    dataTableOutput("b"),
    helpText("studies"),
    verbatimTextOutput("c"),
    dataTableOutput("e")
   )
  )
  )
