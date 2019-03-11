library(shiny)
library(htxapp)
data(n1)
data(studyTitles)
nstud = length(ls(n1$studenv))
nexp = length(ls(n1$expenv))
ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
     helpText(h2("htxcomp study harvester: cancer prototype")),
     helpText(sprintf("%d experiments from %d studies", nexp, nstud)),
     helpText("Sample-level data is derived from a March 2019 snapshot of all human transcriptomic studies with
metadata available through SRAdbV2."),
     helpText("Field sets and field names vary across studies and even within studies.  Field sets
were lightly harmonized using the sampleAttr function of htxcomp."),
     helpText("Use the search field to query the metadata snapshot for studies of interest.
Type the study identifier in the 'add to cart' field and press return to save the study for
retrieval."),
     selectInput("a", "Search all sample.attribute fields", choices=c(ls(n1$kwexenv)), 
       selected="triple-negative breast cancer (TNBC)", multiple=FALSE),
     selectInput("cart", "add to cart", choices=ls(n1$studenv),
       selected="SRP066982", multiple=TRUE),
     downloadButton("downloadData", "get SummarizedExperiment"),
     actionButton("btnSend", "Stop app")
     ),
   mainPanel(
    tabsetPanel(
     tabPanel("basics",
      helpText(h4("Studies in cart")),
      tableOutput("b"),
      #helpText("studies"),
      #verbatimTextOutput("c"),
      helpText(h4("All sample-level information in current study (from SRAdbV2)")),
      dataTableOutput("e")
      ),
     tabPanel("about",
      helpText("The htxcomp package utilized Sean Davis' BigRNA project to create
images of 181000 RNA-seq studies, retrieve in 2017 from NCBI SRA, and uniformly
processed by the salmon pipeline."),
      helpText("htxcomp provides limited metadata about the samples in the
compendium.  Sean's SRAdbV2 package provides high-resolution metadata for
almost all samples in the htxcomp compendium.  (Some samples simply lack
usable metadata entries at SRA.)  The limited metadata is provided in the colData of the
result of htxcomp::loadHtxcomp().  A major limitation is the lack of any information
on experimental state of assayed samples."),
      helpText("Sample-level metadata available from SRAdbV2 reflects the complex structure of information
in the NCBI SRA metadata store.  Fields and value sets vary between studies and
can vary between experiments within studies.  The htxcomp and htxapp packages perform
some normalization of field sets within studies, and provide access to all
information in the sample.attributes fields returned by SRAdbV2 for studies selected
using htxapp."),
       helpText("The sample-level metadata are filtered to match samples actually
available in the htxcomp compendium, and are then added to the metadata component
of the htxcomp-generated SummarizedExperiment, one data.frame per study selected.")
   )
  )
  )
)
)
