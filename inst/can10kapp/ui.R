library(shiny)
library(HumanTranscriptomeCompendium)
library(htxapp)
#data(cancer75indices)
load("cancer75indices.rda")
data(studyTitles)
nstud = length(ls(cancer75indices$studenv))
nexp = length(ls(cancer75indices$expenv))
#library(SummarizedExperiment)
#hh = readRDS("rangedHtxGeneSE.rds")
load("studacc.rda")
allst = ls(cancer75indices$studenv)
navail = sum(studacc %in% allst)
ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
     helpText(h3("Human Transcriptome Compendium study harvester: cancer prototype")),
     helpText(sprintf("This app provides access to annotation and
quantification from %d studies (%d experiments) for which a 
cancer-related keyword was found in study title.", nstud, navail)),
     helpText("Sample-level data is derived from a March 2019 snapshot of all human transcriptomic studies with
metadata available through SRAdbV2."),
     helpText("Field sets and field names vary across studies and even within studies.  Field sets
were lightly harmonized using the sampleAttr function of the HumanTranscriptomeCompendium package."),
     helpText("Use the search field to query the metadata snapshot for studies of interest.
Type the study identifier in the 'add to cart' field and press return to save the study for
retrieval."),
     selectInput("a", "Search all sample.attribute fields", choices=c(ls(cancer75indices$kwexenv)), 
       selected="triple-negative breast cancer (TNBC)", multiple=FALSE),
     selectInput("cart", "add to cart", choices=ls(cancer75indices$studenv),
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
     tabPanel("all studies", dataTableOutput("alltitles")),
     tabPanel("about",
      helpText("Annotation derived from Sean Davis' ", a(href="https://api-omicidx.cancerdatasci.org/sra/1.0/ui/", "Omicidx API.")),
      helpText("The HumanTranscriptomeCompendium package (at github: vjcitn) utilizes Sean Davis' BigRNA project to create
images of 181000 RNA-seq studies, retrieve in 2017 from NCBI SRA, and uniformly
processed by the ", a(href="https://combine-lab.github.io/salmon/", "salmon pipeline.")),
      helpText("HumanTranscriptomeCompendium's `htx_load()` provides limited metadata about the samples in the
compendium.  Sean's SRAdbV2 package provides high-resolution metadata for
almost all samples in the compendium.  (Some samples simply lack
usable metadata entries at SRA.)  The limited metadata is provided in the colData
of the SummarizedExperiment returned by `htx_load()`. 
A major limitation is the lack of any information
on experimental state of assayed samples."),
      helpText("Sample-level metadata available from SRAdbV2 reflects the complex structure of information
in the NCBI SRA metadata store.  Fields and value sets vary between studies and
can vary between experiments within studies.  The HumanTranscriptomeCompendium and htxapp packages perform
some normalization of field sets within studies, and provide access to all
information in the sample.attributes fields returned by SRAdbV2 for studies selected
using htxapp."),
       helpText("The sample-level metadata are filtered to match samples actually
available in the HumanTranscriptomeCompendium compendium, and are then added to the metadata component
of the HumanTranscriptomeCompendium-generated SummarizedExperiment, one data.frame per study selected."),
       verbatimTextOutput("sessinf")
   )
  )
  )
)
)
