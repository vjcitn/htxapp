library(DT)
library(ssrch)
load("ds_can1009b.rda")
library(restfulSE)
if (!exists("rangedHtxSE")) load("rangedHtxSE.rda")
se = rangedHtxSE
sefilter = function(se, y) se[,which(se$study_accession %in% y)]

 docs = docset  = ds_can1009b
 titles = slot(docset, "titles")
 urls = slot(docset, "urls") # may be empty
#
# order keywords so that those with alphabetic prefix
# precede those with special characters or numbers
#
 allkw = sort(unique(ls(envir=kw2docs(docs))))
 ini = substr(allkw,1,1)
 fullinds = seq_len(length(allkw))
 preferred = grep("[A-Za-z]", ini)
 spec = setdiff(fullinds, preferred)
 allkw = allkw[c(preferred, spec)]
 dlmessage = ifelse(is.null(se), "download list of data.frames",
       "download SE")
#
# done
#
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
#    p(img(src="bioc.png", width="60px", align="top"), em("Cancer43k")),
    h4("Bioconductor:Cancer43K"),
    helpText("Full text search over genomic metadata on 43000 cancer transcriptomes exclusive of TCGA, retrieved from NCBI SRA."),
    selectInput("main", "Search studies for",
     choices = allkw, selected="BRAF"),
    selectInput("keep", "Click on rows of 'titles' table to add studies to cart.",
        choices=names(titles), multiple=TRUE),
    helpText("A ", a(href="https://f1000research.com/articles/8-21/v1", target="_blank","restfulSE"),
"is returned; RNA-seq quantifications by ", a(href="https://combine-lab.github.io/salmon/", target='_blank',"salmon.")),
    downloadButton("downloadData", dlmessage),
    actionButton("cleartabs", "Clear tabs."),
    actionButton("cleartitles", "Clear titles."),
#    actionButton("clearcart", "Clear cart."),
    actionButton("stopBtn", "Stop app."),
           width=3
    ),
   mainPanel(
    helpText("Tabs will appear for studies using selected terms in metadata"),
    helpText("Click on tab to see sample.attributes for all experiments in the study, derived with SRAdbV2"),
    tabsetPanel(id="tabs",
     tabPanel("titles", target="titles",
      DT::dataTableOutput("titleTable")
     ),
     tabPanel("about",
      helpText("This app demonstrates an approach to supporting full text search over genomic metadata recorded in the NCBI SRA."),
      helpText("A snapshot of cancer-related metadata was retrieved
in March 2019 using the ", a(href="https://api-omicidx.cancerdatasci.org/sra/1.0/ui/","Omicidx system")," of Sean Davis of NCI."),
      helpText("1009 studies with cancer-related metadata are searchable with this app.  Filtered metadata is collected in a DocSet structure defined in the ssrch packkage.  A view of this object is shown below."),
      verbatimTextOutput("objdesc"),
      helpText("Special methods for organizing and searching the metadata are warranted by the fact that diverse field sets and value sets are used across and even within studies.  Retrieval and partial normalization of metadata from SRAdbV2 is conducted using code in the HumanTranscriptomeCompendium package, available at github.com/vjcitn."),
      helpText("This app also simplifies acquisition of gene-level quantifications
from studies of interest.  A restful SummarizedExperiment instance is generated
for the studies selected in the session.  The quantifications are stored
in Amazon Web Services, in the HDF Scalable Data Service, thanks to
John Readey of the HDF Group."),
      helpText("Development supported by NCI ITCR U01 CA214846"),
      helpText("The software stack underlying ssrch is:"),
      verbatimTextOutput("sessInf")
     )
    )
   )
  )
 )

