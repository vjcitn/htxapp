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
accumtitles=NULL
accumTokens=NULL

 server = function(input, output, session) {
  output$objdesc = renderPrint( docs )
#
# retrieve requested documents
#
#  getTabs = reactive({
#    z = searchDocs(input$main, docs, ignore.case=TRUE)
#    lapply(z$docs, function(x) retrieve_doc(x, docs))
#    })
#
# render a table of titles of selected documents
#
  buildTitleTable = reactive({
   z = searchDocs(input$main, docs, ignore.case=TRUE)
   if (nrow(z)>1 && sum(dd <- duplicated(z$docs))>0) {
      sz = split(z, z$docs)
      kp = sapply(sz, function(x) which.max(nchar(x$hits)))
      for (i in seq_len(length(sz))) sz[[i]] = sz[[i]][kp[i],,drop=FALSE]
      z = do.call(rbind, sz)
      }
   if (is.null(accumtitles)) accumtitles <<- cbind(z, title=titles[z$docs])
   else accumtitles <<- rbind(accumtitles, cbind(z, title=titles[z$docs]))
   d = which(duplicated(accumtitles$docs))
   if (length(d)>0) accumtitles <<- accumtitles[-d,]
   mkl = function(x) sprintf("<a href=%s target='_blank'>%s</a>",x,gsub(".*=", "", x))
   if (length(urls)>0) accumtitles = cbind(pmid=mkl(urls[accumtitles$docs]),
     accumtitles)
   rownames(accumtitles) = NULL
   names(accumtitles)[3] = "study"
   accumtitles
  })
#
# append titles, tabs as requested
#
  tabStack = NULL
  observeEvent(input$main, {
    z = searchDocs(input$main, docs, ignore.case=TRUE)
    lapply(rev(unique(z$docs)), function(x) {
      tabStack <<- c(tabStack, x)
      insertTab("tabs", tabPanel(x, {
        renderDataTable(retrieve_doc(x, docs))}, id=x),
        target="titles", position="after")})
    output$titleTable = DT::renderDataTable( 
              datatable(buildTitleTable(), escape=FALSE ))
    })
  observeEvent(input$titleTable_rows_selected, {
       newt = accumtitles$docs[ input$titleTable_rows_selected ]      
       accumTokens <<- unique(c(accumTokens, newt))
       updateSelectInput(session, "keep", selected=accumTokens)
       })
  observeEvent(input$cleartabs, {
    showNotification("After clearing you must change the query string or displays will not update.")
    for (i in tabStack) removeTab("tabs", target=i) 
    tabStack <<- NULL
    })
  observeEvent(input$cleartitles, {
    showNotification("After clearing you must change the query string or displays will not update.")
    accumtitles <<- NULL
    output$titleTable = DT::renderDataTable( datatable(data.frame()) ) #buildTitleTable() )
    })
#  observeEvent(input$clearcart, {
#    accumTokens <<- NULL
#    updateSelectInput(session, "keep", selected=NULL)
#    })

     observeEvent(input$stopBtn, {
       ans = NULL
       if (length(input$keep)>0) {
          ans = lapply(input$keep, function(x) retrieve_doc(x, docs))
          names(ans) = input$keep
          }
       stopApp(returnValue=ans)
       })
     output$sessInf = renderPrint( sessionInfo() )

     output$downloadData <- downloadHandler(
              filename = function() {
                msg = ifelse(is.null(se), "listOfDFs-", "SE-")
                paste(msg, Sys.Date(), '.rds', sep='')
                },  
              content = function(con) {
                md = lapply(input$keep, function(x) retrieve_doc(x, docs))
                names(md) = input$keep
                if (is.null(se)) {
                   ans = md
                   } else {
                   ans = sefilter(se, input$keep)
                   md = lapply(md, function(x) x[which(x$experiment.accession %in%
                          colnames(ans)),])
                   metadata(ans) = c(metadata(ans), md)
                   }
                saveRDS(ans, file=con)
                }, contentType="application/octet-stream"
               )    

 }
