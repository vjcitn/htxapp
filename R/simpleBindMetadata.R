#' for an htxapp download involving only one study, bind the sample.attributes information
#' to the colData 
#' @param se a SummarizedExperiment instance
#' @note htxapp returns a SummarizedExperiment instance with additional data.frames in the
#' metadata() list.  For the case of one study returned, there is only one such data.frame
#' and it is clear how to combine the information in it with the metadata already in colData.
#' When multiple studies are selected, there will likely be different field sets in each
#' sample.attributes component, so build a unified colData requires manual work.
#' @export
simpleBindMetadata = function(se) {
 nsa = grep("sampleAtts", names(metadata(se)))
 if (length(nsa)!=1) stop("only works for a single sampleAtts component")
 md = metadata(se)[[nsa]]
 de = duplicated(md$experiment.accession)
 if (sum(de)>0) {
   message("experiment.accession contains duplicates that are deleted.")
   md = md[-which(de),]
   }
 rownames(md) = md$experiment.accession
 okid = intersect(rownames(md), colnames(se))
 cd = cbind(colData(se[,okid]), md[okid,])
 se = se[,okid]
 colData(se) = cd
 se
}
