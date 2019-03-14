#' plot traces for the 'paired' or 'tripled' design
#' @param featuresym character(1) name of an assay (rowname of SummarizedExperiment)
#' @param se SummarizedExperiment instance
#' @param tx function that transforms numerical input to desired scale for plotting
#' @param id character(1) name of subject identifier, a vector in colData that
#' partitions its rows into the collections of contributions from each subject
#' @param strat character(1) name of variable in colData that gives the
#' type of each of the repeated measures
#' @examples
#' data(hccLinc4)
#' requireNamespace("ggplot2")
#' feature_trace("TERC", hccLinc4) + xlab(" ") + guides(colour="none")
#' @export
feature_trace = function(featuresym, se, tx=function(x) log(x+1),
    id="patient.id", strat="source_name") {
  stopifnot(all(c(id, strat) %in% names(colData(se))))
  stopifnot(ncol(rowData(se))>0) # must have nontrivial rowData
  ind = grep(featuresym, rowData(se)$gene_name)
  if (length(ind)>1) message(featuresym, "matches multiple features, using first")
  ind = ind[1]
  ensid = rownames(rowData(se))[ind]
  nass = tx(as.numeric(assay(se[ensid,])))
  dd = data.frame(nass, colData(se)[,c(id, strat)])
  names(dd)[1] = featuresym
  ggplot(dd, aes_string(y=featuresym, x=strat, group=id)) + geom_point() +
    geom_path(aes_string(colour=id))
}

