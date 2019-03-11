#' utility to create hashtables (environments) linking study/experiment/keyword
#' for use in searching metadata related to htxcomp elements
#' @param csv character(1) path to a CSV-formated file assumed to have fields study.accession, experiment.accession
#' @param envset a list of environments with elements expenv, studenv, kwexenv, kwstenv
#' @param expenv an environment
#' @param studenv an environment
#' @param cleanFields a list with regular expressions as elements; fields matching elements of the
#' list will not contributes tokens for searching
#' @note Purpose is to create and add content to maps between tokens in metadata and the studies and experiments
#' that bear them.  "Stopwords" are removed.
#' @export
csv2envs = function(csv, envset=NULL, 
    expenv=new.env(hash=TRUE), studenv=new.env(hash=TRUE),
    cleanFields = list(".*id$", ".name$", "_name$")) {
#
# set up 2 environments: 
# experiment to keyword, study to keyword
#
 if (!is.null(envset)) {
   expenv = envset$expenv
   studenv = envset$studenv
   kwexenv = envset$kwexenv
   kwstenv = envset$kwstenv
   }
#
# import CSV derived from htxcomp::sampleAtts, remove duplicate records,
# bind experiment id as rownames
#
 dat = read.csv(csv, stringsAsFactors=FALSE)
 nn = names(dat)
 todrop = unique(unlist(lapply(cleanFields, function(x) grep(x, nn))))
 if (length(todrop)>0) dat = dat[,-todrop]
 dd = which(duplicated(dat$experiment.accession))
 if (length(dd)>0) {
   message("duplicates found in experiment.accession, dropped")
   dat = dat[-dd,]
   }
 rownames(dat) = dat$experiment.accession
#
# stud is unique for dat, exp is a vector of expt ids
#
 stud = dat$study.accession[1]
 exp = dat$experiment.accession
#
# allstr will consist of all strings present in table, including
# field names
#
 fldnames = setdiff(names(dat), c("study.accession", "experiment.accession"))
 allstr = setdiff(unique(c(fldnames, unlist(lapply(dat, force)))), c(stud,exp))
 nc = nchar(allstr)
 bad = which(nc==0)
 if (length(bad)>0) allstr=allstr[-bad]
# clean out dates and numbers with commas
 cln = function(x) { dr = grep("....-..-..|.*...,...", x); if (length(dr)>0) x = x[-dr]; x }
 allstr = cln(allstr)
#
# some columns are numeric data.  we will not be searching for numeric constants
# such information can be used downstream
#
 isnum = which(!is.na(as.numeric(allstr)))
 if (length(isnum)>0) allstr = allstr[-isnum]
#
 dat = dat[,-c(1,2)]
 alltok = setdiff(unlist(strsplit(allstr, " ")), c(stud,exp))
 titles = htxcomp::studTable$study_title
 names(titles) = htxcomp::studTable$study_accession
 titleTokens = setdiff(strsplit(titles[stud], " ")[[1]], stopWords)
 alltok = c(alltok, titleTokens)
 nc = nchar(alltok)
 bad = which(nc==0)
 if (length(bad)>0) alltok=alltok[-bad]
 isnum = which(!is.na(as.numeric(alltok)))
 if (length(isnum)>0) alltok = alltok[-isnum]
 alltok = cln(alltok)
#
# when we contribute to an environment we first check that our
# current key is not present, if it is, we just c() new material
# to the available value
#
# step 1 -- bind available strings into studenv
#
 curstdat = try(get(stud, env=studenv), silent=TRUE)
 if (inherits(curstdat, "try-error")) curstdat=NULL
 assign(stud, c(curstdat, allstr), env=studenv)
#
# step 2 -- bind available tokens to studenv
#
 curstdat = try(get(stud, env=studenv), silent=TRUE)
 if (inherits(curstdat, "try-error")) curstdat=NULL
 assign(stud, c(curstdat, alltok), env=studenv)
#
# step 3 -- set up expenv, with a key for each experiment in the study
#
 tmp = lapply(exp, function(x) {  # FIXME do reverse map here too for specificity of exp assignments
   curexdat = try(get(x, env=expenv), silent=TRUE)
   if (inherits(curexdat, "try-error")) curexdat=NULL
   strs = na.omit(as.character(dat[x,]))
   nst = as.numeric(strs)
   isn = which(!is.na(nst))
   if (length(isn)>0) strs = strs[-isn]
   nc = nchar(strs)
   bad = which(nc==0)
   if (length(bad)>0) strs = strs[-bad]
   strs = cln(strs)
   assign(x, c(curexdat, strs), env=expenv)
   curexdat = try(get(x, env=expenv), silent=TRUE)
   if (inherits(curexdat, "try-error")) curexdat=NULL
   assign(x, c(curexdat, unlist(strsplit(strs, " "))), env=expenv)
   })
#
# at this point expenv has strings and tokens specific to each experiment
# step 4 now set up 'keystring' to experiment
#
   allexpts = ls(expenv)
   allstud = ls(studenv)
   drp = function(x) setdiff(x, c(allexpts, allstud, stopWords))
   expts2words = lapply(allexpts, function(x) c(titleTokens, drp(get(x, env=expenv))))
   ns = sapply(expts2words, length)
   tab = data.frame(expt=rep(allexpts, ns), phr=unlist(expts2words), stringsAsFactors=FALSE)
   if (!exists("kwexenv")) kwexenv = new.env(hash=TRUE)
   pr = split(tab$expt, tab$phr)
   for (i in names(pr)) if (nchar(i)>0) assign(i, pr[[i]], env=kwexenv)
# now set up keystring to study
   stud2words = lapply(allstud, function(x) c(titleTokens, drp(get(x, env=studenv))))
   ns = sapply(stud2words, length)
   tab = data.frame(stud=rep(allstud, ns), phr=unlist(stud2words), stringsAsFactors=FALSE)
   if (!exists("kwstenv")) kwstenv = new.env(hash=TRUE)
   pr = split(tab$stud, tab$phr)
   for (i in names(pr)) assign(i, pr[[i]], env=kwstenv)
 list(expenv=expenv, studenv=studenv, kwexenv=kwexenv, kwstenv=kwstenv)
}
#
doit = function() {
allc = dir(patt="csv$")
n1 = csv2envs(allc[1])
for (i in 2:length(allc)) n1 = csv2envs(allc[i], n1)
save(n1, file="n1.rda", compress="xz")
}
