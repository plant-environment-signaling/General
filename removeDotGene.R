removeDotGene <- function(dotGeneName,ColumnName="",verbose=F){
  if (verbose) cat ("removeDotGene function called\n")
  tmp <- gsub("\\.[0-9].*$","",unlist(dotGeneName))
  return(tmp)
}

