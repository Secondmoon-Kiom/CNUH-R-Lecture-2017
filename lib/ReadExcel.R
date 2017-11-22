ReadExcel <- function(filename) {
  require(XLConnect)
  require(plyr)
  
  WB <- loadWorkbook(filename)
  SheetName <- getSheets(WB)
  
  DFl <- llply(SheetName, function(name) readWorksheet(WB, sheet=name))
  names(DFl) <- SheetName
  return(DFl)
}

