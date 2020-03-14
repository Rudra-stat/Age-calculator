library(xlsx)
dob=read.xlsx(file.choose(),1,header = T)
# dob=read.csv(file.choose(),header = T)  # for csv file use
d=dob$Date.of.Birth
age_calc <- function(dob, enddate=Sys.Date(), units='years'){
  if (!inherits(dob, "Date") | !inherits(enddate, "Date"))
    stop("Both dob and enddate must be Date class objects")
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  
  years <- end$year - start$year
  if(units=='years'){
    result <- ifelse((end$mon < start$mon) | 
                       ((end$mon == start$mon) & (end$mday < start$mday)),
                     years - 1, years)    
  }else if(units=='months'){
    months <- (years-1) * 12
    result <- months + start$mon
  }else if(units=='days'){
    result <- difftime(end, start, units='days')
  }else{
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)
}
age=age_calc(d)

