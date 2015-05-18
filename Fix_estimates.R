library(data.table)
library(xts)
source("ts_widget.R")
input <- read.table("clipboard", sep = "\t", stringsAsFactors = FALSE, header = TRUE, fill = TRUE)

str(input)
tail(input,100)

#function to only keep the last observation of a given date
fixInput <- function(df){
  dt <- as.data.table(df)
  dt[,Date := as.IDate(Date,format = "%m/%d/%Y")]  
  unique_dates <- data.table(Date = unique(dt[,Date]))
  setkey(unique_dates, Date)
  setkey(dt, Date)
  dt[unique_dates, mult = "last"]
}

#function to split a big mess of estimates into seperate lists (after cleaned with fixInput)

splitdf <- function(df){
  colCount <- ncol(df)
  names <- names(df[seq(from = 2, to = colCount, by = 2)])
  split <- lapply(seq(from = 1, to = colCount, by = 2), function(x){
    temp <- df[,x:(x+1), drop = FALSE]
    names(temp)[1] <- "Date"
    temp <- fixInput(temp)
    return(temp)
    })
  names(split) <- names
  return(split)
}

###FOR SAVING AND CREATING THE CORRECT INFORMATION
#eps <- splitdf(input)
#save(eps,file = "eps.RData")
#cps <- splitdf(input)
#save(cps, file = "cps.RData")
#price <- splitdf(input)
#save(price, file = "price.Rdata")
PE_Mult <- CreateMult(eps)
CF_Mult <- CreateMult(cps)
PE_TS <- do.call.merge.dt(PE_Mult)
CF_TS <- do.call.merge.dt(CF_Mult)

#
save(PE_TS, file = "PE_TS.RData")
save(CF_TS, file = "CF_TS.RData")

CreateMult <- function(input){
  raw <- lapply(names(input), function(x){
    tempName <- paste0("i.",x)
    output <- input[[x]][price[[x]],roll = TRUE][, `:=`(PE = get(tempName)/get(x),
                                            Yield = get(x) / get(tempName))][,.(Date,PE)] #change here to get YIELD instead of Multiple
    setkey(output,Date)
    return(output)
  })
  names(raw) <- names(input)  
  return(raw)
}

#merge all DataTables into on Data table with all dates
do.call.merge.dt <- function(lst){
  oldNames <- names(lst)
  while(length(lst) >1){
    idxlst <- seq(from = 1, to = length(lst), by = 2)
    lst <- lapply(idxlst, function(i){
      if(i == length(lst)){return(lst[[i]])}
      return(merge(lst[[i]],lst[[i+1]], all = TRUE))
    })
  }
  setnames(lst[[1]], c("Date", oldNames))
  lst[[1]][,Date:=as.Date(as.numeric(lst[[1]][,Date]))]
  lst[[1]]
}

#none of themse matter becuase need to pass in a big data.frame to widget
convert_ts <- function(lst){
  clean <- lapply(names(lst), function(x){
    tempTable <- lst[[x]][!is.na(Date),]
    as.xts(tempTable[,Yield], order.by = as.Date(as.numeric(tempTable[,Date])))
    #as.xts(clean, order.by = clean$Date2)
  })
  names(clean) <- names(lst)
  return(clean)
}

#to cbind a bunch of xts objects
#ended up not using this.
do.call.cbind <- function(lst){
  orig_names <- names(lst)
  while(length(lst) >1){
    idxlst <- seq(from = 1, to = length(lst), by = 2)
    lst <- lapply(idxlst, function(i){
      if(i==length(lst)){ return(lst[[i]])}
      return(cbind(lst[[i]], lst[[i+1]]))
    })
  }
  lst[[1]]
}
