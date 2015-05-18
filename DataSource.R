library(xts)
library(data.table)
library(dplyr)
library(dygraphs)


raw <- read.table("clipboard", header = TRUE, stringsAsFactors = FALSE)
str(raw)
raw$Date <- as.Date(raw$Date, format = "%m/%d/%Y")

save(raw, file = "rawDF.RData")

pe <- read.table("clipboard", header = TRUE, stringsAsFactors = FALSE)

raw

TSraw <- as.xts(raw[,-1], order.by = raw$Date)
save(TSraw, file = "EBITDAMult.RData")

names(TSraw)
dygraph(TSraw$PKD) %>%
  dyRangeSelector(heigh = 40) %>%
  dyAxis("y", label = "NTM EBITDA Multiple") %>%
  dyAxis("x", pixelsPerLabel = 45, drawGrid = TRUE) %>%
  dyRoller(rollPeriod = 1)


mtcars %>% ggvis(~disp, ~wt) %>%
  layer_points()

mtcars %>% ggvis() %>%
  layer_points() %>%
  props(prop("x", as.name("disp"))) %>%
  props(prop("y", as.name("wt")))
