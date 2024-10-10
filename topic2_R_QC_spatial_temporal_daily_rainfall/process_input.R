library(plyr)
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}
# baca data input
# T <- read.csv(paste0(dirinput,'/data.csv'))
datach <- read.csv(paste0(dirinput,'/data.csv'))
ColT = colnames(datach)
# ch <- data.frame(c())
# for (itT in 1:length(ColT)) {
#   ch <- cbind.fill(ch,T[colnames(T)[itT]])
# }
infoposprov <- read.csv(paste0(dirinput,'/info.csv'))
# infosta <- infoposprov
# infoT <- read.csv(paste0(dirinput,'/info.csv'))
# ColinfoT  <- colnames(infoT)
datach[datach==9999 | datach==999 | datach<0] <- NA
datach[datach==8888 | datach==888] <- 0
data <- datach
info1 <- infoposprov[,c(5,6)]
info2 <- infoposprov[,c(1,2,5,6)]
