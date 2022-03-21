getMean <- function(folder, type, id=1:332){
  
  files_ful <- list.files(folder, full.names=TRUE)
  datA <- data.frame() 
  for (i in id) {
    datA <- rbind(datA, read.csv(files_ful[i]))
  }
  print(mean(datA[, type], na.rm=TRUE))
  
}

getMean("data", "sulfate", 1:10)
getMean("data", "nitrate", 70:72)
getMean("data", "nitrate", 23)

getCompleteObservation <- function(folder, id = 1:332) {
  files_ful <- list.files(folder, full.names=TRUE)
  datA <- data.frame() 
  datB <- data.frame()
  datC <- data.frame('id', 'count')
  for (i in id) {
    datA <- rbind(datA, read.csv(files_ful[i]))
    datB <- c(i, NROW(na.omit(datA[which(datA[, "ID"] == i),])))
    datC <- rbind(datC,datB)
  }
  print(datC)
}

getCompleteObservation("data", 1)
getCompleteObservation("data", c(2, 4, 8, 10, 12))
getCompleteObservation("data", 30:25)
getCompleteObservation("data", 3)


getCorrelation <- function(folder, limen = 0) {
  files_ful <- list.files(folder, full.names=TRUE)
  datA <- data.frame() 
  n <- data.frame() 
  datB <- data.frame()
  for (i in 1:length(files_ful)) {
    datA <- rbind(datA, read.csv(files_ful[i]))
    n <- datA[which(datA[, "ID"] == i),]
    n <- n[complete.cases(n[ , 2:3]),]
    if (nrow(n)>limen){
      datB <- rbind(datB,cor(n["sulfate"], n["nitrate"]))
    }
  }
  datB
}

res <- getCorrelation("data", 150)
head(res)
summary(res)

res <- getCorrelation("data", 400)
head(res)

res <- getCorrelation("data", 5000)
summary(res)
length(res)

res <- getCorrelation("data")
summary(res)
length(res)
