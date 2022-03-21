readCsvFile <- function(name){
  res <- read.csv(name, colClasses = "character")
}
res <- read.csv("data/results_of_care.csv", colClasses = "character")
res[, 11] <- as.numeric(res[, 11])
hist(res[, 11],
     xlab = 'дни', 
      main="Гистограмма 30-дневных показателей 
смертности от сердечного приступа")


getBestHospital <- function(state, criteria) {
  ## Прочитать данные по показателю
  res <- readCsvFile("data/results_of_care.csv")
  n <- data.frame() 
  n <- res[which(res[, 7] == state),]
  
  ## Убедитесь, что штат и показатель корректны
  if (length(n[, 7])==0){stop("недопустимый штат")}
  if (criteria == "heart attack"){
    c <- 11
  } else if (criteria == "heart failure"){
    c <- 17
  }else if (criteria == "pneumonia"){
    c <- 23
  }else {stop("недопустимый показатель")}
  n[, c] <- as.numeric(n[, c])
  n <- n[complete.cases(n), ]
  
  ## Выберите названия больницы в штате с наименьшей 30-дневной смертностью.
  minC <- min(n[, c])
  name <- n[which(n[, c] == minC), 2]
  name <- sort(name)[1]
  returnValue(name)
  
  
  ## При необходимости отсортируйте результаты по наименованию больницы
}
getBestHospital("TX", "heart attack")
getBestHospital("TX", "heart failure")
getBestHospital("MD", "heart attack")
getBestHospital("MD", "pneumonia")
getBestHospital("BB", "heart attack")
getBestHospital("NY", "hert attack")


getHospitalRating <- function(state, criteria, m = "best") {
  ## Прочитать данные по показателю
  res <- readCsvFile("data/results_of_care.csv")
  n <- data.frame() 
  n <- res[which(res[, 7] == state),]
  
  ## Убедится, что штат и показатель корректны
  if (length(n[, 7])==0){stop("недопустимый штат")}
  if (criteria == "heart attack"){
    c <- 11
  } else if (criteria == "heart failure"){
    c <- 17
  }else if (criteria == "pneumonia"){
    c <- 23
  }else {stop("недопустимый показатель")}
  n[, c] <- as.numeric(n[, c])
  n <- n[complete.cases(n), ]
  
  ## Вернуть название больницы в заданном штате с заданным рейтингом 30-дневной смертности
  if (m == "worst"){
    minC <- max(n[, c])
  } else if (m == "best"){
    minC <- min(n[, c])
  } else if (m > 1){
    temp <- sort(n[, c])
    minC <- temp[m]
  }else {stop("недопустимый аргумент")}
  name <- n[which(n[, c] == minC), 2]
  name <- sort(name)[1]
  returnValue(name)
  
}

getHospitalRating("TX", "heart failure", 4)
getHospitalRating("MD", "heart attack", "worst")
getHospitalRating("MN", "heart attack", 5000)

getRaiting <- function(criteria, n = "best") {
  ## Прочитать данные по показателю
  res <- readCsvFile("data/results_of_care.csv")
  
  ## Убедится, что штат и показатель корректны
  if (criteria == "heart attack"){
    c <- 11
  } else if (criteria == "heart failure"){
    c <- 17
  }else if (criteria == "pneumonia"){
    c <- 23
  }else {stop("недопустимый показатель")}
  
  res[, c] <- as.numeric(res[, c])
  res <- res[complete.cases(res), ]
  
  ## Для каждого штата найти больницу с указанным рейтингом
  states <- unique(res[,7])
  datA <- data.frame()
  datB <- data.frame()
  for (i in states) {
    m <- data.frame() 
    m <- res[which(res[, 7] == i),]
    m <- m[order(m[c], m[2]),]
    temp <- m[2]
    row.names(temp) <- NULL
    if (n == "worst"){
      name <- temp[nrow(temp),]
    } else if (n == "best"){
      name <- temp[1,]
    } else if (n > 1){
      name <- temp[n,]
    }else {stop("недопустимый аргумент")}
    
    datB <- c(name[1], i)
    datA <- rbind(datA,datB)
  }
  returnValue(datA)
  ## Вернуть дата фрейм с названиями больниц и названием штата
}


head(getRaiting("pneumonia", 15), 10)
head(getRaiting("heart failure", "worst"), 5)
