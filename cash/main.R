generateMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

cacheInverseMatrix <- function(x, ...) {
  ## Вернуть обратную матрицу для x
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("получение закешированных данных")
    return(m)
  }
  data <- x$get()
  message("вычисление обратной матрицы")
  m <- solve(data)
  x$setInverseMatrix(m)
  m
}

matrix <- matrix(c(5,1,1,1,1,0,5,5,-1),c(3,3))
res <- generateMatrix(matrix)

cacheInverseMatrix(res)

cacheInverseMatrix(res)
