## Comments in Portuguese Brazil.
## ADALSON BARBOSA COSTA
## Esta função cria um objeto "matrix" especial que pode armazenar em cache 
## seu inverso.
#' 
#' @param x A matrix
#' @return Uma lista contendo quatro funcoes para definir e obter o valor do
# 'matriz e para definir e obter o inverso da matriz
#'     
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Defina a funcao para definir o valor da matriz. Tambem limpa o antigo inverso do cache
  set <- function(y) {
    x <<- y    
    m <<- NULL 
  }
  # Definir funcao para obter o valor da matriz
  get <- function() x
  # Defina a funcao para definir o inverso. Isso e usado apenas por getinverse () quando
  # nao ha inverso em cache
  setInverse <- function(inverse) m <<- inverse
  # Definir funcao para obter o inverso
  getInverse <- function() m
  
  # Retornar uma lista com as quatro funcoes acima
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# 'Retorno inverso da matriz x
# 'Funcao  que calcula o inverso da "matrix" especial retornada por
# 'makeCacheMatrix. Se o inverso ja tiver sido calculado e a matrix nao
# 'foi alterada, o cachesolve recupera o valor inverso do cache.
#' 
#' @param x uma matriz especial criada com makeCacheMatrix
#' @return O inverso da matriz x
#' 
cacheSolve <- function(x) {
  m <- x$getInverse() # busca o valor em cache para o inverso
  if(!is.null(m)) {   # Devolve cache nao vazio
    message("getting cached data")
    return(m)
  }
  # Para cache vazio,calcular, armazenar em cache e devolver.
  data <- x$get()  # Obtem valor da matriz
  m <- solve(data) # Calcular o inverso
  x$setInverse(m)  # Armazena o resultado em cache 
  m                # Retornar o inverso
}

