## Las siguientes funciones recibe una matriz de dimension nxn y genera su inversa con registro en la memoria caché

## La función solve permite realizar diferentes tipos de calculos con vectores y matrices. 
## La matriz que se ingrese a la funcion debe ser de dimensiones nxn

makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL 
  set <- function(y) {
    x <<- y
    ma <<- NULL 
  }
  
  get <- function() x 
  setInv <- function(solve) ma <<- solve
  getInv <- function() ma 
 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  
  if(!is.null(m)) { 
    message("Obtencion de datos en caché")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  message("Inversa de la Matriz")
  print(m) 
}

