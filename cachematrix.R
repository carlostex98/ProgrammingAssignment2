##this takes a matrix and return the inverse
## it's a cached matrix using the advantages of lexical scoping

##creates the object

makeCacheMatrix <- function(n = matrix()) {
  inv_m <- NULL
  set <- function(y){
    n <<- y
    inv_m <<- NULL
  }
  get <- function() n
  set_Inv <- function(solve_m) inv_m <<- solve_m
  get_Inv <- function() inv_m
  list(set = set, get = get, set_Inv = set_Inv, get_Inv = get_Inv)
}


## compute the inverse an return it

cacheSolve <- function(x, ...) {
  inv_m <- x$get_Inv()
  if(!is.null(inv_m)){
    message("cached data!")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$set_Inv(inv_m)
  inv_m      
}
