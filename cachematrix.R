## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 
 #User can input matrix through set_mat function
 set_mat <- function(y){
   x <<- y
 }
 
 #User can read matrix through set_mat function
 get_mat <- function(){
   x
 }
 
 #User can initiate matrix inverse though set_matinv
 set_matinv <- function(z){
   inv <<- solve(z)
 }
 
 #User can read matrix inverse though set_matinv
 get_matinv <- function(){
   inv
 }
 
 list(set_mat = set_mat, get_mat = get_mat, set_matinv = set_matinv, get_matinv = get_matinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        val <- x$get_matinv()
        #Retrieving if inverse matrx exist in makeCacheMatrix function
        if(!is.null(val)){
          message("getting cached matrix inverse")
          return (val)
        }
        
        #calculate matrix inverse if inverse matrx does not exist in makeCacheMatrix function
        data <- x$get_mat()
        m <- solve(data)
        x$set_matinv(m)
        m
}
