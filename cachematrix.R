

####I use the makeCacheMatrix function to calculate the inverse of the matrix. 
####Then I catches it by using the <<- symbol that allows the cacheSolve function to compute the inverse of the matrix. 

warnings()
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y) {
    
    	x <<- y
    	inv <<- NULL
}
   
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
  	getinv <- function() inv
	list(set = set, get = get, 
	setinv = setinv, getinv = getinv)
}	

##Once the matrix is cached, I then compute the inverse of the matrix

cacheSolve <- function(x, ...) {	

	inv <- x$getinv()
	if(!is.null(inv)) {
    
    message("getting cached result")
    return(inv)

}
  
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
	
}	  
	
#####Example of a random matrix	
	

mymatrix <- matrix (rnorm(25),5,5)
mymatrix1 <- makeCacheMatrix(m)
cacheSolve(m1)
