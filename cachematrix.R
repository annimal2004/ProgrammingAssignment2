makeCacheMatrix <- function(x = matrix()) {
    invMat  <-  NULL;
    
    set  <- function(y){
        invMat  <<-  NULL;
        x <<- y
    }
    
    get <- function(){
        x;
    }
    
    setInv  <-  function(invMat){
        invMat <<- invMat
    }
    
    getInv  <- function(){
        invMat
    } 
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

    
cacheSolve <- function(x, ...) {
    
    invMat = x$getInv()
    
    if(!is.null(invMat)){
        message("getting cached data")
        return(invMat)
    }else{
        
        data = x$get()
        invMat = solve(data)
        x$setInv(invMat)
        return(invMat)
    }
  
} 
