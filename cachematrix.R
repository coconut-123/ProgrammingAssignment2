## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   # chỗ lưu nghịch đảo

    # hàm set: gán giá trị mới cho ma trận
    set <- function(y) {
        x <<- y
        inv <<- NULL   # reset cache
    }

    # hàm get: trả về ma trận gốc
    get <- function() x

    # hàm setinverse: lưu nghịch đảo vào cache
    setinverse <- function(inverse) inv <<- inverse

    # hàm getinverse: trả về nghịch đảo nếu đã có trong cache
    getinverse <- function() inv

    # trả về list các hàm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    
    # nếu nghịch đảo đã có trong cache thì lấy luôn
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
            # nếu chưa có thì tính nghịch đảo
    data <- x$get()
    inv <- solve(data, ...)   # solve() = tính nghịch đảo ma trận trong R
    
    # lưu lại vào cache
    x$setinverse(inv)
    
    inv
}
