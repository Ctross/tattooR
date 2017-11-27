
add.alpha <- function(col, alpha=1){
  if(missing(col)) 
   stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
   function(x) 
   rgb(x[1], x[2], x[3], alpha=alpha))  
 }
 
rad2deg <- function(rad) {(rad * 180) / (pi)}

deg2rad <- function(deg) {(deg * pi) / (180)}

sec.center  <- function(r,t){
    x <- r * cos(t)
    y <- r * sin(t)
    return(c(x,y))
    }
