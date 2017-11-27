
circle <- function(samps=10000, a=4, b=0.3, rad=1.2, x0=0, y0=0, color="white", alpha=0.5, add=FALSE, bg="black"){
    u <- 2*pi*runif(samps)
    r <- rad*sqrt(rbeta(samps,a,b))
    x <- r*cos(u)+x0
    y <- r*sin(u)+y0
 if(add==FALSE){par(bg = bg);  plot(1:2,1:2,type="n",ylim=c(-3,3),xlim=c(-3,3))}   
    points(rad*x, rad*y, pch=".", col=add.alpha(color,alpha))
}

diamond <- function(samps=30000, a=0.3, b=0.3, x0=0, y0=0, sx=1,sy=3, deg=45, color="white", alpha=0.5, add=FALSE, bg="black"){
 
    px<-rbeta(samps,a,b) - 0.5 
    py<-rbeta(samps,a,b) - 0.5 
    
    theta <- deg2rad(deg)
    
    px2 <- cos(theta) * (px) - sin(theta) * (py) 
    py2 <- sin(theta) * (px) + cos(theta) * (py) 
 if(add==FALSE){par(bg = bg);  plot(1:2,1:2,type="n",ylim=c(-3,3),xlim=c(-3,3))}    
    points(sx*px2 + x0,sy*py2 +y0,pch=".",col=add.alpha(color,alpha))
}

flower <- function(samps=30000, a=8, b=0.1, x0=0, y0=0, shape1=4, shape2=20, rad=2, deg=45, color="white", alpha=0.5, add=FALSE, bg="black"){
  k <- 1 + shape1/shape2
  
  t <- seq(0.001,samps)/shape2
  x <- cos(k*t) * sin(t);
  y <- cos(k*t) * cos(t); 
    
  scale <- rbeta(samps,a,b)*rad
 if(add==FALSE){par(bg = bg);  plot(1:2,1:2,type="n",ylim=c(-3,3),xlim=c(-3,3))}   
  points(scale*(x0+x), scale*(y0+y), pch=".", col=add.alpha(color,alpha))  
}

sierpinski  <- function(samps=30000, x0=3, y0=0, scale=0.4, deg=0, color="white", alpha=0.5, add=FALSE, bg="black"){
  p1<-c(0,0)
  p2<-c(-8,4)   
  p3<-c(0,8)
  
  p<-list(p1,p2,p3)
  vx<-rep(NA,samps)
  vy<-rep(NA,samps)
  
  vx[1]<-1
  vy[1]<-1
  
  for(i in 1:samps){
   vert<- sample(c(1:3),1)
        
   X <- (p[[vert]] + c(vx[i],vy[i]))/2
            
   vx[i+1]<-X[1]
   vy[i+1]<-X[2]
   }
                    
   theta <- deg2rad(deg)
    
   px2 <- cos(theta) * (vx-0) - sin(theta) * (vy-0) + 0
   py2 <- sin(theta) * (vx-0) + cos(theta) * (vy-0) + 0
   
 if(add==FALSE){par(bg = bg);  plot(1:2,1:2,type="n",ylim=c(-3,3),xlim=c(-3,3))}      
    points(scale*(x0+px2),scale(y0+py2),pch=".",col=add.alpha(color,alpha))
}

galaxies  <- function(samps=300, c=0.25, z=-0.1, x0=0, y0=0, scale=0.8, sd=0.07, color="white", alpha=0.6, add=FALSE, bg="black"){
 if(add==FALSE){par(bg = bg);  plot(1:2,1:2,type="n",ylim=c(-3,3),xlim=c(-3,3))}     
  for(n in 1:samps){
    a1 <- n*deg2rad(360*((sqrt(5)+1)/2)^(-2))
    r <- c*sqrt(n)  + z
   
    y1 <- r * sin(a1) + y0
    x1 <- r * cos(a1) + x0
   
   points(scale*rnorm((samps+1-n), x1, sd), scale*rnorm((samps+1-n), y1, sd), pch=".", col=add.alpha(color,alpha))

   }
}

seed.of.life <- function(samps=10000, a=4, b=0.3, rad=2, x0=0, y0=0, color="white", alpha=0.5, add=FALSE, bg="black"){
                         
 t <- 1.0472 
 R <- rad
 centers <- matrix(NA,nrow=7,ncol=2)

 centers[1,] <- c(0,0)  + c(x0,y0)
 centers[2,] <- c(0,R)  + c(x0,y0)
 centers[3,] <- c(0,-R) + c(x0,y0)
 centers[4,] <- c(sec.center(R,0.5))  + c(x0,y0)
 centers[5,] <- c(sec.center(R,2.5*t)) + c(x0,y0)
 centers[6,] <- c(sec.center(R,3.5*t)) + c(x0,y0)
 centers[7,] <- c(sec.center(R,5.5*t)) + c(x0,y0)


 if(add==FALSE){par(bg = bg);  plot(1:2,1:2,type="n",ylim=c(-11,11),xlim=c(-11,11))}     
 
for(i in 1:7)
circle(samps, a, b, rad, x0=centers[i,1], y0=centers[i,2], color, alpha, add=TRUE, bg=bg)
}

  seed.of.life()













    
    
    