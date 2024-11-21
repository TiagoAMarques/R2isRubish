#illustrating misleading R^2 as a measure of quality of fit 

png(file="Fig4ReadMe.png")

par(mfrow=c(2,2),mar=c(4,4,3,0.2))
set.seed(123)
n<-100
xs<-runif(n,10,200)
Eys<-exp(0.04+0.02*xs)
ys<-rpois(n,Eys)
plot(ys~xs,main=paste0("R2=",round(cor(ys,xs),3)))
abline(lm(ys~xs))


n<-100
xs<-c(runif(n,10,200),2000)
Eys<-exp(0.04+0.02*xs)
ys<-rpois(n+1,Eys)
plot(ys~xs,main=paste0("R2=",round(cor(ys,xs),3)))
abline(lm(ys~xs))


xs<-rep(1:4,each=100)
Eys<-2+2*xs
ys<-rnorm(400,mean=Eys,sd=5)
plot(ys~xs,main=paste0("R2=",round(cor(ys,xs),3)))
abline(lm(ys~xs))

# Setting up the parameters for the circle
radius <- 1
n_points <- 1000
# Generating points around the circle
theta <- seq(0, 2 * pi, length.out = n_points)
x <- radius * cos(theta)
y <- radius * sin(theta)
# Plotting the circle
#plot(x, y, type = 'l', col = 'blue', xlab = 'X', ylab = 'Y', asp = 1,
#     main = 'Circle with Points')
# Adding random points on the circle
points_on_circle <- sample(seq(1, n_points), 100)  # Randomly selecting 100 points
xs<-rnorm(100,mean=x[points_on_circle],sd=0.08)
ys<-rnorm(100,mean=y[points_on_circle],sd=0.08)
plot(xs,ys, pch = 19,main=paste0("R2=",round(cor(ys,xs),3)))
abline(lm(ys~xs))

dev.off()
