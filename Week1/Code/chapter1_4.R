#the more you sample, the more the sample's distribution will look like the populaiton distribution
MySample5 <- rnorm(5, m=0, sd=1)
MySample10 <- rnorm(10, m=0, sd=1)
MySample20 <- rnorm(20, m=0, sd=1)
MySample40 <- rnorm(40, m=0, sd=1)
MySample80 <- rnorm(80, m=0, sd=1)
MySample160 <- rnorm(160, m=0, sd=1)
# visualize sample
par(mfcol=c(2,3)) #initialize multi-paneled plot
par(mfg=c(1,1)); hist(MySample5, col=rgb(1,1,0), main="n=5")
par(mfg=c(1,2)); hist(MySample10, col=rgb(1,1,0), main="n=10")
par(mfg=c(1,3)); hist(MySample20, col=rgb(1,1,0), main="n=20")
par(mfg=c(2,1)); hist(MySample40, col=rgb(1,1,0), main="n=40")
par(mfg=c(2,2)); hist(MySample80, col=rgb(1,1,0), main="n=80")
par(mfg=c(2,3)); hist(MySample160, col=rgb(1,1,0), main="n=160")
