#=========================================================================#
# Function Fig_obspm10 - Create and display figure                        #
#=========================================================================#
Fig_obspm10 = function(obs,prev,titlemain,FileName) {
     Thresh = 30
    RMSE = round(sqrt(sum((obs - prev)^2)/length(obs)),2)
    EV   = round(1 - var(prev-obs)/var(obs), 2)
    A    = sum((obs >= Thresh) & (prev >= Thresh))
    B    = sum((obs >= Thresh) & (prev < Thresh))
    C    = sum((obs < Thresh)  & (prev >= Thresh))
    TS   = round(A/(A+B+C),3)

    plot(obs,prev, xlab="PM10 observés", pch=19, ylab="PM10 prévus",
    main = titlemain, cex=1.5, cex.lab=1.6, cex.main = 1.7,
    cex.axis=1.5, xlim=c(0,90), ylim=c(0,90))
    abline(0,1,h=c(30,50),v=c(30,50),col=2,lwd=2) 
    legend('topleft',legend=c(paste("EV   ",EV),paste("RMSE ",RMSE)),cex=1.2)
    legend('bottomright',legend=c(paste("TS ",TS)),cex=1.2)

    jpeg(FileName)
    plot(obs,prev, xlab="PM10 observés", pch=19,ylab="PM10 prévus",
    main = titlemain, cex=1.5, cex.lab=1.6, cex.main = 1.7,
    cex.axis=1.5, xlim=c(0,90), ylim=c(0,90))
    abline(0,1,h=c(30,50),v=c(30,50),col=2,lwd=2) 
    legend('topleft',legend=c(paste("EV   ",EV),paste("RMSE ",RMSE)),cex=1.2)
    legend('bottomright',legend=c(paste("TS ",TS)),cex=1.2)
     XXX= dev.off()
}

