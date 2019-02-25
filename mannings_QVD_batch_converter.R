#  A script for computing the multiplyer and exponent for the equations:
#                 V = a*Q^b         and
#             Depth = c*Q^d
#  from Mannings equation.

# Water Quality models like the WASP program require input in the form of multiplyer and exponent.
#  In many instances, multiple measures may not be available for establishing the relationship.  Using
#  Manning's equation to estimate flow based on channel characteristics is a typical alternative.
#  This program allows estimation of a, b, c, and d parameters based on input Manning's values assuming a
# range of depths from 0.1 to 10 meters.

#  Programmed by John Yagecic, P.E.  August 2016
#                JYagecic@gmail.com


setwd("\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405")

Minput<-read.csv("001_data\\q2d_batch_file.csv")

summary(Minput)

# edit below to change depth range and steps
myDepth<-seq(0.1, 15, by=0.1)

Moutput<-Minput

Moutput$a<-NA
Moutput$b<-NA
Moutput$c<-NA
Moutput$d<-NA

for (j in 1:nrow(Minput)){
    
    myArea<-(myDepth*Minput$BottomWidth[j])+(2*0.5*(myDepth/Minput$SideSlope[j])*myDepth)
    myWp<-Minput$BottomWidth[j]+(2*sqrt((myDepth^2)+((myDepth/Minput$SideSlope[j])^2)))
    Velocity<-(((myArea/myWp)^(2/3))*Minput$BedSlope[j]^0.5)/Minput$Manning.n[j]
    myQ<-Velocity*myArea
    #plot(myQ, myV)
    mydf<-data.frame(Velocity=Velocity, Discharge=myQ, Depth=myDepth)
    
    model.1<-lm(log(Velocity) ~ Discharge, data=mydf)
    start <- list(a=exp(coef(model.1)[1]), b=coef(model.1)[2]) # this is the part the allows nls function to work
    nlmod.VgivenQ<-nls(Velocity ~ a * Discharge ^ b, start=start, data=mydf)
    
    Moutput$a[j]<-coef(nlmod.VgivenQ)[1]
    Moutput$b[j]<-coef(nlmod.VgivenQ)[2]
    
    # Uncomment below to see plots and values of each iteration
    
    #print(Moutput$a[j])
    #print(Moutput$b[j])
    
    #plot(mydf$Discharge, mydf$Velocity, xlab="Discharge (CMS)", ylab="Velocity (M/S)")
    #points(mydf$Discharge, coef(nlmod.VgivenQ)[1]*mydf$Discharge^coef(nlmod.VgivenQ)[2], type="l", col="red")
    #text(quantile(mydf$Discharge,probs=0.75), quantile(mydf$Velocity, probs=0.25), paste("a =", round(Moutput$a[j],4)), cex=1)
    #text(quantile(mydf$Discharge,probs=0.75), quantile(mydf$Velocity, probs=0.35), paste("b =", round(Moutput$b[j],4)), cex=1)
    
    
    #Sys.sleep(0.5)
    
    model.2<-lm(log(Depth) ~ Discharge, data=mydf)
    start2 <- list(c=exp(coef(model.2)[1]), d=coef(model.2)[2]) # this is the part the allows nls function to work
    nlmod.DgivenQ<-nls(Depth ~ c * Discharge ^ d, start=start2, data=mydf)
    
    Moutput$c[j]<-coef(nlmod.DgivenQ)[1]
    Moutput$d[j]<-coef(nlmod.DgivenQ)[2]
    
    # Uncomment below to see plots and values of each iteration
    
    #print(Moutput$c[j])
    #print(Moutput$d[j])
    
    #plot(mydf$Discharge, mydf$Depth, xlab="Discharge (CMS)", ylab="Depth (M)")
    #points(mydf$Discharge, coef(nlmod.DgivenQ)[1]*mydf$Discharge^coef(nlmod.DgivenQ)[2], type="l", col="red")
    #text(quantile(mydf$Discharge,probs=0.75), quantile(mydf$Depth, probs=0.25), paste("c =", round(Moutput$c[j],4)), cex=1)
    #text(quantile(mydf$Discharge,probs=0.75), quantile(mydf$Depth, probs=0.35), paste("d =", round(Moutput$d[j],4)), cex=1)
    
    #Sys.sleep(0.5)
    
}

write.table(Moutput, file="siletz_q2d_coefficients.csv", sep=",", row.names=FALSE)









