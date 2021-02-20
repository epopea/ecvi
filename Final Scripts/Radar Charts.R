# Creating the data

qdata <- data %>% select(dzone, mesoregion, TIP_Morb) 
qdata <- data.frame(qdata)

qdata$qTXx <- quantcut(data$TXx, 5)
qdata$qTNx <- quantcut(data$TNx, 5)
qdata$qTX90p <- quantcut(data$TX90p, 5)
qdata$qTN90p <- quantcut(data$TN90p, 5)
qdata$qDTR <- quantcut(data$DTR, 5)
qdata$qCdd <- quantcut(data$Cdd, 5)
qdata$qR99p <- quantcut(data$R99p, 5) 

for (i in 4:10){
  qdata[,i] <- as.numeric(qdata[,i])
}

qdata$expm = vector(length = nrow(qdata))
for (i in 1:nrow(qdata)){
  qdata$expm[i] <- rowMeans(qdata[i,4:10])
}

qdata$exp <- qdata$expm/5

qdata$qkid <- quantcut(data$kid, 5)
qdata$qold <- quantcut(data$old, 5)
qdata$qliterate <- quantcut(data$literate, 5)
qdata$qpcincome <- quantcut(data$pcincome, 5)
qdata$qpoor <- quantcut(data$poor, 5)

for (i in 13:17){
  qdata[,i] <- as.numeric(qdata[,i])
}

qdata$sucm = vector(length = nrow(qdata))
for (i in 1:nrow(qdata)){
  qdata$sucm[i] <- rowMeans(qdata[i,13:17])
}

qdata$suc <- qdata$sucm/5

qdata$qurb <- quantcut(data$urb, 5)
qdata$qwater <- quantcut(data$water, 5)
qdata$qsewage <- quantcut(data$sewage, 5)
qdata$qgarbage <- quantcut(data$garbage, 5)
qdata$qfhs <- quantcut(data$fhs, 5)
qdata$qbeds <- quantcut(data$beds, 5)

for (i in 20:25){
  qdata[,i] <- as.numeric(qdata[,i])
}

qdata$adpm = vector(length = nrow(qdata))
for (i in 1:nrow(qdata)){
  qdata$adpm[i] <- rowMeans(qdata[i,20:25])
}

qdata$adp <- qdata$adpm/5
qdata$mindex <- rowMeans(qdata[,c(12,19,27)])

# Exposure
dexp <- as.data.frame(matrix(c(mean(qdata$qTXx[qdata$dzone==2]/5),
                               mean(qdata$qTNx[qdata$dzone==2]/5),
                               mean(qdata$qTX90p[qdata$dzone==2]/5),
                               mean(qdata$qTN90p[qdata$dzone==2]/5),
                               mean(qdata$qDTR[qdata$dzone==2]/5),
                               mean(qdata$qCdd[qdata$dzone==2]/5),
                               mean(qdata$qR99p[qdata$dzone==2]/5)),
                             ncol=7))
colnames(dexp) <- c("TXx" , "TNx" , "TX90p", "TN90p", "DTR", "Cdd", "R99p")
dexp <- rbind(rep(1,7) , rep(0,7) , dexp)
tiff(file="Figures/exposure_dry.tiff",
     width=6, height=6, units="in", res=300)
radarchart(dexp,  title="Exposure", axistype=1,
           #custom polygon
           pcol=rgb(0.2,0.2,0.2,0.9) , pfcol=rgb(0.2,0.2,0.2,0.5) , plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8,
           #custom labels
           vlcex=1.2 )
dev.off()

dexp <- as.data.frame(matrix(c(mean(qdata$qTXx[qdata$dzone==1]/5),
                               mean(qdata$qTNx[qdata$dzone==1]/5),
                               mean(qdata$qTX90p[qdata$dzone==1]/5),
                               mean(qdata$qTN90p[qdata$dzone==1]/5),
                               mean(qdata$qDTR[qdata$dzone==1]/5),
                               mean(qdata$qCdd[qdata$dzone==1]/5),
                               mean(qdata$qR99p[qdata$dzone==1]/5)),
                             ncol=7))
colnames(dexp) <- c("TXx" , "TNx" , "TX90p", "TN90p", "DTR", "Cdd", "R99p")
dexp <- rbind(rep(1,7) , rep(0,7) , dexp)
tiff(file="Figures/exposure_wet.tiff",
     width=6, height=6, units="in", res=300)
radarchart(dexp,  title="Exposure", axistype=1,
           #custom polygon
           pcol=rgb(0.2,0.2,0.2,0.9) , pfcol=rgb(0.2,0.2,0.2,0.5) , plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8,
           #custom labels
           vlcex=1.2 )
dev.off()

# Susceptibiity
dsuc <- as.data.frame(matrix(c(mean(qdata$qkid[qdata$dzone==2]/5),
                               mean(qdata$qold[qdata$dzone==2]/5),
                               mean(qdata$qpcincome[qdata$dzone==2]/5),
                               mean(qdata$qliterate[qdata$dzone==2]/5),
                               mean(qdata$qpoor[qdata$dzone==2]/5)),
                             ncol=5))
colnames(dsuc) <- c("Children" , "Elderly" , "Income", "Literate", "Poor")
dsuc <- rbind(rep(1,5) , rep(0,5) , dsuc)
tiff(file="Figures/susceptibility_dry.tiff",
     width=6, height=6, units="in", res=300)
radarchart(dsuc,  title="Susceptibility", axistype=1,
           #custom polygon
           pcol=rgb(0.2,0.2,0.2,0.9) , pfcol=rgb(0.2,0.2,0.2,0.5) , plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8,
           #custom labels
           vlcex=1.2 )
dev.off()

dsuc <- as.data.frame(matrix(c(mean(qdata$qkid[qdata$dzone==1]/5),
                               mean(qdata$qold[qdata$dzone==1]/5),
                               mean(qdata$qpcincome[qdata$dzone==1]/5),
                               mean(qdata$qliterate[qdata$dzone==1]/5),
                               mean(qdata$qpoor[qdata$dzone==1]/5)),
                             ncol=5))
colnames(dsuc) <- c("Children" , "Elderly" , "Income", "Literate", "Poor")
dsuc <- rbind(rep(1,5) , rep(0,5) , dsuc)
tiff(file="Figures/susceptibility_wet.tiff",
     width=6, height=6, units="in", res=300)
radarchart(dsuc,  title="Susceptibility", axistype=1,
           #custom polygon
           pcol=rgb(0.2,0.2,0.2,0.9) , pfcol=rgb(0.2,0.2,0.2,0.5) , plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8,
           #custom labels
           vlcex=1.2 )
dev.off()

# Adaptive Capacity
dadp <- as.data.frame(matrix(c(mean(qdata$qwater[qdata$dzone==2]/5),
                               mean(qdata$qgarbage[qdata$dzone==2]/5),
                               mean(qdata$qsewage[qdata$dzone==2]/5),
                               mean(qdata$qurb[qdata$dzone==2]/5),
                               mean(qdata$qfhs[qdata$dzone==2]/5),
                               mean(qdata$qbeds[qdata$dzone==2]/5)),
                             ncol=6))
colnames(dadp) <- c("Water" , "Garbage" , "Sewage", "Urbanization", "Primary Care", "Hospital Beds")
dadp <- rbind(rep(1,6) , rep(0,6) , dadp)
tiff(file="Figures/capacity_dry.tiff",
     width=6, height=6, units="in", res=300)
radarchart(dadp,  title="Adaptive Capacity", axistype=1,
           #custom polygon
           pcol=rgb(0.2,0.2,0.2,0.9) , pfcol=rgb(0.2,0.2,0.2,0.5) , plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8,
           #custom labels
           vlcex=1.2 )
dev.off()

dadp <- as.data.frame(matrix(c(mean(qdata$qwater[qdata$dzone==1]/5),
                               mean(qdata$qgarbage[qdata$dzone==1]/5),
                               mean(qdata$qsewage[qdata$dzone==1]/5),
                               mean(qdata$qurb[qdata$dzone==1]/5),
                               mean(qdata$qfhs[qdata$dzone==1]/5),
                               mean(qdata$qbeds[qdata$dzone==1]/5)),
                             ncol=6))
colnames(dadp) <- c("Water" , "Garbage" , "Sewage", "Urbanization", "Primary Care", "Hospital Beds")
dadp <- rbind(rep(1,6) , rep(0,6) , dadp)
tiff(file="Figures/capacity_wet.tiff",
     width=6, height=6, units="in", res=300)
radarchart(dadp,  title="Adaptive Capacity", axistype=1,
           #custom polygon
           pcol=rgb(0.2,0.2,0.2,0.9) , pfcol=rgb(0.2,0.2,0.2,0.5) , plwd=4,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=0.8,
           #custom labels
           vlcex=1.2 )
dev.off()

