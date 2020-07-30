#CFARS Phase 2 white paper plots (Sample Data)
#2020-07-27, now using other UL dataset

#install and load packages. 
#install.packages("data.table")
#install.packages("openxlsx")
library(data.table)
library(plyr)
#library(openxlsx)

#### Set Up File Paths ####
#set working directory
wd <- 'C:/Users/05640/Documents/IndustryTasks/CFARS_SS/White Paper'

#set/create output folder
output_path <- paste(wd,'/Output/2020-07-27b',sep='')
if (!file.exists(paste(output_path, sep = '')))                    dir.create(file.path(paste(output_path,sep='')), showWarnings=FALSE)
if (!file.exists(paste(output_path,'/input_file_used', sep = ''))) dir.create(file.path(paste(output_path,'/input_file_used',sep='')), showWarnings=FALSE)


#point to the data file and copy it to Output
file_path <- paste(wd,'/Input/07common-139m-exported-simple.csv', sep='')   
file.copy(file_path, paste(output_path,'/input_file_used', sep=''))

#### Load data ####
mydata <- read.csv(file_path)

#calc TI Diff and TI Diff2, also convert to %
mydata$TIDiff_Cup2Cup    <- 100*(mydata$TI_Cup2 -  mydata$TI_Cup1)
mydata$TIDiff_RSD2Cup  <- 100*(mydata$TI_RSD -  mydata$TI_Cup1)
mydata$TIDiff2_Cup2Cup   <- mydata$TIDiff_Cup2Cup * mydata$TIDiff_Cup2Cup
mydata$TIDiff2_RSD2Cup <- mydata$TIDiff_RSD2Cup * mydata$TIDiff_RSD2Cup
  


#### Set plotting defaults - x axis ####
#create ws bin vector (numeric)
ws <- seq(0.5,19.5,0.5)
ti <- seq(0,40,5)

#X = WS
x_range      <- c(0,20)
x_vec        <- seq(0,20,2)
x_vec_label  <- as.character(x_vec)
x_incr       <- 2

#Y = TI
y_range      <- c(0,80)
y_vec        <- seq(0,80,10)
y_vec_label  <- as.character(y_vec)
y_incr       <- 10

#Z = TI Diff
z_range <- c(-50,50)
z_vec        <- seq(-50,50,10)
z_vec_label  <- as.character(z_vec)
z_incr       <- 10

#A = MBE
a_range <- c(-10,10)
a_vec        <- seq(-10,10,2)
a_vec_label  <- as.character(a_vec)
a_incr       <- 2

#B = RMSE
b_range <- c(-20,20)
b_vec        <- seq(-20,20,5)
b_vec_label  <- as.character(b_vec)
b_incr       <- 5

#C = Char TI
c_range <- c(10,50)
c_vec        <- seq(10,50,10)
c_vec_label  <- as.character(c_vec)
c_incr       <- 10

#d = Char TI DIFF
d_range <- c(-15,15)
d_vec        <- seq(-15,15,5)
d_vec_label  <- as.character(d_vec)
d_incr       <- 5

#### Plot TI Scatter #### 
png(paste(output_path,'/EXAMPLE 1 TI vs WS scatter.png', sep=''),width = 1200, height = 350, res = 100)
  par(mfrow=c(1,3))
  par(mar = c(5, 6, 4, 2)) #bottom, left, top, right
  
  #TI_Cup1
  plot(mydata$WS_Cup1_Avg, 100*mydata$TI_Cup1, main = paste('Example Dataset','\n', 'Cup1'), cex.main = 1.4,
       pch = 16, col = 'black', 
       xlim = x_range, ylim = y_range, 
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI [%]', cex.lab = 2,
       yaxs = 'i', xaxs = 'i', axes = F)

  grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1.3, 
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1.3, las = 1,
       at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
  box()
  
  #TI_Cup2
  plot(mydata$WS_Cup1_Avg, 100*mydata$TI_Cup2, main = paste('Example Dataset','\n', 'Cup2'), cex.main = 1.4,
       pch = 16, col = 'blue',
       xlim = x_range, ylim = y_range, 
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI [%]', cex.lab = 2,
       yaxs = 'i', xaxs = 'i', axes = F)
  
  grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1.3, 
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1.3, las = 1,
       at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
  box()
  
  
  #TI_RSD
  plot(mydata$WS_Cup1_Avg, 100*mydata$TI_RSD, main = paste('Example Dataset','\n', 'RSD'), cex.main = 1.4,
       pch = 16, col = 'green4', 
       xlim = x_range, ylim = y_range, 
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI [%]', cex.lab = 2,
       yaxs = 'i', xaxs = 'i', axes = F)
  
  grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1.3, 
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1.3, las = 1,
       at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
  box()

dev.off()

#### Plot TI DIFF Scatter #### 
png(paste(output_path,'/EXAMPLE 2 TIDiff vs WS scatter.png', sep=''),width = 900, height = 400, res = 100)
  par(mfrow=c(1,2))
  par(mar = c(5, 5, 4, 2)) #bottom, left, top, right
  
  #Cup2Cup
  plot(mydata$WS_Cup1_Avg, mydata$TIDiff_Cup2Cup, main = paste('Example Dataset','\n','Cup2Cup'), 
       pch = 16, col = 'blue', 
       xlim = x_range, ylim = z_range, 
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI Difference [%]', cex.lab = 1.5,
       yaxs = 'i', xaxs = 'i', axes = F)
  
  grid((x_range[2]-x_range[1])/x_incr, (z_range[2] - z_range[1])/z_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1, 
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1, las = 1,
       at = seq(z_range[1],z_range[2],z_incr), labels = z_vec_label)
  box()
  
  #RSD2Cup
  plot(mydata$WS_Cup1_Avg, mydata$TIDiff_RSD2Cup, main = paste('Example Dataset','\n','RSD2Cup'), 
       pch = 16, col = 'green4',
       xlim = x_range, ylim = z_range, 
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI Difference [%]', cex.lab = 1.5,
       yaxs = 'i', xaxs = 'i', axes = F)
  
  grid((x_range[2]-x_range[1])/x_incr, (z_range[2] - z_range[1])/z_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1, 
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1, las = 1,
       at = seq(z_range[1],z_range[2],z_incr), labels = z_vec_label)
  box()

dev.off()


#### Bin it ####
#make 0.5 m/s bins
breaks <- c(0,seq(0.25, 20.25, by=0.5)) 
bin_number <- .bincode(mydata$WS_Cup1_Avg, breaks, right=FALSE)
mydata$WS_bin <- bin_number*0.5 -0.5

#save
write.csv(mydata, file = paste(output_path,'/example_mydata.csv', sep=''), row.names = FALSE)


#bin WS and TI. TI Avg and TI Std go into CharTI.
#Also bin TIDiff and TIDiff^2. That is MBE and RMSE!
binned <- ddply(mydata, "WS_bin", summarise, 
                BinCount=  length(WS_bin)  , 
                WS.Cup1.Avg = round( mean(WS_Cup1_Avg) ,2), 
                WS.Cup1.Std = round( sd(WS_Cup1_Avg) ,2), 
                TI.Cup1.Avg = round( mean(100*TI_Cup1) ,2), 
                TI.Cup1.Std = round( sd(100*TI_Cup1) ,2), 
                
                WS.Cup2.Avg = round( mean(WS_Cup2_Avg) ,2), 
                WS.Cup2.Std = round( sd(WS_Cup2_Avg) ,2), 
                TI.Cup2.Avg = round( mean(100*TI_Cup2) ,2), 
                TI.Cup2.Std = round( sd(100*TI_Cup2) ,2),
                
                WS.RSD.Avg = round( mean(WS_RSD_Avg) ,2), 
                WS.RSD.Std = round( sd(WS_RSD_Avg) ,2), 
                TI.RSD.Avg = round( mean(100*TI_RSD) ,2), 
                TI.RSD.Std = round( sd(100*TI_RSD) ,2),

                MBE_Cup2Cup    = round( mean(TIDiff_Cup2Cup), 4), 
                MBE_RSD2Cup  = round( mean(TIDiff_RSD2Cup), 4), 
                RMSE_Cup2Cup   = round( sqrt(mean(TIDiff2_Cup2Cup)), 4), 
                RMSE_RSD2Cup = round( sqrt(mean(TIDiff2_RSD2Cup)), 4)) 

#add Char TI
binned$CharTI.Cup1 <- round(binned$TI.Cup1.Avg + 1.28*binned$TI.Cup1.Std,2)
binned$CharTI.Cup2 <- round(binned$TI.Cup2.Avg + 1.28*binned$TI.Cup2.Std,2)
binned$CharTI.RSD  <- round(binned$TI.RSD.Avg + 1.28*binned$TI.RSD.Std,2)

#add Char TI diff
binned$CharTIDiff_Cup2Cup   <- binned$CharTI.Cup2 - binned$CharTI.Cup1
binned$CharTIDiff_RSD2Cup <- binned$CharTI.RSD - binned$CharTI.Cup1

#reorder columns.
binned <- binned[,c(1,2,3,4,5,6,19,
                    7,8,9,10,20,
                    11,12,13,14,21,
                    15,16,17,18,22,23)]

#truncate to just complete bins. otherwise TI_Std and CharTI don't calculate.
binned <- binned[binned$BinCount >= 3,]

#truncate to remove 0.5 m/s, starting at 1.0 m/s instead. There is enough data in 0.5 m/s bin but 
#it is throwing off the axes. just showing this data as an example anyway
binned <- binned[-1,]

#save output table
write.csv(binned, file = paste(output_path,'/example_binned.csv', sep=''), row.names = FALSE)
     
#### Plot TI Diff (MBE) Binned ####
png(paste(output_path,'/EXAMPLE 3 MBE vs WS.png', sep=''),width = 800, height = 400, res = 100)
  par(mfrow=c(1,1))
  par(mar = c(5, 5, 4, 2)) #bottom, left, top, right

  #TI_Cup1
  plot(binned$WS_bin, binned$MBE_Cup2Cup, main = paste('Example Dataset',"\n", "TI MBE"),
       xlim = x_range, ylim = a_range,
       type = 'l', lwd = 2, lty = 1, col = 'blue',
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI MBE [%]', cex.lab = 1.6,
       yaxs = 'i', xaxs = 'i', axes = F)
  #xaxt = 'n', yaxt = 'n')
  lines(binned$WS_bin, binned$MBE_RSD2Cup, col= 'green4', lwd = 2)
  
  grid((x_range[2]-x_range[1])/x_incr, (a_range[2] - a_range[1])/a_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1.2,
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1.2, las = 1,
       at = seq(a_range[1],a_range[2],a_incr), labels = a_vec_label)
  box()
  legend("bottomright",
         c('RSD2Cup','Cup2Cup'),
         col = c("green4","blue"),
         lty = 1, lwd = 2, bg = 'white', cex = 1.1)
  abline(h = 0, col = 'black', lty='dotted', lwd=1.5)
  
dev.off()

#### Plot TI Diff^2 (RMSE) Binned ####
png(paste(output_path,'/EXAMPLE 4 RMSE vs WS.png', sep=''),width = 800, height = 400, res = 100)
  par(mfrow=c(1,1))
  par(mar = c(5, 5, 4, 2)) #bottom, left, top, right
  
  #TI_Cup1
  plot(binned$WS_bin, binned$RMSE_Cup2Cup, main = paste('Example Dataset',"\n", "TI RMSE"),
       xlim = x_range, ylim = b_range,
       type = 'l', lwd = 2, lty = 1, col = 'blue',
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'TI RMSE [%]', cex.lab = 1.6,
       yaxs = 'i', xaxs = 'i', axes = F)
  #xaxt = 'n', yaxt = 'n')
  lines(binned$WS_bin, binned$RMSE_RSD2Cup, col= 'green4', lwd = 2)
  
  grid((x_range[2]-x_range[1])/x_incr, (b_range[2] - b_range[1])/b_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1.2,
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1.2, las = 1,
       at = seq(b_range[1],b_range[2],b_incr), labels = b_vec_label)
  box()
  legend("bottomright",
         c('RSD2Cup','Cup2Cup'),
         col = c("green4","blue"),
         lty = 1, lwd = 2, bg = 'white', cex = 1.1)
  abline(h = 0, col = 'black', lty='dotted', lwd=1.5)
  
dev.off()

#### Plot Char TI ####
png(paste(output_path,'/EXAMPLE 5 CharTI vs WS.png', sep=''),width = 800, height = 400, res = 100)
  par(mfrow=c(1,1))
  par(mar = c(5, 5, 4, 2)) #bottom, left, top, right
  
  #TI_Cup1
  plot(binned$WS_bin, binned$CharTI.Cup1, main = paste('Example Dataset',"\n", "Characteristic TI"),
       xlim = x_range, ylim = c_range,
       type = 'l', lwd = 2, lty = 1, col = 'black',
       xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'Characteristic TI [%]', cex.lab = 1.6,
       yaxs = 'i', xaxs = 'i', axes = F)
  #xaxt = 'n', yaxt = 'n')
  lines(binned$WS_bin, binned$CharTI.Cup2, col= 'blue', lwd = 2)
  lines(binned$WS_bin, binned$CharTI.RSD, col= 'green4', lwd = 2)
  
  grid((x_range[2]-x_range[1])/x_incr, (c_range[2] - c_range[1])/c_incr) #number of x grid lines, number of y grid lines
  axis(side = 1, cex.axis = 1.2,
       at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
  axis(side = 2, cex.axis = 1.2, las = 1,
       at = seq(c_range[1],c_range[2],c_incr), labels = c_vec_label)
  box()
  legend("topright",
         c('RSD','Cup2','Cup1'),
         col = c("green4","blue","black"),
         lty = 1, lwd = 2, bg = 'white', cex = 1.1)
  
dev.off()

#### Plot Char TI DIFF ####
png(paste(output_path,'/EXAMPLE 6 CharTIDiff vs WS.png', sep=''),width = 800, height = 400, res = 100)
par(mfrow=c(1,1))
par(mar = c(5, 5, 4, 2)) #bottom, left, top, right

#TI_Cup1
plot(binned$WS_bin, binned$CharTIDiff_Cup2Cup, main = paste('Example Dataset',"\n", "Characteristic TI Difference"),
     xlim = x_range, ylim = d_range,
     type = 'l', lwd = 2, lty = 1, col = 'blue',
     xlab = expression('WS'['Cup1']*' [m/s]'), ylab = 'Char TI Difference [%]', cex.lab = 1.6,
     yaxs = 'i', xaxs = 'i', axes = F)
#xaxt = 'n', yaxt = 'n')
lines(binned$WS_bin, binned$CharTIDiff_RSD2Cup, col= 'green4', lwd = 2)

grid((x_range[2]-x_range[1])/x_incr, (d_range[2] - d_range[1])/d_incr) #number of x grid lines, number of y grid lines
axis(side = 1, cex.axis = 1.2,
     at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
axis(side = 2, cex.axis = 1.2, las = 1,
     at = seq(d_range[1],d_range[2],d_incr), labels = d_vec_label)
box()
legend("bottomright",
       c('RSD2Cup','Cup2Cup'),
       col = c("green4","blue"),
       lty = 1, lwd = 2, bg = 'white', cex = 1.1)
abline(h = 0, col = 'black', lty='dotted', lwd=1.5)

dev.off()

#clean up
rm(binned, mydata)
rm(a_incr, a_range, a_vec, a_vec_label)
rm(b_incr, b_range, b_vec, b_vec_label)
rm(c_incr, c_range, c_vec, c_vec_label)
rm(d_incr, d_range, d_vec, d_vec_label)
rm(x_incr, x_range, x_vec, x_vec_label)
rm(y_incr, y_range, y_vec, y_vec_label)
rm(z_incr, z_range, z_vec, z_vec_label)
rm(bin_number, breaks, file_path, output_path, ti, wd, ws)
