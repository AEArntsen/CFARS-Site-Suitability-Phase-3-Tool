#CFARS Phase 2 white paper plots (Results)
#2020-07-27

#install and load packages. 
#install.packages("data.table")
#install.packages("openxlsx")
library(data.table)
library(openxlsx)

#### Set Up File Paths ####
#set working directory
wd <- 'C:/Users/05640/Documents/IndustryTasks/CFARS_SS/White Paper'

#set/create output folder
output_path <- paste(wd,'/Output/2020-07-27',sep='')
if (!file.exists(paste(output_path, sep = '')))                    dir.create(file.path(paste(output_path,sep='')), showWarnings=FALSE)
if (!file.exists(paste(output_path,'/input_file_used', sep = ''))) dir.create(file.path(paste(output_path,'/input_file_used',sep='')), showWarnings=FALSE)


#point to the data file and copy it to Output
file_path <- paste(wd,'/Input/CFARS_SS_Phase2_Results_Final_Masked_v02_20200717.xlsx', sep='')   
file.copy(file_path, paste(output_path,'/input_file_used', sep=''))

#### Load data ####
#read the data in for each tab
#ti_bin_count        <- read.xlsx(file_path, sheet = 2) 
#ti_bin_thresholds   <- read.xlsx(file_path, sheet = 3) 
#ti_regression_stats <- read.xlsx(file_path, sheet = 4) 
#ti_values    <- read.xlsx(file_path, sheet = 5) 

#Tab 6 is mbe (old file 20200501 incorrectly it "ti_difference")
ti_mbe       <- read.xlsx(file_path, sheet = 6)  #need openxlsx here (Java heap space errors with xlsx)

#Tab 7 is normalized mbe (old file 200501 incorrectly called it "ti_mbe" and "ti_error")
#ti_mbe_N    <- read.xlsx(file_path, sheet = 7) 

#Tab 8 is rmse 
ti_rmse      <- read.xlsx(file_path, sheet = 8) 

#Tab 9 is CharTI
#char_ti      <- read.xlsx(file_path, sheet = 9) 

#Tab 10 is CharTI_Diff
char_ti_diff <- read.xlsx(file_path, sheet = 10) 



#### Loop 3 times: ti_mbe, ti_rmse, char_ti_diff ####
for (x in 1:3) {
  
  #code will select the dataset to use, and the "base" string name
  #code will set plot title, yaxis lablels, ylimit defaults. 
  if(x==1){
    ti_data  <- ti_mbe
    base_str <- "TI_MBE"  
    plot_name <- "Individual and Aggregated TI MBE"
    plot2_name <- "Aggregated TI MBE"
    y_name    <- "TI MBE [%]"   
    y_range   <- c(-12,12)
    y_vec     <- seq(-12,12,2)
    y_vec_label  <- as.character(y_vec)
    y_incr    <- 2
  }
  if(x==2) {
    ti_data <- ti_rmse
    base_str <- "TI_RMSE"
    plot_name <- "Individual and Aggregated TI RMSE"
    plot2_name <- "Aggregated TI RMSE"
    y_name    <- "TI RMSE [%]"   
    y_range   <- c(-25,25)
    y_vec     <- seq(-25,25,5)
    y_vec_label <- as.character(y_vec)
    y_incr    <- 5
  }
  if(x==3) { 
    ti_data <- char_ti_diff
    base_str <- "CharTI_diff"
    plot_name <- "Individual and Aggr. Char. TI Difference"
    plot2_name <- "Aggregated Characteristic TI Difference"
    y_name    <- "Char TI Difference [%]"   
    y_range   <- c(-25,25)
    y_vec     <- seq(-25,25,5)
    y_vec_label <- as.character(y_vec)
    y_incr    <- 5
  }
  
  #### Prep Aggregated results ####
  #find the row indices of each grouping. string comparisons are case and space sensitive!
  #ignore any corrected RSD results here
  #ignore mast only projects here
  ind_Cup2Cup  <- which(ti_data$ProjectType == "RSD + Mast" &
                         ti_data$Comparison == paste(base_str,"_Ane2_Ref", sep=''))      
  ind_Lidar2Cup <- intersect(which(ti_data$Comparison == paste(base_str,"_RSD_Ref", sep='')), 
                             which(ti_data$Sensor == "WindCube v2" |  #yes space, lower case v
                                   ti_data$Sensor == "WindCubev2.1" | #no space, lower case v
                                   ti_data$Sensor == "ZX"))           #both letters upper case
  ind_Sodar2Cup <- intersect(which(ti_data$Comparison == paste(base_str,"_RSD_Ref", sep='')),  
                             which(ti_data$Sensor == "Triton"))       #first letter upper case
  
  #find the column indices for WS bins 0.5-19.5m/s
  ws_col_start <- which("mean_0.5_mps" == names(ti_data))
  ws_col_end   <- which("mean_19.5_mps" == names(ti_data))
  ws_cols      <- seq(ws_col_start,ws_col_end,1)
  
  #subset the data (we lose project name/meta data)
  #multiply by 100 to get units of TI [%]
  ti_data_Cup2Cup   <- 100*ti_data[ind_Cup2Cup,ws_cols]
  ti_data_Lidar2Cup <- 100*ti_data[ind_Lidar2Cup,ws_cols]
  ti_data_Sodar2Cup <- 100*ti_data[ind_Sodar2Cup,ws_cols]
  
  #Char TI only.. the rows with NA are calculated as 0. 
  #Set them back to NA, so aggregation counts work the same way as MBE and RMSE.
  if (x==3) {
    ti_data_Cup2Cup[ti_data_Cup2Cup == 0] <- NA
    ti_data_Lidar2Cup[ti_data_Lidar2Cup == 0] <- NA
    ti_data_Sodar2Cup[ti_data_Sodar2Cup == 0] <- NA
  }

  
  #### Calculate Aggregated results ####
  #for N counts -- use length/which to count the number that aren't NA
  aggr_ti_data_Cup2Cup    <- apply(ti_data_Cup2Cup,2, function(x) { mean(x, na.rm=TRUE) } ) #apply on margin = 2 means do the function by column
  aggr_ti_data_Cup2Cup_N  <- apply(ti_data_Cup2Cup,2, function(x) { length(which(!is.na(x))) } ) 
  
  aggr_ti_data_Lidar2Cup   <- apply(ti_data_Lidar2Cup,2, function(x) {mean(x, na.rm=TRUE) } ) 
  aggr_ti_data_Lidar2Cup_N <- apply(ti_data_Lidar2Cup,2, function(x) { length(which(!is.na(x))) } ) #apply on margin = 2 means do the function by column
  
  aggr_ti_data_Sodar2Cup    <- apply(ti_data_Sodar2Cup,2, function(x) { mean(x, na.rm=TRUE) } ) 
  aggr_ti_data_Sodar2Cup_N  <- apply(ti_data_Sodar2Cup,2, function(x) { length(which(!is.na(x))) } ) #apply on margin = 2 means do the function by column
  
  #### Save Excel Files ####
  #switch back to regular xlsx for writing .xlsx files
  if (x==1) {
   detach("package:openxlsx", unload = TRUE)
   library(xlsx)
  }

  #first set of files 
  #save the subsetted data used. We grabbed just the data so we could use apply function
  #now add leading 7 columns (metadata) back in. data also has been multiplied by 100 to get %
  excel_file_name <- paste(output_path,'/data_used_', base_str, '.xlsx',sep='') 
  wb <- createWorkbook()
  
  shx <- createSheet(wb,'Cup2Cup')
  addDataFrame(cbind(ti_data[ind_Cup2Cup,seq(1,7,1)], ti_data_Cup2Cup), shx, row.names = FALSE) 
  
  shy <- createSheet(wb,'Lidar2Cup')
  addDataFrame(cbind(ti_data[ind_Lidar2Cup,seq(1,7,1)], ti_data_Lidar2Cup), shy, row.names = FALSE)
  
  shz <- createSheet(wb,'Sodar2Cup')
  addDataFrame(cbind(ti_data[ind_Sodar2Cup,seq(1,7,1)], ti_data_Sodar2Cup), shz, row.names = FALSE)
  
  saveWorkbook(wb,excel_file_name)


  #second file
  #save the aggregated data we calculated. this is what goes into the plot
  #now add leading 7 columns (metadata) back in
  excel_file_name <- paste(output_path,'/data_calcs_', base_str, '.xlsx',sep='') 
  wb2 <- createWorkbook()
  
  shx <- createSheet(wb2, 'aggr_Cup2Cup')
  addDataFrame(rbind(aggr_ti_data_Cup2Cup, aggr_ti_data_Cup2Cup_N), shx, row.names = TRUE)
  
  shy <- createSheet(wb2, 'aggr_Lidar2Cup')
  addDataFrame(rbind(aggr_ti_data_Lidar2Cup, aggr_ti_data_Lidar2Cup_N), shy, row.names = TRUE)
  
  shz <- createSheet(wb2, 'aggr_Sodar2Cup')
  addDataFrame(rbind(aggr_ti_data_Sodar2Cup, aggr_ti_data_Sodar2Cup_N), shz, row.names = TRUE)
  
  saveWorkbook(wb2,excel_file_name)
  
  #### Set Plot Defaults ####
  ws <- seq(0.5,19.5,0.5)   #create ws bin vector (numeric)
  
  #defaults for x axis
  x_range      <- c(0,16)
  x_vec        <- seq(0,16,2)
  x_vec_label  <- as.character(x_vec)
  x_incr       <- 2
  
  
  #### Plot Individual and Aggregated Results ####
  #MBE (x=1), RMBE (x=2), CharTI Diff (x=3)
  #3 panels with individual lines- Cup2Cup, Lidar2Cup, Sodar2Cup
  png(paste(output_path,'/', x, ' ',plot_name,'.png', sep=''),width = 1200, height = 350, res = 100)
    par(mfrow=c(1,3))
    par(mar = c(5, 6, 4, 2)) #bottom, left, top, right
   
    #Cup2Cup first panel
    #plot project 1 to start the plot
    plot(ws, ti_data_Cup2Cup[1,], main = paste(plot_name,'\n','Cup2Cup',sep=''), cex.main = 1.4,
         xlim = x_range, ylim = y_range, 
         type = 'l', lwd = 2, lty = 1,
         xlab = 'Wind Speed [m/s]', ylab = y_name, cex.lab = 1.6,
         yaxs = 'i', xaxs = 'i', axes = F, col = 'lightblue')
    
    grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
    
    
    #now plot project 2 through N
    for (n in 2:nrow(ti_data_Cup2Cup)) {
      lines(ws, ti_data_Cup2Cup[n,], col= 'lightblue', lwd = 2)
    }
    
    abline(h = 0, col = 'black', lty='dotted', lwd=1.5)
    
    #now plot the aggregated result
    lines(ws, aggr_ti_data_Cup2Cup, col= 'blue', lwd = 2)
    
    #finish plot details
    axis(side = 1, cex.axis = 1.2, 
         at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
    axis(side = 2, cex.axis = 1.2, las = 1,
         at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
    legend("bottomright",
           c(paste('Individual (up to N=',length(ind_Cup2Cup),')',sep=''),
             'Aggregated'), 
           col = c("lightblue","blue"),
           lty = 1, lwd = 2, bg = 'white', cex = 1.3)
    box()
  
    
    #Lidar2Cup second panel
    #plot project 1 to start the plot
    plot(ws, ti_data_Lidar2Cup[1,], main = paste(plot_name,'\n','Lidar2Cup',sep=''), cex.main = 1.4,
         xlim = x_range, ylim = y_range, 
         type = 'l', lwd = 2, lty = 1,
         xlab = 'Wind Speed [m/s]', ylab = y_name, cex.lab = 1.6,
         yaxs = 'i', xaxs = 'i', axes = F, col = 'lightgreen')
    
    grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
    
    
    #now plot project 2 through N
    for (n in 2:nrow(ti_data_Lidar2Cup)) {
      lines(ws, ti_data_Lidar2Cup[n,], col= 'lightgreen', lwd = 2)
    }
    
    abline(h = 0, col = 'black', lty='dotted', lwd=1.5)
    
    #now plot the aggregated result
    lines(ws, aggr_ti_data_Lidar2Cup, col= 'green4', lwd = 2)
    
    #finish plot details
    axis(side = 1, cex.axis = 1.2, 
         at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
    axis(side = 2, cex.axis = 1.2, las = 1,
         at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
    legend("bottomright",
           c(paste('Individual (up to N=',length(ind_Lidar2Cup),')',sep=''),
             'Aggregated'), 
           col = c("lightgreen","green4"),
           lty = 1, lwd = 2, bg = 'white', cex = 1.3)
    box()
    
    #Sodar2Cup third panel
    #plot project 1 to start the plot
    plot(ws, ti_data_Sodar2Cup[1,], main = paste(plot_name,'\n','Sodar2Cup',sep=''), cex.main = 1.4,
         xlim = x_range, ylim = y_range, 
         type = 'l', lwd = 2, lty = 1,
         xlab = 'Wind Speed [m/s]', ylab = y_name, cex.lab = 1.6,
         yaxs = 'i', xaxs = 'i', axes = F, col = 'goldenrod1')
    
    grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
    
    
    #now plot project 2 through N
    for (n in 2:nrow(ti_data_Sodar2Cup)) {
      lines(ws, ti_data_Sodar2Cup[n,], col= 'goldenrod1', lwd = 2)
    }
    
    abline(h = 0, col = 'black', lty='dotted', lwd=1.5)
    
    #now plot the aggregated result
    lines(ws, aggr_ti_data_Sodar2Cup, col= 'orangered2', lwd = 2)
    
    #finish plot details
    axis(side = 1, cex.axis = 1.2, 
         at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
    axis(side = 2, cex.axis = 1.2, las = 1,
         at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
    legend("bottomright",
           c(paste('Individual (up to N=',length(ind_Sodar2Cup),')',sep=''),
             'Aggregated'), 
           col = c("goldenrod1","orangered2"),
           lty = 1, lwd = 2, bg = 'white', cex = 1.3)
    box()
    dev.off()
  
    #### Plot Aggregated Only ####
    #MBE (x=1), RMBE (x=2), CharTI Diff (x=3)
    #1 plot, 1 panel. 3 average lines (Cup2Cup, Lidar2Cup, Sodar2Cup)
    
    if (x==1){ #TI MBE plot, zoom in for aggregated results to +/-8
      y_range   <- c(-8,8)
      y_vec     <- seq(-8,8,2)
      y_vec_label  <- as.character(y_vec)
      y_incr    <- 2
    }
    if (x==3){ #Char TI plot, zoom in for aggregated results to +/-10
      y_range   <- c(-10,10)
      y_vec     <- seq(-10,10,2)
      y_vec_label  <- as.character(y_vec)
      y_incr    <- 2
    }
    
    png(paste(output_path,'/', x+3, ' ',plot2_name,'.png', sep=''),width = 800, height = 400, res = 100)
      par(mfrow=c(1,1))
      par(mar = c(5, 6, 4, 2)) #bottom, left, top, right
      
      #Cup2Cup
      plot(ws, aggr_ti_data_Cup2Cup, main = plot2_name, cex.main = 1.4,
           xlim = x_range, ylim = y_range,
           type = 'l', lwd = 2, lty = 1, col = 'blue',
           xlab = 'Wind Speed [m/s]', ylab = y_name, cex.lab = 1.6,
           yaxs = 'i', xaxs = 'i', axes = F)
      
      grid((x_range[2]-x_range[1])/x_incr, (y_range[2] - y_range[1])/y_incr) #number of x grid lines, number of y grid lines
      
      abline(h = 0, col = 'black', lty='dotted', lwd=1.5)
      
      #Lidar2Cup and Sodar2Cup
      lines(ws, aggr_ti_data_Lidar2Cup, col= 'green4', lwd = 2)
      lines(ws, aggr_ti_data_Sodar2Cup, col= 'orangered2', lwd = 2)
      
      axis(side = 1, cex.axis = 1.2,
           at = seq(x_range[1],x_range[2],x_incr), labels = x_vec_label)
      axis(side = 2, cex.axis = 1.2, las = 1,
           at = seq(y_range[1],y_range[2],y_incr), labels = y_vec_label)
      box()
      legend("bottomleft",
             c(paste('Sodar2Cup (up to N=',length(ind_Sodar2Cup),')', sep=''),
               paste('Lidar2Cup (up to N=',length(ind_Lidar2Cup),')', sep=''),
               paste('Cup2Cup (up to N=',length(ind_Cup2Cup),')', sep='')),
             col = c("orangered2", "green4", "blue"),
             lty = 1, lwd = 2, bg = 'white', cex = 0.9)
    dev.off()

}


#### Clean Up ####
detach("package:xlsx", unload = TRUE)
rm(ind_Cup2Cup, ind_Lidar2Cup, ind_Sodar2Cup, ws_col_start, ws_col_end, ws_cols)
rm(ti_data_Cup2Cup, ti_data_Lidar2Cup, ti_data_Sodar2Cup)
rm(aggr_ti_data_Cup2Cup, aggr_ti_data_Lidar2Cup, aggr_ti_data_Sodar2Cup)
rm(aggr_ti_data_Cup2Cup_N, aggr_ti_data_Lidar2Cup_N, aggr_ti_data_Sodar2Cup_N)
rm(shx, shy, shz, wb, wb2)
rm(base_str, excel_file_name, file_path)
rm(output_path, plot_name, wd, ws, x)
rm(y_incr, y_name, y_range, y_vec, y_vec_label, x_incr,  x_range, x_vec, x_vec_label)
rm(ti_data, ti_mbe, ti_rmse, char_ti_diff, n, plot2_name)
