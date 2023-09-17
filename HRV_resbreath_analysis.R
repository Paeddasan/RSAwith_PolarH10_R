# Perpare Environment ---------------------------------------####
rm(list = ls()) #remove environment


if(Sys.info()["nodename"] == "A591-D070" ){
  .libPaths("Z:/Documents/R/Packages")
}
#necessary packages
packages <- c("tidyverse",
              "stringr",
              "runner",
              "ggplot2",
              "cowplot",
              "lubridate",
              "RHRV")
#Install pack if not installed yet
installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packeges])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# set working directory
getwd()
setwd("../HRVTest")

#______________________________________________________________________________
# Preset necessary variables-----------------------------------------------####
bf_results <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(bf_results) <- c("ID", "day", "BF_specified", "BF_ACC", "BF_RSA")
# #plot lists for grids and safe
# plotspec_lst <- list() #list of PSD Plots
# plotHR_lst <- list() # list of HR plots
# plotbreath_lst <- list() #list of breathing plots
# 
# 
# #create results data.frames
# #Time domain HRV results
# HRVtime_results <- data.frame(matrix(nrow = 0, ncol = 10))
# colnames(HRVtime_results) <- c("freq", "size", "SDNN",
#                                "SDANN", "SDNNIDX", "pNN50",
#                                "SDSD",  "rMSSD", "IRRR", "MADRR")
# #Heart rate results
# HeartR_results <-  data.frame(matrix(nrow = 0, ncol = 4))
# colnames(HeartR_results) <- c("freq", "HeartRmin",
#                               "HeartRmax", "HeartRamplitude")
# #Frequency domain HRV results
# HRVspec_results <- data.frame(matrix(nrow = 0, ncol = 17))
# colnames(HRVspec_results) <- c("freq", "sum_HF", "sum_LF", "sum_ULF",
#                                "sum_VLF",  "sum_band",
#                                "max_HF", "max_LF", "max_ULF",
#                                "max_VLF", "max_band",  "min_HF",
#                                "min_LF", "min_ULF", "min_VLF",
#                                "min_band",  "freq_peak")

#used respiration rates
respratios <- rep(c(4.5, 5, 5.5, 6, 6.5), each = 2)


#_____________________________________________________________________________
# Functions--------------------------------------------------------------####

# FUNCTION Loading in the sensor logger files-------------------------------####
#creating a list of all files
#changing the timezone in POSIX
f.load_sensorlogger = function(id= ID, d = day, respR =respratios, dir = getwd()){
  missing_files <- as.data.frame(readxl::read_excel("../MissingFiles_RSI.xlsx", sheet = 1))
  ls_HR <- list()
  files <- list.files()
  loaderror = 0
  #load filenames
  idfiles <- c(paste0("ACC_ID", ID, "_", d, ".txt"),
               paste0("RR_ID", ID, "_", d, ".txt"),
               paste0("MARKER_ID", ID, "_", d, ".txt"))
               #paste0("ECG_ID", ID, "_", d, ".txt"))
  
  #import raw files from Polar Sensor Logger
  if(idfiles[1] %in% files){
    ls_HR$ACC <- readr::read_delim(idfiles[1],
                                   show_col_types = F,
                                   col_names = c("phonetime", "senstime", "X", "Y", "Z"),
                                   skip = 1,
                                   delim = ";") %>%
      as.data.frame()
  } else{
    print(paste0("ACC file ID", id,", day", d, " not found"))
    ls_HR$acc <- NULL
    missing_files <- missing_files %>%
      rbind(c(idfiles[1], NA))
    loaderror = loaderror + 1
  }
  
  if(idfiles[2] %in% files){
    ls_HR$RR <- readr::read_delim(idfiles[2],
                                  show_col_types = F,
                                  col_names = c("phonetime", "RR"),
                                  skip =1,
                                  delim = ";") %>%
      as.data.frame()
  } else{
    print(paste0("RR file ID", id,", day", d, " not found"))
    missing_files <- missing_files %>%
      rbind(c(idfiles[4], NA))
    loaderror = loaderror + 1
    ls_HR$RR <- NULL
  }
  if(idfiles[3] %in% files){
    ls_HR$Marker <- readr::read_delim(idfiles[3],
                                      show_col_types = F,
                                      col_names = c("phonetime", "MarkerType"),
                                      skip =1,
                                      delim = ";") %>%
      as.data.frame()
    
    if(length(ls_HR$Marker$phonetime) == length(respR)) {
      ls_HR$Marker$respratio <- respR
    } else{
      print("Not all Markers found")
      ls_HR$Marker[["respratio"]] <- 1
    }
    
  } else{
    print(paste0("Marker file ID", id,", day", d, " not found"))
    ls_HR$Marker <- NULL
    missing_files <- missing_files %>%
      rbind(c(idfiles[3], NA))
    loaderror = loaderror + 1
  }
  # if(idfiles[4] %in% files){
  #   ls_HR$ECG <- readr::read_delim(idfiles[4],
  #                                  show_col_types = F,
  #                                  col_names = c("phonetime", "sensortime", "stamp", "ECG"),
  #                                  skip =1,
  #                                  delim = ";") %>%
  #     as.data.frame()
  # } else{
  #   print(paste0("ECG file ID", id,", day", d, " not found"))
  #   df_ecg <- NULL
  #   missing_files <- missing_files %>%
  #     rbind(c(idfiles[4], NA))
  #   loaderror = loaderror + 1
  #}
  colnames(missing_files) = c("files", "testdate")
  
  #change timezone and add Marker col
  ls_HR <- sapply(ls_HR, FUN = function(df) {
    df[["phonetime"]] <- force_tz(df[["phonetime"]], tz = "Europe/Berlin")
    #df[["marker"]] <- 0
    
    df
  })
  #add absolute runtime in seconds
  ls_HR <- lapply(ls_HR, function(df){
    #df <- ls_HR$ACC
    df
  })
  #df_rr$sumtime <- cumsum(df_rr$RR)/1000 # absolute time in seconds
  if(loaderror ==0){
    return(ls_HR)
  } else{
    return(loaderror)
  }
  
}

#FUNCTION Adding Markers to all data positions
f.addrespiration = function(ls = ls_HR){
  lsnoMark <- ls[names(ls) != "Marker"] # catch list of all data except marker
  df_Marker = ls_HR$Marker # catch dataframe of Markers
  lsnoMark <- lapply(lsnoMark, FUN = function(df){ #ON Marker = 1, OFF MARKER = 2
    for(i in 1:length(df_Marker$phonetime)){
      
      if(df_Marker$MarkerType[i] == "MARKER_START "){
        #df$marker[which.min(abs(df$phonetime - df_Marker$phonetime[i]))] <- 1
        df$respratio[df$phonetime >= df_Marker$phonetime[i] & df$phonetime <= df_Marker$phonetime[i+1]] <- df_Marker$respratio[i+1]
      }
      if(df_Marker$MarkerType[i] == "MARKER_STOP "){
        #df$marker[which.min(abs(df$phonetime - df_Marker$phonetime[i]))] <- 2
      }
      
    }
    
    df
  })
  ls_HR <- lsnoMark
  return(ls_HR)
}

f.BreathingFanalysis = function(ls = ls_HR){
  ls = ls_HR
  respR <- unique(ls$ACC$respratio[!is.na(ls$ACC$respratio)]) #identify RespRations
  #create new list of separated dataframes by respratio
  ls_BF <- vector("list", length = length(respR)) 
  names(ls_BF) <- paste(respR, "bpm")
  
  #loop through ACC and RR data to separate by RespRatio
  for (i in 1:length(respR)){
    ls_BF[[i]]$ACC <- ls$ACC %>%
      filter(respratio == respR[i])
    ls_BF[[i]]$RR <- ls$RR %>%
      filter(respratio == respR[[i]])
  }
  
  ls_BF <- lapply(ls_BF, FUN = function(lsc){
    lsc <- lapply(lsc, FUN = function(df){
      #cut away the first 12 seconds because of noise and people need some time
      #to get into the breathing rate
      df <- tail(df, length(df$phonetime)-
                   as.integer(
                     12/mean(
                       diff(seconds(df$phonetime))
                       , na.rm =T)))
      
      #sumtime as absolute time with one RespRatio
      df$sumtime <- c(0, cumsum(
        as.numeric(diff(seconds(df$phonetime)))))
      
      
      #calculate velocity and distance 
      if(is.element("Z", names(df))){
        #df$Z <- df$Z - mean(df$Z, na.rm = T)
        
        #number of datapoints for meanrun as part of full length
        dedrifttimeACC = as.integer(length(df$sumtime)/8) 
        #dedrift = running mean over longer time period to take away drift of ACC
        df$Z_dedrift <- df$Z - runner::mean_run(df$Z, k = dedrifttimeACC)
        df$Zvelo <- df$Z_dedrift * c(diff(df$sumtime), NA)
        df$Zdist <- df$Zvelo * c(diff(df$sumtime), NA)
        df$Zdist_avg <- runner::mean_run(df$Zdist, k = 100)
        
      }   
      if(is.element("RR", names(df))){
        df <- filter(df,  RR <= 60*10^3/25 & RR >= 60*10^3/180)
        dedrifttimeHR = as.integer(length(df$sumtime)/8)
        df$HR_dedrift <- 2*df$HR_mean - runner::mean_run(df$HR_mean, k = dedrifttimeHR)
      }
      
      df
    })
    lsc
  })
  for(i in 1:length(ls_BF)){
    
    ls_BF[[i]]$ACC %>%
      ggplot(aes(x = sumtime, y = Z_dedrift)) +
      ggtitle(names(ls_BF[i])) +
      geom_line() ->ls_BF$plots[[i]]
  }
  names(ls_BF$plots) <- paste(respR, "bpm")
  ls_BF$plots$grid <- cowplot::plot_grid(plotlist = ls_BF$plots)
  ls_BF$plots$grid
  
  ls_BF
}

f.identify_peaks = function(ls1 = ls_pks, id = ID, d = day){
  # id = ID
  # d = day
  # ls1 = ls_pks
  ls_df = ls1[["dfs"]]
  for (i in 1:length(ls_df)){
    #min distance between two peak should be dependent on sampling freq -> absolute time about 3 sec
    minpeakdist_acc <- as.integer(8/diff(ls_df[[i]]$ACC$sumtime)[1])
    
    ls_df[[i]]$accpks <- gsignal::findpeaks(ls_df[[i]]$ACC$Zdist_avg +10,
                                            MinPeakHeight = 0.0051+10,
                                            MinPeakDistance = minpeakdist_acc,
                                            DoubleSided = F)
    
    ls_df[[i]]$RR %>%
      ggplot(aes(x = sumtime, y = HR_dedrift)) +
      geom_point()
    
    osdHR <- mean(ls_df[[i]]$RR$HR_dedrift, na.rm = T) +
      0.5*sd(ls_df[[i]]$RR$HR_dedrift, na.rm = T)/2 #minimal peak finder mean + 1sd distance
    minpeakdist_hr <-  as.integer(8/mean(diff(ls_df[[i]]$RR$sumtime), na.rm = T))
    
    ls_df[[i]]$hrpks <- gsignal::findpeaks(ls_df[[i]]$RR$HR_dedrift,
                                           MinPeakHeight = osdHR,
                                           MinPeakDistance = minpeakdist_hr,#min peak distance about 25bpm
                                           DoubleSided = F) 
    
    # osdHR <- mean(ls_df[[i]]$RR$HR_dedrift*(-1)+200, na.rm = T) +
    #   1.5*sd(ls_df[[i]]$RR$HR_dedrift*(-1)+200, na.rm = T)/2 #minimal peak finder mean + 1sd distance
    # ls_df[[i]]$hrminpks <- gsignal::findpeaks(ls_df[[i]]$RR$HR_dedrift*(-1)+200,
    #                                             MinPeakHeight = osdHR,
    #                                             MinPeakDistance = minpeakdist_hr,
    #                                             DoubleSided = F)
  }
  
  ls_plot <- list()
  df_bfreq <- data.frame(matrix(ncol = 3, nrow = 0))
  names(df_bfreq) <- c("TaskFrq", "ObsFreq", "RSAFreq")
  
  for(i in 1:length(ls_df)){
    bf_anlytime <- ls_df[[i]]$ACC$sumtime[tail(ls_df[[i]]$accpks$loc, 1)] -
      ls_df[[i]]$ACC$sumtime[head(ls_df[[i]]$accpks$loc, 1)]
    bfreq <- round(((length(ls_df[[i]]$accpks$loc)-1)/bf_anlytime)*60,1)
    
    hr_anlytime <- ls_df[[i]]$RR$sumtime[tail(ls_df[[i]]$hrpks$loc, 1)] -
      ls_df[[i]]$RR$sumtime[head(ls_df[[i]]$hrpks$loc, 1)]
    rsafreq <- round((length(ls_df[[i]]$hrpks$loc)-1)/hr_anlytime*60, 1)
    df_bfreq <-  rbind(df_bfreq, c(i, bfreq, rsafreq))
    
    ls_plot$ACC[[i]] <-  ggplot(ls_df[[i]]$ACC, aes(x = sumtime, y = Zdist_avg)) +
      geom_line() +
      geom_vline(xintercept = ls_df[[i]]$ACC$sumtime[ls_df[[i]]$accpks$loc],
                 color = "#11897A", linewidth = 0.2) +
      ggtitle(bfreq) +
      xlab("time [s]") +
      ylab("displacement [m]")
    
    ls_plot$RR[[i]] <-  ls_df[[i]]$RR %>%
      ggplot(aes(x = sumtime, y = HR_mean))+
      geom_line() +
      geom_vline( xintercept = ls_df[[i]]$RR$sumtime[ls_df[[i]]$hrpks$loc],
                  color = "#11897A", linewidth = 0.2) +
      geom_vline(xintercept =ls_df[[i]]$RR$sumtime[ls_df[[i]]$hrminpks$loc],
                 color = "#94c154", linewidth = 0.2) +
      ggtitle(rsafreq) +
      xlab("time [s]") +
      ylab("Heart Rate [1/min]")
    
  }
  colnames(df_bfreq) <- c("idx", "ACC_freq", "RSA_freq")
  
  ls_df2 <- list("dfs" = ls_df, "plots" = ls_plot, "bfreq" = df_bfreq)
  
  grid_title <- ggdraw() +
    draw_label(
      paste("ID", id, "day", d, "Breathing Analysis from ACC data"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  ls_df2$plots$ACC$grid <-  cowplot::plot_grid(grid_title,
                                               cowplot::plot_grid(
                                                 plotlist = ls_plot$ACC),
                                               ncol = 1,
                                               rel_heights = c(0.1, 1))
  grid_title <- ggdraw() +
    draw_label(
      paste("ID", id, "day", d, "RSA Analysis from HR data"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  ls_df2$plots$RR$grid <- cowplot::plot_grid(grid_title,
                                             cowplot::plot_grid(
                                               plotlist = ls_plot$RR),
                                             ncol = 1,
                                             rel_heights = c(0.1, 1))
  
  
  return(ls_df2)
  
}

#_____________________________________________________________________________
# Load necessary data--------------------------------------------------####
# set Id and day of test
ID = 9
day = 2


for(ID in 14: 15){
  for(day in 1:2){
    
    ls_HR <- f.load_sensorlogger()
    if(!is.list(ls_HR)){
      print(paste0("Data for ID", ID, " day", day, " found."))
      next()
      
      
    } else{
      print(paste("Fully loaded ID", ID, " day", day))
      
      ls_HR <- f.addrespiration()
      
      # add hearrate
      ls_HR$RR$HR <- 1000/ls_HR$RR$RR*60
      #running mean over HR
      ls_HR$RR$HR_mean <- runner::mean_run(ls_HR$RR$HR, k = 5)
      
      

    }
    
    
    ls_BF <- f.BreathingFanalysis(ls_HR)
    ls_pks <- list()
    ls_pks <- list("dfs" = ls_BF[1:5], "plots" =  ls_BF[[6]])
    
    
    ls_pks <- f.identify_peaks()
    
    # ls_pks$dfs[[2]]$ACC %>%
    #   ggplot(aes(x = sumtime, y = Zdist_avg))+
    #   geom_line() +
    #   geom_line(data = ls_pks$dfs[[2]]$RR, mapping)
    
    ggsave(ls_pks$plots$RR[[2]], file= "exampleRSA.jpeg",
           )
    ls_pks$plots$RR$grid
    ls_pks$plots$ACC$grid
    cowplot::save_plot(filename = paste0("./plots/RSA_BF_ID", ID, "_day", day,".jpeg"), plot = ls_pks$plots$RR$grid)
    cowplot::save_plot(filename = paste0("./plots/ACC_BF_ID", ID, "_day", day,".jpeg"), plot = ls_pks$plots$ACC$grid)
    
    df <- cbind(ID, day, unique(respratios), ls_pks$bfreq[, 2:3])
    colnames(df) <- c("ID", "day", "BF_specified", "BF_ACC", "BF_RSA")
    bf_results <- rbind(bf_results, df)
    print(paste0("Results for ID", ID, "day", day, " added")) 
      
    }
    
    
  }
  
writexl::write_xlsx(bf_results, "Breathing_results.xlsx")






  
  