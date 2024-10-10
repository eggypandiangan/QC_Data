#### PROGRAM QC DATA TEMPERATURE DARI FKLIM 71 #######################
#### Program ini ditujukan untuk meng-QC data T07, T13, T18, Tave, Tmax,Tmin
#### dari FKLIM 71
#### Metode QC merujuk pada aturan WMO....
#### Program ini dibuat oleh Sdr. Robi Muharsyah (robi.muharsyah@gmail.com)
#### Penggunaan dari program ini harap mendapatkan ijin dari pembuat
# -------------------------------------------------------------------------
rm(list = ls())
setwd('D:/BELAJAR/OMAN_TRAINING/topic3_Matlab_basisc_QC_daily_Temp/topic3_R_basisc_QC_daily_Temp') #GANTI DIREKTORI
library(plyr)
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}
## 
dirData <- paste0(getwd(),'/ORI_Temperature_1981-2023/') #direktori data perbalai
outdir <- paste0(getwd(),'/AllTest_QC_output');dir.create(outdir, recursive = T)
##
filenameTave <- 'Rekap_tave_bmkgsoft_fklim_1981-2023.xlsx';
filenameTmin <- 'Rekap_tmin_bmkgsoft_fklim_1981-2023.xlsx';
filenameTmax <- 'Rekap_tmax_bmkgsoft_fklim_1981-2023.xlsx';
tave <- readxl::read_excel(paste0(dirData,filenameTave),na='', guess_max = Inf);
tmin <- readxl::read_excel(paste0(dirData,filenameTmin),na='', guess_max = Inf);
tmax <- readxl::read_excel(paste0(dirData,filenameTmax),na='', guess_max = Inf);
##
info1 <- read.csv('List_stawmoid_utk_dicek.csv');
listSta = info1[,3][]
##
for (itsta in 1:length(listSta)) {
  # itsta <- 1 ########
  # itsta <- length(listSta)-1 ########
  namefile <- listSta[itsta]
  print(namefile)
  # print(which(listSta == namefile))
  # nosta <- nowmo; namefile <- nosta;
  outfile <- paste0(getwd(),namefile)
  dir.create(paste0(getwd(),'/graphic/',namefile), recursive = T)
  ##
  vecDates <- paste(sprintf('%02d',tave[[3]]),month.abb[tave[[2]]],tave[[1]], sep = '-')
  idgetvecDate <- which(substr(vecDates,1,2)=='01' & substr(vecDates,4,6)=='Jan')
  getvecDates <- vecDates[idgetvecDate[seq(1,length(idgetvecDate),6)]]
  ##
  # T <- cbind(tave[,match(namefile,colnames(tave))],tmax[,match(namefile,colnames(tmax))],tmin[,match(namefile,colnames(tmin))])
  T <- cbind(tave[colnames(tave)==namefile],tmax[colnames(tmax)==namefile],tmin[colnames(tmin)==namefile])
  colnames(T) <- c('TAV','TMX','TMN')
  T$TAV[T$TAV==9999] <- NA; T$TAV[T$TAV==8888] <- NA 
  T$TMX[T$TMX==9999] <- NA; T$TMX[T$TMX==8888] <- NA 
  T$TMN[T$TMN==9999] <- NA; T$TMN[T$TMN==8888] <- NA 
  ### MEMULAI PROSES QC -------------------------------------------------------
  ### Terdiri dari empat step berdasarkan rujukan WMO
  ### Tahap I  : TEST1 dan TEST2
  ### Tahap II : TEST3 dan TEST4
  
  ### MULAI TAHAP I -----------------------------------------------------------
  ### Tiap-tiap temperature di QC
  linecolor <- c('green','red','blue')
  Tcorr <- data.frame(c())
  all_date_test1a  <- data.frame(c())
  all_date_test1b <- data.frame(c())
  all_date_test1c <- data.frame(c())
  
  all_date_test2a <- data.frame(c())
  all_date_test2b <- data.frame(c())
  
  all_date_test3 <- data.frame(c())
  all_date_test4 <- data.frame(c())
  
  ### TEST 1 ------------------------------------------------------------------
  ### 11111 Gross Check Error: Abberant Values Test 1111 ----------------------
  ### 11111 Gross Check Error: Consistency of calendar dates 1111 -------------
  ### 11111 Gross Check Error: Comparison of daily averages 1111 --------------
  
    for (itr in 1:dim(T)[2]) {
    # Abberant Values Test 1111
    # itr <- 1 #######
    Tcorr0 <- T[,itr]
    # Tcorr0[200] <- 46 #
    # Tcorr0[201] <- 4 #
    idxAb <- which(Tcorr0>45 | Tcorr0<5)
    Tcorr0[idxAb] <- NA
    date_test1a <- ifelse(identical(vecDates[unique(idxAb)],character(0)),NA,vecDates[unique(idxAb)])
    all_date_test1a <- cbind.fill(all_date_test1a,date_test1a)
    ### PLOTTING --------------------------------------------------------------
    if (!identical(idxAb, integer(0))) {
      png(filename = paste0('graphic/',namefile,'/GrossCheck_Test_',colnames(T)[itr],'_',namefile,'.png'),
          res = 200, width = 1600, height = 1200)
      par(mar = c(1.5, 1.4, 1.8, 1))
      plot(x = 1:length(vecDates), y = T[,itr], type = 'l', col = linecolor[itr],
           lwd = 1.5, ylim = c(0,45), xaxt='n', yaxt='n', xlab = '', ylab='',
           panel.first = {
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                               labels = NA, tcl = 0.3, tick = T, cex.axis=0.6, 
                  las=1, line=0, lwd=1.6);
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = getvecDates, tcl = 0.3, tick = F, cex.axis=0.6, 
                  las=1, line=-0.8, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = NA, tick = T, 
                  tcl = 0.3, cex.axis=0.6,las=1, line=0, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = seq(0,45,5), tick = F,
                  tcl = 1, cex.axis=0.6,las=1, line=-0.8, lwd=1.6);
             abline(h = seq(0,45,5), lty = 1, lwd=1.3, col='gray')
             abline(v = idgetvecDate[seq(1,length(idgetvecDate),6)], lty = 1,
                    lwd=1.3, col='gray')
             })
      abline(h = c(5,40), lty = 2, lwd=2)
      title(main = paste0(colnames(T)[itr],': ',namefile), line = 0.2, cex.main=1.6)
      dev.off()
    }
    Tcorr <- cbind.fill(Tcorr,Tcorr0)
  }
  # Consistency dates
  cekDt <- as.vector(diff(as.Date(vecDates, format='%d-%b-%Y')))
  # cekDt[10] <- 6 #
  # cekDt[44] <- 4 #
  idxDt <- which(cekDt!=1)
  gapDt <- cekDt[idxDt]
  if (!identical(gapDt, numeric(0))) {
    LostDt <- c()
    for (iter1 in 1:length(gapDt)) {
      # iter1 <- 1
      lostD <- format(seq(as.Date(vecDates[idxDt[iter1]+1], format='%d-%b-%Y'), by='day', length.out=gapDt[iter1]-1),'%d-%b-%Y')
      LostDt <- c(LostDt,lostD)
    }
    all_date_test1b <- cbind.fill(all_date_test1b,LostDt)
    Tcorr <- rbind.fill(data.frame('date'=vecDates,Tcorr),data.frame('date'=LostDt))
    Tcorr <- Tcorr[order(as.Date(Tcorr[,1], format = '%d-%b-%Y'), decreasing = F),]
    vecDates <- Tcorr[[1]]
    idgetvecDate <- which(substr(vecDates,1,2)=='01' & substr(vecDates,4,6)=='Jan')
    getvecDates <- vecDates[idgetvecDate[seq(1,length(idgetvecDate),6)]]
  }
  
  # TEST 2 ------------------------------------------------------------------
  # Tolerance test 2222aaa --------------------------------------------------
  # Nilai yg lebih dari mean+4*stdv dan kurang dari mean-4*stdv dihilangkan
  # Tolerance test 2222bbb --------------------------------------------------
  # -------------------------------------------------------------------------
  for (itr in 1:dim(T)[2]) {
    # itr <- 1 ###
    Tcorr0 <- Tcorr[,itr]
    # Tcorr0[10] <- 29 ##
    # Tcorr0[11] <- 29 ##
    # Tcorr0[12] <- 29 ##
    # Tcorr0[13] <- 29 ##
    # Tcorr0[14] <- 29 ##
    # Tcorr1[30]= 33 ##
    # Tcorr1[25]= 15 ##
    Tcorr1 <- Tcorr0
    miu <- mean(na.omit(Tcorr1))
    std <- sd(na.omit(Tcorr1))
    
    ta <- miu+4*std
    tb <- miu-4*std
    more_tress_a <- which(Tcorr1>ta)
    more_tress_b <- which(Tcorr1<tb)
    agettress <- c(more_tress_a,more_tress_b)
    
    Tcorr1[agettress] <- NA
    date_test2a <- vecDates[sort(unique(agettress))]
    all_date_test2a <- cbind.fill(all_date_test2a,date_test2a)
    
    # Nilai yg berturut2 sama lebih dari 4 kali -------------------------------
    y <- 0
    yy <- integer(length(Tcorr1))
    for (iter in 1:(length(Tcorr1) - 1)) {
      if (!is.na(Tcorr1[iter]) && !is.na(Tcorr1[iter + 1]) && Tcorr1[iter] == Tcorr1[iter + 1]) {
        y <- y + 1
      } else {
        y <- 0
      }
      yy[iter + 1] <- y
    }
    loc <- which(yy>=4)
    
    agetloc <- integer(0)
    date_test2b <- character(0)
    if (!identical(loc,integer(0))) {
      agetloc <- integer(0)
      for (iter1 in 1:length(loc)) {
        # iter1 <- 1 ####
        getloc <- (loc[iter1] - 3):loc[iter1]
        agetloc <- c(agetloc, getloc)
      }
      agetloc <- unique(agetloc)
      Tcorr1[agetloc] <- NA
      date_test2b <- vecDates[agetloc]
    }
    all_date_test2b <- cbind.fill(all_date_test2b,date_test2b)
    
    # PLOTTING ----------------------------------------------------------------
    sameTcorr1 <- Tcorr0
    sameTcorr1[!1:length(sameTcorr1) %in% agetloc] <- NA
    if (itr == 1) {
      y_limits <- c(15, 35)
    } else if (itr %in% 2:4) {
      y_limits <- c(10, 40)
    } else {
      y_limits <- c(15, 35)
    }
    png(filename = paste0('graphic/',namefile,'/Tolerance_Test_',colnames(T)[itr],'_',namefile,'.png'),
        res = 200, width = 1600, height = 1200)
    par(mar = c(1.5, 1.4, 1.8, 1))
    plot(x = 1:length(vecDates), y = Tcorr0, type = 'l', col = linecolor[itr],
         lwd = 1.5, ylim = y_limits, xaxt='n', yaxt='n', xlab = '', ylab='',
         panel.first = {
           axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                labels = NA, tcl = 0.3, tick = T, cex.axis=0.6, 
                las=1, line=0, lwd=1.6);
           axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                labels = getvecDates, tcl = 0.3, tick = F, cex.axis=0.6, 
                las=1, line=-0.8, lwd=1.6);
           axis(side = 2, at = seq(0,45,5),labels = NA, tick = T, 
                tcl = 0.3, cex.axis=0.6,las=1, line=0, lwd=1.6);
           axis(side = 2, at = seq(0,45,5),labels = seq(0,45,5), tick = F,
                tcl = 1, cex.axis=0.6,las=1, line=-0.8, lwd=1.6);
           abline(h = seq(0,45,5), lty = 1, lwd=1.3, col='gray')
           abline(v = idgetvecDate[seq(1,length(idgetvecDate),6)], lty = 1,
                  lwd=1.3, col='gray')
         })
    abline(h = c(tb,ta), lty = 1, lwd=2)
    text(length(vecDates) / 2, ta + 0.4, expression(mu + 4 * sigma), cex = 1.3, pos = 4)
    text(length(vecDates) / 2, tb - 0.5, expression(mu - 4 * sigma), cex = 1.3, pos = 4)
    lines(x = 1:length(vecDates), y = sameTcorr1, pch = 8, type = 'o', lwd = 1.5)
    title(main = paste0(colnames(T)[itr],': ',namefile), line = 0.2, cex.main=1.6)
    dev.off()
    
    Tcorr[,itr]  = Tcorr1;
  }
  
  # TEST 3 ------------------------------------------------------------------
  # Internal Cosistency -----------------------------------------------------
  # Terdiri dari tiga tahapan Tmax<Tmin, Tmax<Tave dan Tave<Tmin ------------
  # for (itr in 1:dim(T)[2]) {
    # itr <- 1 ####
    # Adakah Tmax<Tmin ???
    ic1 <- Tcorr[,2]<Tcorr[,3];
    if (sum(ic1, na.rm = T)>0 && !is.na(sum(ic1, na.rm = T))) {
      idxic1 <- which(ic1==TRUE)
      Tcorr[idxic1, c(2, 3)] <- NA
      all_date_test3 <- cbind.fill(all_date_test3,vecDates[idxic1])
      png(filename = paste0('graphic/',namefile,'/Internal_Consistency_Test_TMX_TMN_',namefile,'.png'),
          res = 200, width = 1600, height = 1200)
      par(mar = c(1.5, 1.4, 1.8, 1))
      plot(x = 1:length(vecDates), y = Tcorr[,3], type = 'l', col = 'blue',
           lwd = 1.5, ylim = c(15,40), xaxt='n', yaxt='n', xlab = '', ylab='',
           panel.first = {
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = NA, tcl = 0.3, tick = T, cex.axis=0.6, 
                  las=1, line=0, lwd=1.6);
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = getvecDates, tcl = 0.3, tick = F, cex.axis=0.6, 
                  las=1, line=-0.8, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = NA, tick = T, 
                  tcl = 0.3, cex.axis=0.6,las=1, line=0, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = seq(0,45,5), tick = F,
                  tcl = 1, cex.axis=0.6,las=1, line=-0.8, lwd=1.6);
             abline(h = seq(0,45,5), lty = 1, lwd=1.3, col='gray')
             abline(v = idgetvecDate[seq(1,length(idgetvecDate),6)], lty = 1,
                    lwd=1.3, col='gray')
           })
      lines(x = 1:length(vecDates), y = Tcorr[,2], lty = 1, lwd = 1.5, col='red')
      # plot posisi tanggal dengan kejadian Tmax<Tmin
      for (iterx in 1:length(idxic1)) {
        # iterx <- 1 ###
        abline(v = idxic1[iterx], lwd=1.5, col='magenta', lty=1)
        text(x = idxic1[iterx], y = max(15:40)-1*iterx, labels = vecDates[idxic1[iterx]],
             adj = 0, cex=0.6, xpd=NA)
      }
      title(main = paste0('TMX vs TMN: ',namefile), line = 0.2, cex.main=1.6)
      dev.off()
    } else {
      all_date_test3 <- cbind.fill(all_date_test3,NA)
    }
    
    # Adakah Tmax<Tave ??? ----------------------------------------------------
    ic2 = Tcorr[,2]<Tcorr[,1];
    if (sum(ic2, na.rm = T)>0 && !is.na(sum(ic2, na.rm = T))) {
      idxic2 <- which(ic2==TRUE)
      Tcorr[idxic2, c(1, 2)] <- NA
      all_date_test3 <- cbind.fill(all_date_test3,vecDates[idxic2])
      png(filename = paste0('graphic/',namefile,'/Internal_Consistency_Test_TMX_TAV_',namefile,'.png'),
          res = 200, width = 1600, height = 1200)
      par(mar = c(1.5, 1.4, 1.8, 1))
      plot(x = 1:length(vecDates), y = Tcorr[,2], type = 'l', col = 'red',
           lwd = 1.5, ylim = c(20,40), xaxt='n', yaxt='n', xlab = '', ylab='',
           panel.first = {
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = NA, tcl = 0.3, tick = T, cex.axis=0.6, 
                  las=1, line=0, lwd=1.6);
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = getvecDates, tcl = 0.3, tick = F, cex.axis=0.6, 
                  las=1, line=-0.8, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = NA, tick = T, 
                  tcl = 0.3, cex.axis=0.6,las=1, line=0, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = seq(0,45,5), tick = F,
                  tcl = 1, cex.axis=0.6,las=1, line=-0.8, lwd=1.6);
             abline(h = seq(0,45,5), lty = 1, lwd=1.3, col='gray')
             abline(v = idgetvecDate[seq(1,length(idgetvecDate),6)], lty = 1,
                    lwd=1.3, col='gray')
           })
      lines(x = 1:length(vecDates), y = Tcorr[,1], lty = 1, lwd = 1.5, col='green')
      # plot posisi tanggal dengan kejadian Tmax<Tave
      for (iterx in 1:length(idxic2)) {
        # iterx <- 1 ###
        abline(v = idxic2[iterx], lwd=1.5, col='magenta', lty=1)
        text(x = idxic2[iterx], y = max(20:40)-0.6*iterx, labels = vecDates[idxic2[iterx]],
             adj = 0, cex=0.6, xpd=NA)
      }
      title(main = paste0('TMX vs TAV: ',namefile), line = 0.2, cex.main=1.6)
      legend('topright', legend = c('Tmax','Tave'), col = c('red','green'), lty = c(1,1),
             lwd = c(1.5,1.5), cex = c(0.8,0.8))
      dev.off()
    } else {
      all_date_test3 <- cbind.fill(all_date_test3,NA)
    }
    
    # Adakah Tave<Tmin ??? ----------------------------------------------------
    ic3 = Tcorr[,1]<Tcorr[,3];
    if (sum(ic3, na.rm = T)>0 && !is.na(sum(ic3, na.rm = T))) {
      idxic3 <- which(ic3==TRUE)
      Tcorr[idxic3, c(1, 2)] <- NA
      all_date_test3 <- cbind.fill(all_date_test3,vecDates[idxic3])
      png(filename = paste0('graphic/',namefile,'/Internal_Consistency_Test_TAV_TMN_',namefile,'.png'),
          res = 200, width = 1600, height = 1200)
      par(mar = c(1.5, 1.4, 1.8, 1.3))
      plot(x = 1:length(vecDates), y = Tcorr[,1], type = 'l', col = 'green',
           lwd = 1.5, ylim = c(15,35), xaxt='n', yaxt='n', xlab = '', ylab='',
           panel.first = {
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = NA, tcl = 0.3, tick = T, cex.axis=0.6, 
                  las=1, line=0, lwd=1.6);
             axis(side = 1, at = idgetvecDate[seq(1,length(idgetvecDate),6)], 
                  labels = getvecDates, tcl = 0.3, tick = F, cex.axis=0.6, 
                  las=1, line=-0.8, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = NA, tick = T, 
                  tcl = 0.3, cex.axis=0.6,las=1, line=0, lwd=1.6);
             axis(side = 2, at = seq(0,45,5),labels = seq(0,45,5), tick = F,
                  tcl = 1, cex.axis=0.6,las=1, line=-0.8, lwd=1.6);
             abline(h = seq(0,45,5), lty = 1, lwd=1.3, col='gray')
             abline(v = idgetvecDate[seq(1,length(idgetvecDate),6)], lty = 1,
                    lwd=1.3, col='gray')
           })
      lines(x = 1:length(vecDates), y = Tcorr[,3], lty = 1, lwd = 1.5, col='blue')
      # plot posisi tanggal dengan kejadian Tmax<Tave
      for (iterx in 1:length(idxic3)) {
        # iterx <- 1 ###
        abline(v = idxic3[iterx], lwd=1.5, col='magenta', lty=1)
        text(x = idxic3[iterx], y = max(15:35)-0.6*iterx, labels = vecDates[idxic3[iterx]],
             adj = 0, cex=0.6, xpd=NA)
      }
      title(main = paste0('TAV vs TMN: ',namefile), line = 0.2, cex.main=1.6)
      legend('topright', legend = c('Tave','Tmin'), col = c('green','blue'), lty = c(1,1),
             lwd = c(1.5,1.5), cex = c(0.8,0.8))
      dev.off()
    } else {
      all_date_test3 <- cbind.fill(all_date_test3,NA)
    }
  # }
  
  # TEST 4 ------------------------------------------------------------------
  # Temporal coherency ------------------------------------------------------
  # -------------------------------------------------------------------------
  tc <- diff(Tcorr)
  for (iter2 in 1:dim(Tcorr)[2]) {
    # iter2 <- 1 #####
    idxtc <- which(abs(tc[,iter2])>10)
    all_date_test4 <- cbind.fill(all_date_test4,vecDates[idxtc]);
    if (!identical(idxtc, integer(0))) {
      Tcorr[idxtc,iter2] <- NA
    }
  }
  ####
  countTest1a <- colSums(!is.na(all_date_test1a))  # jumlah tgl dg Aberant Value (T>45 atau T<5)
  countTest1b <- colSums(!is.na(all_date_test1b))
  countTest1b <- ifelse(identical(countTest1b,numeric(0)),0,countTest1b)
  countTest1c <- colSums(!is.na(all_date_test1c))
  countTest1c <- ifelse(identical(countTest1c,numeric(0)),0,countTest1c)
  countTest2a <- colSums(!is.na(all_date_test2a))
  countTest2b <- colSums(!is.na(all_date_test2b))
  countTest3  <- colSums(!is.na(all_date_test3))
  countTest4  <- colSums(!is.na(all_date_test4))
  ####
  all_date_test1b <- ifelse(identical(all_date_test1b,data.frame(c())),NA,all_date_test1b)
  all_date_test1c <- ifelse(identical(all_date_test1c,data.frame(c())),NA,all_date_test1c)
  ###
  out_QC <- setNames(object = data.frame(vecDates,Tcorr), nm = c('TANGGAL','TAV','TMX','TMN'))
  Count_Error <- data.frame(rbind(cbind(t(colnames(T)),'cekDate','cekTAV',t(colnames(T)),t(colnames(T)),'TMX<TMN','TMX<TAV','TAV<TMN',t(colnames(T))),
        cbind(t(countTest1a),t(countTest1b),t(countTest1c),t(countTest2a),t(countTest2b),t(countTest3),t(countTest4))))
  colnames(Count_Error) <- c(rep('TEST1a',3),rep('TEST1b',1),rep('TEST1c',1),rep('TEST2a',3),rep('TEST2b',3),rep('TEST3',3),
                             rep('TEST4',3))
  Date_Error <- rbind(cbind(t(colnames(T)),'cekDate','cekTAV',t(colnames(T)),t(colnames(T)),'TMX<TMN','TMX<TAV','TAV<TMN',t(colnames(T))),
                cbind.fill(all_date_test1a,all_date_test1b,all_date_test1c,all_date_test2a,all_date_test2b,all_date_test3,all_date_test4))
  colnames(Date_Error) <- c(rep('TEST1a',3),rep('TEST1b',1),rep('TEST1c',1),rep('TEST2a',3),rep('TEST2b',3),rep('TEST3',3),
                             rep('TEST4',3))
  l_out <- list(out_QC,Count_Error,Date_Error); names(l_out) <- c('out_QC','Count_Error','Date_Error')
  openxlsx::write.xlsx(x = l_out, file = paste0('AllTest_QC_output/',namefile,'_QC.xlsx'))
}
  
