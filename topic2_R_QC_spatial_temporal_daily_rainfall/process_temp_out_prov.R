# Bagian QC temporal
##
outdir          <- paste0(diroutput,'/',namaprovout,'/out_temp/');dir.create(outdir,recursive = T);
outdirbplot     <- paste0(outdir,'bplot_temp/'); dir.create(outdirbplot,recursive = T);
outfileqc1      <- paste0(diroutput,'/',namaprovout,'data_pos_',namaprovout,'_QCtemporal.csv');
outfileqc2      <- paste0(diroutput,'/',namaprovout,'info_pos_',namaprovout,'_QCtemporal.csv');
####################################################################################################
############################### SIAPKAN DATA YANG AKAN DIPROSES ####################################
####################################################################################################
# membuang semua pos yg tidak punya koordinat dan memilih periode tahun
# yang seragam untuk semua pos
notsta          <- which(is.na(info1[,1])); info1 <- info1[!(1:nrow(info1)) %in% notsta,] # hilangkan baris NaN di LON LAT
noinfo2         <- info2[,1]; noinfo2 <- noinfo2[!(1:length(noinfo2)) %in% notsta] # hilangkan baris NaN di no pos
namainfo2       <- info2[,2]; namainfo2 <- namainfo2[!(1:length(namainfo2)) %in% notsta] # hilangkan baris NaN di nama pos
vectgl          <- data[,1:3]
chday           <- data[,4:ncol(data)] 
chday           <- chday[,!(1:ncol(chday)) %in% notsta]; chday[is.na(chday)] <- NA
idxyr           <- which(vectgl[,1] >= stryr & vectgl[,1] <= endyr)
# memisahkan vektor tanggal dan data saja
vectglget       <- vectgl[idxyr,]
vectglgetnum    <- as.Date(paste(sprintf(fmt = '%02d',vectglget[,3]),month.abb[vectglget[,2]],
                                 sprintf(fmt = '%04d',vectglget[,1]),sep = '-'), format = '%d-%b-%Y') # buat tgl
chdayget        <- chday[idxyr,]
##
############################################## TAHAP 1 #############################################
####################################### QC temporal bulanan ########################################
####################################################################################################
chdaygetQC <- matrix(data = NA, nrow = dim(chdayget)[1], ncol = dim(chdayget)[2])
tempout_tot <- data.frame(c())
for (iter3 in 1:dim(chdayget)[2]) {
  # iter3 <- 1 ###
  posinfo <- namainfo2[iter3]; # pos terpilih
  noposinfo <- noinfo2[iter3]; # pos terpilih
  print(paste0('Process Temporal QC for : ', posinfo))
  chdaygetpos_curr <- cbind(vectglget,'CH'=chdayget[,iter3]) 
  vectglyr <- unique(chdaygetpos_curr[,1])  
  ###################### QC PERBULAN ####################################
  chmon_noQC <- matrix(data = NA, nrow = 100000,ncol = 12)
  chmon_QC <- matrix(data = NA, nrow = 100000,ncol = 12)
  outli_chdaymon <- data.frame(c())
  for (itmon in 1:12) {
    # itmon <- 1 ####
    # print(itmon)
    idxmon <- which(chdaygetpos_curr[,2]==itmon)
    chmongetpos <- chdaygetpos_curr[idxmon,]
    chmongetposQC1 <- chmongetpos
    chmongetposQC2 <- chmongetpos
    
    chmongetposN <- chmongetpos[,ncol(chmongetpos)]
    chmongetposN[chmongetposN==0] <- NA # NONZERO condition
    Q1 <- quantile(x = chmongetposN, probs = lowTercile/100, na.rm=T,type=5) # hitung Q1
    Q3 <- quantile(x = chmongetposN, probs = UpTercile/100, na.rm=T,type=5) # hitung Q3
    TRS <- Q3+Range*(Q3-Q1) # tresshold outlier
    chmon_noQC[1:dim(chmongetpos)[1],itmon] <- chmongetposN
    #########################
    outli_chday1 <- data.frame(c())
    idx_out <- which(chmongetpos[,ncol(chmongetpos)]>TRS)
    idx_in <- which(chmongetpos[,ncol(chmongetpos)]<=TRS)
    cekjmlout <- length(idx_out)
    # data oulier dibuang karena dipakai untuk buat boxplot
    chmon_QC[1:length(chmongetposN[idx_in]),itmon] <- chmongetposN[idx_in]
    if (cekjmlout!=0) {
      matq <- t(matrix(c(Q1,Q3,TRS)))
      # matq <- data.frame('Q1'=Q1,'Q3'=Q3,'TRS'=TRS)
      outli_chday1 = as.data.frame(cbind.fill(outli_chday1,cbind(chmongetpos[idx_out,],
                      matrix(rep(matq,length(idx_out)),byrow=T, ncol=ncol(matq)))))
      colnames(outli_chday1)[5:ncol(outli_chday1)] <- c('Q1','Q3','TRS')
      if (replaceqc==1) {
        chmongetposQC1[idx_out,ncol(chmongetposQC1)] <- NA # jadikan yg outlier sbg NA
      } else {
        chmongetposQC1[idx_out,ncol(chmongetposQC1)] <- TRS # jadikan yg outlier sbg NA
      }
    }
    outli_chdaymon <- rbind.fill(outli_chdaymon,outli_chday1) # kumpulan semua tanggal yg outlier dista yg di cek
  }
  outli_chdaymons <- cbind(outli_chdaymon,'a'=t(t(rep('M',dim(outli_chdaymon)[1]))))
  ######################## gambar boxplot ############################################
  idx_nan = which(colSums(is.na(chmon_QC))==100000)
  if (length(idx_nan)==0 & Figout==1) {
    png(filename = paste0(outdirbplot,'temp_out_permon_',noposinfo,'.png'), width = 1200, height = 800, res = 200)
    par(mar = c(3, 2, 1.7, 1), oma = c(0, 0, 2, 0))
    bp <- boxplot(chmon_QC, whisker = 0, col = "white", medlty=0, outline = FALSE, xaxt = 'n',yaxt = 'n',
                  lwd=1.5,range=100, ylim=c(0,max(max(chmon_QC,na.rm = T),max(outli_chdaymon[,4:7],na.rm = T),na.rm = T)))
    axis(side = 2, at = seq(0, max(max(chmon_QC,na.rm = T),max(outli_chdaymon[,4:7],na.rm = T),na.rm = T), 10),
    labels = seq(0, max(max(chmon_QC,na.rm = T),max(outli_chdaymon[,4:7],na.rm = T),na.rm = T),10),
         tick = F,tcl = 1,cex.axis=0.6,las=1, line=-0.8, lwd=1.6)
    segments(x0 = seq(0, max(max(chmon_QC,na.rm = T),max(outli_chdaymon[,4:7],na.rm = T),na.rm = T), 0.5), y0 = 0,
          y1 = max(max(chmon_QC,na.rm = T),max(outli_chdaymon[,4:7],na.rm = T),na.rm = T), lty = 2, lwd = 0.5, 
             col = "lightgrey")
    segments(x0 = 0, x1 = max(chmon_QC,na.rm = T),y0 = seq(0, max(max(chmon_QC,na.rm = T),max(outli_chdaymon[,4:7],na.rm = T),na.rm = T), 10), 
             lty = 2,lwd = 0.5, col = "lightgrey")
    if (length(outli_chdaymon)!=0) {
      points(outli_chdaymon[,2], outli_chdaymon[,4], pch = 21, col = rgb(0, 0, 0), cex = 0.6, lwd = 1.5)
      points(outli_chdaymon[,2], outli_chdaymon[,7], pch = 4, col = rgb(255 / 255, 201 / 255, 14 / 255), cex = 0.6, lwd = 1.5)
    }
    text(cex=.8, x=1:ncol(chmon_QC), y=par("usr")[3] - 7.5, c('J','F','M','A','M','J','J','A','S','O','N','D'), 
         xpd=TRUE, srt=0)
    mtext("milimeter", side = 2, line = 1, cex = 0.8)
    title(main = paste0('Temporal Outlier Monthly pos hujan ', posinfo, '\n', noposinfo), cex.main = 0.9, line=0.1)
    legend(x="topright",legend=c("Outlier","Tresholds"),col=c(rgb(63 / 255, 7 / 255, 204 / 255),rgb(255 / 255, 201 / 255, 14 / 255)),
           lwd=2, lty=c(0,0),pch=c(21,4), bty='n', cex=0.6,text.font = 2,bg="transparent")
    dev.off()
  }
  ####################################### QC pertahun ###############################################
  chyr_noQC <- matrix(NA,nrow = 100000, ncol = length(vectglyr))
  chyr_QC <- matrix(NA,nrow = 100000, ncol = length(vectglyr))
  outli_chdayyr <- data.frame(c())
  for (ityr in 1:length(vectglyr)) {
    # ityr <- 23 #####
    idxyr <- which(chdaygetpos_curr[,1]==vectglyr[ityr]);
    chyrgetpos <- chdaygetpos_curr[idxyr,]
    chyrgetposQC1 <- chyrgetpos
    
    chyrgetposN <- chyrgetpos[,ncol(chyrgetpos)]
    chyrgetposN[chyrgetposN==0] <- NA # NONZERO condition
    Q1 <- quantile(x = chyrgetposN, probs = lowTercile/100, na.rm=T,type=5) # hitung Q1
    Q3 <- quantile(x = chyrgetposN, probs = UpTercile/100, na.rm=T,type=5) # hitung Q3
    TRS <- Q3 + Range * (Q3 - Q1) # tresshold outlier
    #####################
    
    outli_chday2 <- NULL
    idx_out <- which(chyrgetpos[,ncol(chyrgetpos)] > TRS)
    idx_in <- which(chyrgetpos[, ncol(chyrgetpos)] <= TRS)
    cekjmlout <- length(idx_out)  
    # data oulier dibuang karena dipakai untuk buat boxplot  
    if (length(chyrgetposN[idx_in])==0) {
      chyrgetposNz <- NA
    } else {
      chyrgetposNz <- chyrgetposN[idx_in]
    }
    chyr_QC[1:length(chyrgetposN[idx_in]), ityr] <- chyrgetposNz
    if (cekjmlout != 0) {
      # matq <- t(matrix(c(Q1,Q3,TRS)))
      # outli_chday1 = as.data.frame(cbind.fill(outli_chday1,cbind(chmongetpos[idx_out,],
                      # matrix(rep(matq,length(idx_out)),byrow=T, ncol=ncol(matq)))))
      # colnames(outli_chday1)[5:ncol(outli_chday1)] <- c('Q1','Q3','TRS')
      outli_chday2 <- cbind(chyrgetpos[idx_out, ], matrix(rep(c(Q1, Q3, TRS), length(idx_out)), ncol = 3, byrow = TRUE))
      colnames(outli_chday2)[5:ncol(outli_chday2)] <- c('Q1','Q3','TRS')
      if (replaceqc == 1) {
        chyrgetposQC1[idx_out, ncol(chyrgetpos)] <- NA # Replace outliers with NA
      } else {
        chyrgetposQC1[idx_out, ncol(chyrgetpos)] <- TRS # Replace outliers with TRS
      }
    }
    outli_chdayyr <- rbind(outli_chdayyr, outli_chday2) # Accumulate all outlier dates
    chyr_noQC[1:dim(chyrgetpos)[1], ityr] <- chyrgetposN
  }
  outli_chdayyrs <- cbind(outli_chdayyr, 'a'=matrix(rep("Y", dim(outli_chdayyr)[1]), ncol = 1))
  if (nrow(outli_chdayyr)!=0) {
    ###################### gambar box #########################
    idx_nan <- which(colSums(is.na(chyr_QC))==100000)
    if (length(idx_nan)!=0) {
      chyr_QC[nrow(chyr_QC),idx_nan] <- 0
    }
    if (Figout==1) {
      png(filename = paste0(outdirbplot,'temp_out_peryear_',noposinfo,'.png'), width = 1200, height = 800, res = 200)
      par(mar = c(3, 2, 1.7, 1), oma = c(0, 0, 2, 0))
      bp <- boxplot(chyr_QC, whisker = 0, col = "white", medlty=0, outline = FALSE, xaxt = 'n',yaxt = 'n',
                    lwd=1.5,range=100, ylim=c(0,max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T)))
      axis(side = 2, at = seq(0, max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T), 10),
           labels = seq(0, max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T),10),
           tick = F,tcl = 1,cex.axis=0.4,las=1, line=-0.8, lwd=1.6)
      segments(x0 = seq(0, max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T), 0.5), y0 = 0,
               y1 = max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T), lty = 2, lwd = 0.5, 
               col = "lightgrey")
      segments(x0 = 0, x1 = max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T),
               y0 = seq(0, max(max(chyr_QC,na.rm = T),max(outli_chdayyr[,4:7],na.rm = T),na.rm = T), 10), 
               lty = 2,lwd = 0.5, col = "lightgrey")
      if (nrow(outli_chdayyr)!=0) {
        points(outli_chdayyr[,1]-(stryr-1),outli_chdayyr[,4], pch = 21, col = rgb(0, 0, 0), cex = 0.6, lwd = 1.5)
        points(outli_chdayyr[,1]-(stryr-1),outli_chdayyr[,7], pch = 4, col = rgb(255 / 255, 201 / 255, 14 / 255), cex = 0.6, lwd = 1.5)
      }
      text(cex=.4, x=(1:length(vectglyr))-0.5, y=par("usr")[3]-9.3, labels = vectglyr,xpd=TRUE, srt=45)
      mtext("milimeter", side = 2, line = 1, cex = 0.5)
      title(main = paste0('Temporal Outlier Annual pos hujan ', posinfo, '\n', noposinfo), cex.main = 0.7, line=0.1)
      legend(x="topright",legend=c("Outlier","Tresholds"),col=c(rgb(63 / 255, 7 / 255, 204 / 255),rgb(255 / 255, 201 / 255, 14 / 255)),
             lwd=2, lty=c(0,0),pch=c(21,4), bty='n', cex=0.6,text.font = 2,bg="transparent")
      dev.off()
    }
  }
  # jk ditemukan outlier pada kasus bulanan/tahunan
  if (nrow(outli_chdaymon)!=0 & nrow(outli_chdayyr)!=0) {
    # gabungkan semua outlier yg ditemukan baik pada bulanan/tahunan
    outli <- rbind(outli_chdaymon,outli_chdayyr);
    tglnum_outli <- as.Date(paste(sprintf('%02d',outli[,3]),sprintf('%02d',outli[,2]),outli[,1],sep=''),format='%d%m%Y')
    # tglnum_outli <- format(as.Date(paste(sprintf('%02d',outli[,3]),sprintf('%02d',outli[,2]),outli[,1],sep=''),format='%d%m%Y'),
    #                        format = '%d-%b-%Y')
    outlis <- rbind(outli_chdaymons,outli_chdayyrs) # ini yg untuk di print
    outli_sort <- outli[order(tglnum_outli),]
    outlis_sort <- outlis[order(tglnum_outli), ]
    idxunik <- which(!duplicated(outli_sort[, 1:3]))
    if (length(idxunik)>0) {
      duplicate_ind <- setdiff(1:nrow(outli_sort), idxunik)
      outli_end <- outli_sort[duplicate_ind,]
      colnames(outli_end) <- c('Year','Month','Day','outlier','Q1','Q3','TRS')
      write.csv(x = outli_end, file = paste0(outdir,namaprovout,'_TO_',noposinfo,'.csv'),row.names = F)
      idx_rm2 <- which(paste0(vectglget[,1],sprintf('%02d',vectglget[,2]),sprintf('%02d',vectglget[,3])) %in% 
              paste0(outli_end[,1],sprintf('%02d',outli_end[,2]),sprintf('%02d',outli_end[,3])))
      chdaygetpos_currQC <- chdaygetpos_curr[,ncol(chdaygetpos_curr)]
      if (replaceqc==1) {
        chdaygetpos_currQC[idx_rm2] <- NA
      } else {
        chdaygetpos_currQC[idx_rm2] = outli_end[,ncol(outli_end)]
      }
      chdaygetQC[,iter3] <- chdaygetpos_currQC
    }
    tempout_sama <- dim(outli_end)[1]
  } else {
    chdaygetQC[,iter3] <- chdaygetpos_curr[,ncol(chdaygetpos_curr)] 
    tempout_sama <- 0
  }
  tempout_tot <- rbind(tempout_tot,tempout_sama)
}
##
# simpan data hasil QC temporal, sesuai pilihan apakah diganti NaN atau TRS
chdaygetQCout <- cbind(vectglget,chdaygetQC)
colnames(chdaygetQCout) <- ColT
write.csv(x = chdaygetQCout, file = outfileqc1, row.names = F)
write.csv(x = infoposprov, file = outfileqc2, row.names = F)
##
# simpan info jumlah outlier yg ditemukan pada QC temporal
getspatemp <- cbind(infoposprov,'Suspect_Temp'=tempout_tot[[1]])
write.csv(x = getspatemp, file = paste0(outdir,namaprovout,'_Rekap_outlier_QCtemp.csv'), row.names = F)
