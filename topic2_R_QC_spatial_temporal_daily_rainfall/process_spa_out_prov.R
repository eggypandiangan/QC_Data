# Bagian QC spasial
##
outdir          <- paste0(diroutput,'/',namaprovout,'/out_spa/'); dir.create(outdir, recursive = T);
outdirbplot     <- paste0(diroutput,'/',namaprovout,'/bplot_spa/'); dir.create(outdirbplot, recursive = T);
outfileqc1      <- paste0(diroutput,'/',namaprovout,'data_pos_',namaprovout,'_QCspasial.csv');
outfileqc2      <- paste0(diroutput,'/',namaprovout,'info_pos_',namaprovout,'_QCspasial.csv');
outfileqc3      <- paste0(outdir,'corr_dist_',namaprovout,'.png')
############################################################################################################
################################# SIAPKAN DATA YANG AKAN DIPROSES ##########################################
############################################################################################################
# membuang semua pos yg tidak punya koordinat dan memilih periode tahunyang seragam untuk semua pos
notsta          <- which(is.na(info1[,1])); info1 <- info1[!(1:nrow(info1)) %in% notsta,] # hilangkan baris NaN di LON LAT
noinfo2         <- info2[,1]; noinfo2 <- noinfo2[!(1:length(noinfo2)) %in% notsta] # hilangkan baris NaN di no pos
namainfo2       <- info2[,2]; namainfo2 <- namainfo2[!(1:length(namainfo2)) %in% notsta] # hilangkan baris NaN di nama pos
vectgl          <- data[,1:3]
# vectgl        <- as.Date(paste(sprintf(fmt = '%02d',data[,3]),month.abb[data[,2]],
#                         sprintf(fmt = '%04d',data[,1]),sep = '-'), format = '%d-%b-%Y') # buat tgl
chday           <- data[,4:ncol(data)] # pisahkan matrix data saja daeri tanggal
chday           <- chday[,!(1:ncol(chday)) %in% notsta]; chday[is.na(chday)] <- NA
idxyr           <- which(vectgl[,1] >= stryr & vectgl[,1] <= endyr)
# memisahkan vektor tanggal dan data saja
vectglget       <- vectgl[idxyr,]
vectglgetnum    <- as.Date(paste(sprintf(fmt = '%02d',vectglget[,3]),month.abb[vectglget[,2]],
                   sprintf(fmt = '%04d',vectglget[,1]),sep = '-'), format = '%d-%b-%Y') # buat tgl
chdayget        <- chday[idxyr,]
##
########################### TAHAP I : Identifikasi korelasi CH antara pos #################################
###########################################################################################################
############################## BAGIAN INI MENGHITUNG KORELASI ANTAR POS ###################################
###########################################################################################################
# hitung jarak antar sesama pos (dalam Km)
dist <- as.matrix(dist(info1[,1:2])*111)
Ldistn <- dist[lower.tri(dist, diag = F)]
Rmat <- cor(chdayget, use = 'pairwise.complete.obs', method = 'pearson')
LRmatn <- Rmat[lower.tri(Rmat, diag = F)]
##
################# TAHAP II : Menentukan outlier berdasarkan CH di pos sekitarnya ##########################
###########################################################################################################
################################ BAGIAN INI MENENTUKAN SPATIAL OUTLIER ####################################
###########################################################################################################
getneigh <- matrix(data = NA, nrow = dim(Rmat)[2]+2, ncol = dim(Rmat)[2])
# getneigh <- data.frame(c())
jmlout <- data.frame(c())
chdaygetQC <- matrix(data = NA, nrow = dim(chdayget)[1], ncol = dim(chdayget)[2])
for (iter3 in 1:dim(Rmat)[1]) {
  # iter3 <- 2 #####
  posinfo <- namainfo2[iter3] # pos terpilih
  noposinfo <- noinfo2[iter3]
  print(paste0('Process Spatial QC for : ', posinfo))
  chdaygetpos_curr <- chdayget[,iter3] 
  idxdist <- which(dist[,iter3]<=rad_corr) # index lokasi stasiun terpilih
  # dilakukan hanya untuk pos yang punya tetangga pada radius rad_corr Km
  if (!length(idxdist)==0){
    posinfo_co       <- namainfo2[idxdist]  # semua pos hujan terpilih,termasuk pos yg sdg dicek
    posinfo_co_neigh <- posinfo_co[which(idxdist!=iter3)] # pos tetangga
    neigh            <- c(noposinfo,posinfo,length(posinfo_co_neigh),posinfo_co_neigh) # simpan informasi pos tetangga
    getneigh[,iter3] <- neigh # kumpulkan semua informasi pos terpilih
    # getneigh         <- cbind.fill(getneigh,neigh) # kumpulkan semua informasi pos terpilih
    
    chdaygetpos      <- chdayget[,idxdist] # semua CH pos hujan dalam radius rad_corr
    chdaygetposX     <- chdaygetpos  # pos terpilih ikut dlm penghitungan TRS
    chdaygetposX[chdaygetposX==0] <- NA # jadikan NONZERO condition
    chdaygetposZ     <- chdaygetpos[,which(idxdist!=iter3)] # pos terpilih tdk ikut dlm penghitungan TRS
    
    # tentukan batasan utk outlier
    Q1               <- apply(X = chdaygetposX, MARGIN = 1, 
                        FUN = function(z){quantile(x = z, probs = lowTercile/100, na.rm=T,type=5)}) # hitung Q1
    Q3               <- apply(X = chdaygetposX, MARGIN = 1, 
                        FUN = function(z){quantile(x = z, probs = UpTercile/100, na.rm=T,type=5)}) # hitung Q1
    TRS              <- Q3+Range*(Q3-Q1) # tresshold outlier
    chdaygetposY     <- data.frame(vectglget,chdaygetpos_curr,Q1,Q3,TRS)
    
    # mencari hari yg outlier berdasarkan TRS
    # chdaygetpos_curr1 <- data.frame(chdaygetpos_curr)
    # TRS1 <- data.frame(TRS)
    idx_outli <- which(chdaygetpos_curr>TRS)
    # idx_outli <- c(29,45,66) #####
    cekjmlout <- length(idx_outli); jmlout <- cbind.fill(jmlout,cekjmlout)
    if (cekjmlout!=0) {
      outli_chday  <- chdaygetposX[idx_outli,] # baris/tgl outlier
      outli_chday_neigh <- chdaygetposZ[idx_outli,]
      outli_val <- chdaygetposY[idx_outli,]
      outli_Tgl <- format(as.Date(paste(sprintf(fmt = '%04d',chdaygetposY[idx_outli,1]),sprintf(fmt = '%02d',chdaygetposY[idx_outli,2]),
                                   sprintf(fmt = '%02d',chdaygetposY[idx_outli,3]), sep = '-')),format = '%Y%m%d')
      get_chcurr <- chdaygetpos_curr[idx_outli]
      idx_cek <- rowSums((outli_chday-(t(t(get_chcurr))[, rep(1:ncol(t(t(get_chcurr))), each = dim(outli_chday)[2])]))>0,
                         na.rm = T)
      idx_cek_val <- data.frame(c())
      if (sum(idx_cek)!=0) {
        for (itpos1 in 1:dim(outli_chday)[1]) {
          # itpos1 <- 1 ###
          idx1 <- which(outli_chday[itpos1,]>outli_val[itpos1,4])
          idx_cek_vals <- outli_chday[itpos1,idx1]
          idx_cek_vals <- ifelse(prod(dim(idx_cek_vals))==0,NA,idx_cek_vals)
          idx_cek_val <- cbind.fill(idx_cek_val,idx_cek_vals)
        }
      }
      outli_val  <- cbind.fill(outli_val,idx_cek)
      
      #######################################################################################    
      #### plot BOXPLOT tapi hanya untuk kejadian hari dengan outlier
      if (Figout==1) {
        png(filename = paste0(outdirbplot,"spa_out_", noposinfo, ".png"), width = 1200, height = 800, res = 200)
        # png(filename = paste0(outfileqc3), width = 1200, height = 800, res = 200)
        # png(filename = paste0(outdir, "spa_out_", noposinfo, ".png"), width = 1200, height = 800, res = 200)
        par(mar = c(3, 2, 1.7, 1), oma = c(0, 0, 2, 0))
        bp <- boxplot(t(outli_chday_neigh), whisker = 0, col = "white", medlty=0, outline = FALSE, xaxt = 'n',yaxt = 'n',
                lwd=1.5,range=100, ylim=c(0,max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T)))
        axis(side = 2, at = seq(0, max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T), 10),
             labels = seq(0, max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T),10), tick = F,
             tcl = 1,cex.axis=0.6,las=1, line=-0.8, lwd=1.6)
        segments(x0 = seq(0, max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T), 0.5), y0 = 0, 
                 y1 = max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T), lty = 2, lwd = 0.5, 
                 col = "lightgrey")
        segments(x0 = 0, x1 = max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T), 
                 y0 = seq(0, max(max(outli_chday_neigh,na.rm = T),max(outli_val[,c(4:7)],na.rm = T),na.rm = T), 10), lty = 2, 
                 lwd = 0.5, col = "lightgrey")
        points(1:length(get_chcurr), outli_val[, 4], pch = 21, col = rgb(63 / 255, 7 / 255, 204 / 255), cex = 0.6, lwd = 1.5)
        points(1:length(get_chcurr), outli_val[, 7], pch = 4, col = rgb(255 / 255, 201 / 255, 14 / 255), cex = 0.6, lwd = 1.5)
        if (sum(idx_cek) != 0) {
          # for (itpos2 in 1:ncol(idx_cek_val)) {
            # itpos2 <- 1 ###
            points(1:length(get_chcurr), idx_cek_val, pch = 22, col = "black", cex = 0.6, lwd = 1.5)
            # points(1:length(get_chcurr), idx_cek_val[, itpos2], pch = 22, col = "black", cex = 1.5, lwd = 2)
          # }
        }
        if (length(outli_Tgl)>1) {
          posy <- par("usr")[3] - 7
        } else {
          posy <- par("usr")[3] - 1.8
        }
        text(cex=.5, x=(1:length(outli_Tgl))-0.04, y=posy, outli_Tgl, xpd=TRUE, srt=45)
        mtext("milimeter", side = 2, line = 1, cex = 0.8)
        title(main = paste0("Spatial Outlier (", rad_corr, "Km) pos hujan ", posinfo, "\n", noposinfo), cex.main = 1)
        legend(x="topright",legend=c("Outlier","Tresholds"),col=c(rgb(63 / 255, 7 / 255, 204 / 255),rgb(255 / 255, 201 / 255, 14 / 255)),
                lwd=2, lty=c(0,0),pch=c(21,4), bty='n', cex=0.6,text.font = 2,bg="transparent")
        dev.off()
      }
      colnames(outli_val) <- c('Year','Month','Day','outlier','Q1','Q3','TRS','CND')
      write.csv(x = outli_val,file = paste0(outdir,namaprovout,'_SO_',noposinfo,'.csv'), row.names = F)
      # identifikasi tanggal yg menjadi outlier pada pos yg sdg di cek ada 2 pilihan: 
      # (i) semua CH pada tgl tsb di hapus dan dijadikan NaN atau 
      # (ii)pakai trhreshold
      idx_rm2 <- which(paste0(vectglget[,1],sprintf('%02d',vectglget[,2]),sprintf('%02d',vectglget[,3])) %in% 
                      paste0(outli_val[,1],sprintf('%02d',outli_val[,2]),sprintf('%02d',outli_val[,3])))
      chdaygetpos_currQC <- chdaygetpos_curr
      if (replaceqc==1) {
        chdaygetpos_currQC[idx_rm2] <- NA
      } else {
        chdaygetpos_currQC[idx_rm2] <- outli_val[,ncol(outli_val)]; # diisi dg tres
      }
      chdaygetQC[,iter3] <- chdaygetpos_currQC # jika tidak ada outilier yg terdeteksi
    } else {
    chdaygetQC[,iter3]<- chdaygetpos_curr  # jika tidak ada outilier yg terdeteksi
    }
  } else { ### Jika tidak ada tetangga
    cekjmlout <- -9999;
    jmlout <- cbind.fill(jmlout,cekjmlout)
    getneigh[1:2,iter3] <- c('0','NOT CHECK') 
    chdaygetQC[,iter3] <- chdaygetpos_curr;
  }
}
##
# simpan data hasil QC spasial, sesuai pilihan apakah diganti NaN atau TRS
chdaygetQCout <- cbind(vectglget,chdaygetQC)
colnames(chdaygetQCout) <- ColT
write.csv(x = chdaygetQCout,file = outfileqc1, row.names = F)
write.csv(x = infoposprov, file = outfileqc2, row.names = F)
write.csv(x = rbind.fill(as.data.frame(jmlout),as.data.frame(getneigh)), file = paste0(outdir,'collect_neighbor_',namaprovout,'_',rad_corr,'Km.csv'), 
          row.names = F, col.names = F)
write.table(x = rbind.fill(as.data.frame(jmlout),as.data.frame(getneigh)), file = paste0(outdir,'collect_neighbor_',namaprovout,'_',rad_corr,'Km.csv'), 
          row.names = F, col.names = F, sep = ',')
##
# simpan info jumlah outlier yg ditemukan pada QC spasial
getspatemp <- cbind.fill(infoposprov,data.frame('Suspect_Spatial'=t(jmlout)))
write.table(x = getspatemp, file = paste0(outdir,namaprovout,'_REKAP.csv'),row.names = F, sep = ',')