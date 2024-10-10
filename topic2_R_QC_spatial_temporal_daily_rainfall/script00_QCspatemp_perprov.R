#################################################################################
# Skrip Quality Control (QC) data curah hujan harian
# Tujuan QC untuk mencari outlier secara spatial dan temporal
# inputan: satu file excel (xlsx) terdiri dari dua sheet:
#          Sheet pertama = "INFOPOS"
#          Sheet kedua = "DATA"
# output : satu file excel data hasil QC (outlier diremove dan diganti NaN)
#          satu folder berisi boxplot pos-pos yg mempunyai outlier temporal
#          dan spasial
# Kontak: Robi Muharsyah, robi.muharsyah@gmail.com
# Referensi: Spatiotemporal Characteristics of Extremes Rainfall Events Over
#           Java Island, Indonesia: case east java provinces (Thesis Supari 2012)
# Cara menggunakan skrip:
##### Proses QC spasial dan temporal dilakukan secara terpisah
##### Pertama jalankan QC spasial terlebih dahulu, caranya "matikan" atau
##### beri tanda # didepan tic;process_temp_out_prov;toc (baris 44)
######### Tunggu hingga proses selesai
##### kedua jalankan QC temporal, caranya "matikan" atau
##### beri tanda # didepan tic;process_spa_out_prov;toc (baris 43)
######### Tunggu hingga proses selesai
# Bekasi, 8 Juli 2021
#################################################################################
rm(list = ls())
setwd('D:/BELAJAR/OMAN_TRAINING/topic2_Matlab_QC_spatial_temporal_daily_rainfall/topic2_R_QC_spatial_temporal_daily_rainfall')
diroutput <- paste0(getwd(),'/02_output'); dir.create(diroutput, recursive = T);
dirinput  <- paste0(getwd(),'/01_input'); dir.create(dirinput, recursive = T)
##
# input nama file xlsx (namafile sebaiknya tidak memuat spasi, pisahkan kata
# dengan karakter "_"
namaprovout <- 'DKI_Jakarta'

#################################################################################
source('process_input.R') # bagian ini jangan dihapus/diubah
#################################################################################

############### Bagian berikut boleh diubah sesuai keperluan ####################
stryr       <- 1991
endyr       <- 2021
rad_corr    <- 50  # radius pencarian tetangga (Km)
lowTercile  <- 0  
UpTercile   <- 80
Range       <- 5
replaceqc   <- 1   # 1 = jika nilai outlier di jadikan NaN, selain 1 maka diganti jadi TRS
missval     <- NA  # 9999
Figout      <- 1   # menampilkan gbr boxplot
##
# proses QC
system.time(source('process_spa_out_prov.R'))   ##### comment salah satu bagian ini
system.time(source('process_temp_out_prov.R'))  ##### comment salah satu bagian ini
#################################################################################