library(tidyverse)

#======= Pagu Anggaran ========

#======= Realisasi Anggaran =========
realisasi <- as_tibble(read.table("https://combinatronics.com/zaky31/anggaran/main/realisasi_2022.txt", sep = ";", header = TRUE))
#str(realisasi)

#===== Realisasi per bulan ======
realisasi$bulan <- factor(realisasi$bulan, levels = c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December"))
realisasi_1 <- realisasi %>% drop_na(bulan) %>% group_by(bulan, .drop = FALSE) %>% summarise(realisasi = sum(jml),.groups = 'drop')
realisasi_1 <- add_column(realisasi_1, kumulatif = cumsum(as.numeric(realisasi_1$realisasi)))
#realisasi_1

#======= Rencana Penarikan Dana =========
rpd_2022 <- as_tibble(read.table("https://combinatronics.com/zaky31/rpd/main/rpd_2022_fix.txt", sep = ";", header = TRUE))
#str(rpd_2022)
colnames(rpd_2022)[8:length(colnames(rpd_2022))] <- c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December")

#=== Update tipe data ====
rpd_2022$`01-January` <- as.integer(rpd_2022$`01-January`)
rpd_2022$`02-February` <- as.integer(rpd_2022$`02-February`)
rpd_2022$`03-March` <- as.integer(rpd_2022$`03-March`)
rpd_2022$`04-April` <- as.integer(rpd_2022$`04-April`)
rpd_2022$`05-May` <- as.integer(rpd_2022$`05-May`)
rpd_2022$`06-June` <- as.integer(rpd_2022$`06-June`)
rpd_2022$`07-July` <- as.integer(rpd_2022$`07-July`)
rpd_2022$`08-August` <- as.integer(rpd_2022$`08-August`)
rpd_2022$`09-September` <- as.integer(rpd_2022$`09-September`)
rpd_2022$`10-October` <- as.integer(rpd_2022$`10-October`)
rpd_2022$`11-November` <- as.integer(rpd_2022$`11-November`)
rpd_2022$`12-December` <- as.integer(rpd_2022$`12-December`)

rpd_2022 <- rpd_2022 %>% replace(is.na(.),0)

rpd_2022$ro <- factor(rpd_2022$ro, levels = c("4130.ABG","4130.AFA","4130.CAB","4130.CAN"))
rpd_2022$komponen <- factor(rpd_2022$komponen, levels = c("4130.ABG.001.051", "4130.ABG.001.052", "4130.ABG.001.053", "4130.AFA.001.051", "4130.AFA.001.052", "4130.AFA.001.053", "4130.CAB.001.053", "4130.CAN.001.051"))
rpd_2022$mak <- factor(rpd_2022$mak, levels = c("521111 - Belanja Keperluan Perkantoran","521115 - Belanja Honor Operasional Satuan Kerja","521131 - Belanja Barang Operasional - Penanganan Pandemi COVID-19","521211 - Belanja Bahan","521213 - Belanja Honor Output Kegiatan","521219 - Belanja Barang Non Opeasional Lainnya","521811 - Belanja Barang Persediaan Barang Konsumsi","522131 - Belanja Jasa Konsultan","522141 - Belanja Sewa","522151 - Belanja Jasa Profesi","522191 - Belanja Jasa Lainnya","522192 - Belanja Jasa - Penanganan Pandemi COVID-19","523121 - Belanja Pemeliharaan Peralatan dan Mesin","524111 - Belanja Perjalanan Dinas Biasa","524113 - Belanja Perjalanan Dinas Dalam Kota","524114 - Belanja Perjalanan Dinas Paket Meeting Dalam Kota","524119 - Belanja Perjalanan Dinas Paket Meeting Luar Kota","524219 - Belanja Perjalanan Dinas Lainnya - Luar Negeri","532111 - Belanja Modal Peralatan dan Mesin","532121 - Belanja Penambahan Nilai Peralatan dan Mesin","536111 - Belanja Modal Lainnya"))
rpd_2022$modal <- factor(rpd_2022$modal, levels = c("RM","PNP"))
rpd_2022$week <- factor(rpd_2022$week, levels = c("1","2","3","4"))

#str(rpd_2022)

# === Buat table gather ====
rpd_2022_1 <- gather(rpd_2022,month,jml,`01-January`:`12-December`)
rpd_2022_1$month <- factor(rpd_2022_1$month, levels = c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December"))
#str(rpd_2022_1)

# === menhitung untuk setiap keseluruhan ===
rpd_ro <- rpd_2022_1 %>% drop_na() %>% group_by(ro, kegiatan, mak, modal, month) %>% summarise(rpd = sum(jml), .groups = 'drop')
summary_rpd <- spread(rpd_ro, month, rpd)

# === kompilasi untuk setiap bulan ====
rpd_bulan <- rpd_2022_1 %>% group_by(month) %>% summarise(rpd.bulan = sum(jml),.groups = 'drop')
rpd_bulan <- add_column(rpd_bulan, rpd = cumsum(as.numeric(rpd_bulan$rpd.bulan)))
rpd_bulan <- add_column(rpd_bulan, realisasi = realisasi_1$kumulatif)
rpd_bulan <- add_column(rpd_bulan, `percent(%)` = round(100*(rpd_bulan$rpd-rpd_bulan$realisasi)/sum(rpd_bulan$rpd.bulan), digits = 2))
rpd_bulan$month <- as.character(rpd_bulan$month)
rpd_bulan <- rpd_bulan[c("month","rpd","realisasi","percent(%)")]

# === grafik untuk setiap bulan ====# 
plot.rpd.1 <- ggplot(data = rpd_bulan, aes(x = 1:12, y = rpd)) + 
  geom_line(aes(y = rpd), color = "#2a9d8f") +
  geom_point(aes(y = rpd), color = "#2a9d8f") +
  geom_text(aes(label = rpd)) +
  geom_line(aes(y = realisasi), color = "#e9c46a") +
  geom_point(aes(y = realisasi),color = "#e9c46a") +
  xlim(rpd_bulan$month) +
  xlab("Bulan") + ylab ("Anggaran") +
  theme_bw()


