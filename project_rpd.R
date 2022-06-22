require(tidyverse)
require(googlesheets4)

#======= Pagu Anggaran ========
#gs4_deauth()
#pagu <- read_sheet("https://docs.google.com/spreadsheets/d/10PeMfrDTBZ2F_kSB98dAHgAly3vfDLTGCfSxGhCCPIw/edit?usp=sharing", sheet = "Rincian Kertas Kerja")
#pagu <- pagu[which(pagu$jml != 0),]

#pagu$rincian_output <- factor(pagu$rincian_output,levels = c("4130.ABG.001 - Kajian Keamanan, Mutu, Gizi dan Manfaat Pangan Olahan yang Diselesaikan","4130.AFA.001 - Standar Pangan Olahan yang Disusun","4130.CAN.001 - Perangkat Pengolah Data dan Komunikasi","4130.CAB.001 - Sarana Pengawasan Pangan Olahan"))
#levels(pagu$rincian_output) <- c("4130.ABG.001","4130.AFA.001","4130.CAN.001","4130.CAB.001")
#pagu$komponen <- factor(pagu$komponen, levels = c("4130.ABG.001.051 - Kajian dan Evaluasi Standardisasi Keamanan Pangan","4130.AFA.001.051 - Penyusunan Norma Standar Prosedur dan Kriteria (NSPK) di Bidang Pangan","4130.AFA.001.053 - Monitoring dan Evaluasi Kinerja Standardisasi Pangan Olahan","4130.ABG.001.052 - Kajian dan Evaluasi Standardisasi Mutu Pangan Olahan","4130.ABG.001.053 - Kajian dan Evaluasi Standardisasi Pangan Olahan Tertentu","4130.AFA.001.052 - Koordinasi dan Harmonisasi Standar Pangan Olahan","4130.CAN.001.051 - Pengadaan Perangkat Pengolah Data dan Komunikasi","4130.CAB.001.053 - Pengadaan Peralatan Fasilitas Perkantoran"))
#levels(pagu$komponen) <- c("4130.ABG.001.051","4130.AFA.001.051","4130.AFA.001.053","4130.ABG.001.052","4130.ABG.001.053","4130.AFA.001.052","4130.CAN.001.051","4130.CAB.001.053")
'pagu$kegiatan <- factor(pagu$kegiatan, levels = c("051.A - Pelayanan Analisis dan Evaluasi Standar Keamanan Pangan"                                                         
                                                 ,"052.A - Pelayanan Analisis dan Evaluasi Standardisasi Mutu Pangan Olahan"                                                
                                                  ,"053.A - Pelayanan Analisis dan Evaluasi Standardisasi Pangan Olahan Tertentu"                                            
                                                  ,"051.A - Penyusunan dan Revisi Peraturan, Standar, Pedoman dan Code of Practice di Bidang Keamanan Pangan"                
                                                  ,"051.B - Penyusunan dan Revisi Peraturan, Standar, Pedoman dan Code of Practice di Bidang Pangan Olahan Tertentu"         
                                                  ,"051.C - Penyusunan dan Revisi Peraturan, Standar, Pedoman dan Code of Practice di Bidang Mutu Pangan Olahan"             
                                                  ,"051.D - Evaluasi Kualitas Kebijakan / Standar Pangan Olahan"                                                             
                                                  ,"051.E - Evaluasi Pelayanan Publik Direktorat Standardisasi Pangan Olahan"                                                
                                                  ,"051.F - Fasilitasi Standardisasi Keamanan, Mutu, dan Manfaat Pangan Olahan"                                              
                                                  ,"051.G - Automatic Adjusment (Pemblokiran)"                                                                               
                                                  ,"052.A - Partisipasi Aktif pada Sidang Codex dan Pertemuan Internasional"                                                 
                                                  ,"052.B - Persiapan Posisi Indonesia pada Pertemuan Internasional"                                                         
                                                  ,"052.C - Advokasi dan Sosialisasi Standar Pangan"                                                                         
                                                  ,"053.A - Peningkatan Kompetensi Pegawai Direktorat Standardisasi Pangan Olahan"                                           
                                                  ,"053.B - Evaluasi dan Perencanaan Direktorat Standardisasi Pangan Olahan"                                                 
                                                  ,"053.C - Pelaksanaan Operasional Kegiatan Standardisasi Pangan Olahan"                                                    
                                                  ,"053.D - Pengelolaan Data dan Informasi pada Direktorat Standardisasi Pangan Olahan"                                      
                                                  ,"053.E - Monitoring dan Evaluasi Pelaksanaan Anggaran Direktorat Standardisasi Pangan Olahan"                             
                                                  ,"053.F - Forum Koordinasi Teknis Deputi Bidang Pengawasan Pangan Olahan pada Direktorat Standardisasi Pangan Olahan"      
                                                  ,"053.G - Perkuatan Jejaring Lintas Sektor dalam Rangka Standardisasi Pangan Olahan"                                       
                                                  ,"053.A - Pengadaan Peralatan Fasilitas Perkantoran pada Direktorat Pengawasan Produksi Pangan Olahan"                     
                                                  ,"053.B - Pengadaan Peralatan Fasilitas Perkantoran pada Direktorat Pemberdayaan Masyarakat dan Pelaku Usaha Pangan Olahan"
                                                  ,"053.C - Pengadaan Peralatan Fasilitas Perkantoran pada Direktorat Standardisasi Pangan Olahan"                           
                                                  ,"051.A - Pengadaan Perangkat Pengolah Data pada Direktorat Standardisasi Pangan Olahan"                                   
                                                  ,"051.B - Pengadaan Perangkat Pengolah Data pada Direktorat Pengawasan Produksi Pangan Olahan"                             
                                                  ,"051.C - Pengadaan Perangkat Pengolah Data dalam rangka Pelayanan Publik Direktorat Registrasi Pangan Olahan"             
                                                  ,"051.D - Pengadaan Perangkat Pengolah Data pada Direktorat Pengawasan Peredaran Pangan Olahan"                            
                                                  ,"051.E - Pengadaan Perangkat Pengolah Data pada Direktorat Pemberdayaan Masyarakat dan Pelaku Usaha Pangan Olahan")  )
pagu$`kode akun` <- factor(pagu$`kode akun`)
pagu$jenis_modal <- factor(pagu$jenis_modal, levels = c("RM","PNP"))
pagu$jenis_belanja <- factor(pagu$jenis_belanja, levels = c("Belanja Barang","Belanja Modal"))
#str(pagu)'


#======= Realisasi Anggaran =========
#gs4_deauth()
#realisasi <- read_sheet("https://docs.google.com/spreadsheets/d/10PeMfrDTBZ2F_kSB98dAHgAly3vfDLTGCfSxGhCCPIw/edit?usp=sharing", sheet = "INPUT UP TUP")
#realisasi <- realisasi %>% drop_na(jml)

#realisasi %>% print(n = Inf)
#realisasi$bulan <- factor(realisasi$bulan, levels = c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December"))
#realisasi$rincian_output <- factor(realisasi$rincian_output, levels = c("4130.ABG.001 - Kajian Keamanan, Mutu, Gizi dan Manfaat Pangan Olahan yang Diselesaikan","4130.AFA.001 - Standar Pangan Olahan yang Disusun","4130.CAN.001 - Perangkat Pengolah Data dan Komunikasi","4130.CAB.001 - Sarana Pengawasan Pangan Olahan"))
#levels(realisasi$rincian_output) <- c("4130.ABG.001","4130.AFA.001","4130.CAN.001","4130.CAB.001")
#realisasi$komponen <- factor(realisasi$komponen, levels = c("4130.ABG.001.051 - Kajian dan Evaluasi Standardisasi Keamanan Pangan","4130.AFA.001.051 - Penyusunan Norma Standar Prosedur dan Kriteria (NSPK) di Bidang Pangan","4130.AFA.001.053 - Monitoring dan Evaluasi Kinerja Standardisasi Pangan Olahan","4130.ABG.001.052 - Kajian dan Evaluasi Standardisasi Mutu Pangan Olahan","4130.ABG.001.053 - Kajian dan Evaluasi Standardisasi Pangan Olahan Tertentu","4130.AFA.001.052 - Koordinasi dan Harmonisasi Standar Pangan Olahan","4130.CAN.001.051 - Pengadaan Perangkat Pengolah Data dan Komunikasi","4130.CAB.001.053 - Pengadaan Peralatan Fasilitas Perkantoran"))
#levels(realisasi$komponen) <- c("4130.ABG.001.051","4130.AFA.001.051","4130.AFA.001.053","4130.ABG.001.052","4130.ABG.001.053","4130.AFA.001.052","4130.CAN.001.051","4130.CAB.001.053")
'realisasi$kegiatan <- factor(realisasi$kegiatan, levels = c("051.A - Pelayanan Analisis dan Evaluasi Standar Keamanan Pangan"                                                         
                                                            ,"052.A - Pelayanan Analisis dan Evaluasi Standardisasi Mutu Pangan Olahan"                                                
                                                            ,"053.A - Pelayanan Analisis dan Evaluasi Standardisasi Pangan Olahan Tertentu"                                            
                                                            ,"051.A - Penyusunan dan Revisi Peraturan, Standar, Pedoman dan Code of Practice di Bidang Keamanan Pangan"                
                                                            ,"051.B - Penyusunan dan Revisi Peraturan, Standar, Pedoman dan Code of Practice di Bidang Pangan Olahan Tertentu"         
                                                            ,"051.C - Penyusunan dan Revisi Peraturan, Standar, Pedoman dan Code of Practice di Bidang Mutu Pangan Olahan"             
                                                            ,"051.D - Evaluasi Kualitas Kebijakan / Standar Pangan Olahan"                                                             
                                                            ,"051.E - Evaluasi Pelayanan Publik Direktorat Standardisasi Pangan Olahan"                                                
                                                            ,"051.F - Fasilitasi Standardisasi Keamanan, Mutu, dan Manfaat Pangan Olahan"                                              
                                                            ,"051.G - Automatic Adjusment (Pemblokiran)"                                                                               
                                                            ,"052.A - Partisipasi Aktif pada Sidang Codex dan Pertemuan Internasional"                                                 
                                                            ,"052.B - Persiapan Posisi Indonesia pada Pertemuan Internasional"                                                         
                                                            ,"052.C - Advokasi dan Sosialisasi Standar Pangan"                                                                         
                                                            ,"053.A - Peningkatan Kompetensi Pegawai Direktorat Standardisasi Pangan Olahan"                                           
                                                            ,"053.B - Evaluasi dan Perencanaan Direktorat Standardisasi Pangan Olahan"                                                 
                                                            ,"053.C - Pelaksanaan Operasional Kegiatan Standardisasi Pangan Olahan"                                                    
                                                            ,"053.D - Pengelolaan Data dan Informasi pada Direktorat Standardisasi Pangan Olahan"                                      
                                                            ,"053.E - Monitoring dan Evaluasi Pelaksanaan Anggaran Direktorat Standardisasi Pangan Olahan"                             
                                                            ,"053.F - Forum Koordinasi Teknis Deputi Bidang Pengawasan Pangan Olahan pada Direktorat Standardisasi Pangan Olahan"      
                                                            ,"053.G - Perkuatan Jejaring Lintas Sektor dalam Rangka Standardisasi Pangan Olahan"                                       
                                                            ,"053.A - Pengadaan Peralatan Fasilitas Perkantoran pada Direktorat Pengawasan Produksi Pangan Olahan"                     
                                                            ,"053.B - Pengadaan Peralatan Fasilitas Perkantoran pada Direktorat Pemberdayaan Masyarakat dan Pelaku Usaha Pangan Olahan"
                                                            ,"053.C - Pengadaan Peralatan Fasilitas Perkantoran pada Direktorat Standardisasi Pangan Olahan"                           
                                                            ,"051.A - Pengadaan Perangkat Pengolah Data pada Direktorat Standardisasi Pangan Olahan"                                   
                                                            ,"051.B - Pengadaan Perangkat Pengolah Data pada Direktorat Pengawasan Produksi Pangan Olahan"                             
                                                            ,"051.C - Pengadaan Perangkat Pengolah Data dalam rangka Pelayanan Publik Direktorat Registrasi Pangan Olahan"             
                                                            ,"051.D - Pengadaan Perangkat Pengolah Data pada Direktorat Pengawasan Peredaran Pangan Olahan"                            
                                                            ,"051.E - Pengadaan Perangkat Pengolah Data pada Direktorat Pemberdayaan Masyarakat dan Pelaku Usaha Pangan Olahan")  )
realisasi$jenis_modal <- factor(realisasi$jenis_modal, levels = c("RM","PNP"))
realisasi$jenis_belanja <- factor(realisasi$jenis_belanja, levels = c("Belanja Barang","Belanja Modal"))
#str(realisasi)'

#===== Realisasi per bulan ======
#realisasi_1 <- realisasi %>% drop_na(bulan) %>% group_by(bulan, .drop = FALSE) %>% summarise(realisasi = sum(jml),.groups = 'drop')
#realisasi_1 <- add_column(realisasi_1, kumulatif = cumsum(as.numeric(realisasi_1$realisasi)))
#str(realisasi_1)

#===== Realisasi untuk per jenis modal ======
#modal <- left_join(pagu %>% group_by(komponen, jenis_belanja, jenis_modal, .drop = FALSE) %>% 
#                     summarise(pagu = sum(jml), .groups = 'drop') %>%
#                     filter(pagu > 0),
#                   realisasi %>% drop_na(bulan) %>% group_by(komponen,jenis_belanja,jenis_modal, .drop = FALSE) %>% 
#                     #filter(bulan %in% c("01-January")) %>%
#                     summarise(realisasi = sum(jml), .groups = 'drop'),
#          by = c('komponen'='komponen','jenis_belanja' = 'jenis_belanja','jenis_modal' = 'jenis_modal'))
#modal <- function(input){
#  left_join(pagu %>% group_by(komponen, jenis_belanja, jenis_modal, .drop = FALSE) %>% 
#              summarise(pagu = sum(jml), .groups = 'drop') %>%
#              filter(pagu > 0),
#            realisasi %>% drop_na(bulan) %>% group_by(komponen,jenis_belanja,jenis_modal, .drop = FALSE) %>% 
#              filter(bulan %in% input) %>%
#              summarise(realisasi = sum(jml), .groups = 'drop'),
#            by = c('komponen'='komponen','jenis_belanja' = 'jenis_belanja','jenis_modal' = 'jenis_modal'))
#}

#===== Cek SPP =======
#spp <- eventReactive(input$update,{
#  unique(realisasi()$no_spp[!is.na(realisasi$no_spp)])
#},ignoreNULL = FALSE)
  
#realisasi[which(realisasi$no_spp %in% "00728T"),c("no_spp","kegiatan","kode_akun","detail","jml","keterangan")]

#cek_spp <- function(input){
#  realisasi[which(realisasi$no_spp %in% input),c("no_spp","kegiatan","kode_akun","detail","jml","keterangan")]
#}

#===== Realisasi POK =======
#realisasi %>% group_by(rincian_output,kegiatan,kode_akun1) %>% summarise(realisasi = sum(jml), .groups = 'drop')
#pagu %>% group_by(rincian_output,kegiatan,kode_akun1) %>% summarise(pagu = sum(jml), .groups = 'drop')
#realisasi <- eventReactive(input$update,{
#  gs4_deauth()
#  read_sheet("https://docs.google.com/spreadsheets/d/10PeMfrDTBZ2F_kSB98dAHgAly3vfDLTGCfSxGhCCPIw/edit?usp=sharing", sheet = "INPUT UP TUP")
#}, ignoreNULL = FALSE)

#pok_realisasi <- eventReactive(input$update,{
#  left_join(pagu %>% group_by(rincian_output,kegiatan,kode_akun1,detail) %>% summarise(pagu = sum(jml), .groups = 'drop'),
#            realisasi() %>% group_by(rincian_output,kegiatan,kode_akun1,detail) %>% 
#              summarise(realisasi = sum(jml), .groups = 'drop'),
#            by = c('rincian_output','kegiatan','kode_akun1','detail')) %>%
#    mutate(realisasi = replace_na(realisasi,0))
#}, ignoreNULL = FALSE)
#realisasi[which(realisasi$no_spp %in% "00728T"),c("no_spp","kegiatan","kode_akun","detail","jml","keterangan")]
#======= Rencana Penarikan Dana =========
#rpd_2022 <- as_tibble(read.table("https://combinatronics.com/zaky31/rpd/main/rpd_2022_fix.txt", sep = ";", header = TRUE)) %>%
#  rename(`01-January` = X.01.January.,
#         `02-February` = X.02.February.,
#         `03-March` = X.03.March.,
#         `04-April` = X.04.April.,
#         `05-May` = X.05.May.,
#         `06-June` = X.06.June.,
#         `07-July` = X.07.July.,
#         `08-August` = X.08.August.,
#         `09-September` = X.09.September.,
#         `10-October` = X.10.October.,
#         `11-November` = X.11.November.,
#         `12-December` = X.12.December.) %>%
#  replace(is.na(.),0)
#str(rpd_2022)

#rpd_2022$ro <- factor(rpd_2022$ro, levels = c("4130.ABG","4130.AFA","4130.CAB","4130.CAN"))
#rpd_2022$komponen <- factor(rpd_2022$komponen, levels = c("4130.ABG.001.051", "4130.ABG.001.052", "4130.ABG.001.053", "4130.AFA.001.051", "4130.AFA.001.052", "4130.AFA.001.053", "4130.CAB.001.053", "4130.CAN.001.051"))
#rpd_2022$mak <- factor(rpd_2022$mak, levels = c("521111 - Belanja Keperluan Perkantoran","521115 - Belanja Honor Operasional Satuan Kerja","521131 - Belanja Barang Operasional - Penanganan Pandemi COVID-19","521211 - Belanja Bahan","521213 - Belanja Honor Output Kegiatan","521219 - Belanja Barang Non Opeasional Lainnya","521811 - Belanja Barang Persediaan Barang Konsumsi","522131 - Belanja Jasa Konsultan","522141 - Belanja Sewa","522151 - Belanja Jasa Profesi","522191 - Belanja Jasa Lainnya","522192 - Belanja Jasa - Penanganan Pandemi COVID-19","523121 - Belanja Pemeliharaan Peralatan dan Mesin","524111 - Belanja Perjalanan Dinas Biasa","524113 - Belanja Perjalanan Dinas Dalam Kota","524114 - Belanja Perjalanan Dinas Paket Meeting Dalam Kota","524119 - Belanja Perjalanan Dinas Paket Meeting Luar Kota","524219 - Belanja Perjalanan Dinas Lainnya - Luar Negeri","532111 - Belanja Modal Peralatan dan Mesin","532121 - Belanja Penambahan Nilai Peralatan dan Mesin","536111 - Belanja Modal Lainnya"))
#rpd_2022$modal <- factor(rpd_2022$modal, levels = c("RM","PNP"))
#rpd_2022$week <- factor(rpd_2022$week, levels = c("1","2","3","4"))

#str(rpd_2022)

# === Buat table gather ====
#rpd_2022_1 <- gather(rpd_2022,month,jml,`01-January`:`12-December`)
#rpd_2022_1$month <- factor(rpd_2022_1$month, levels = c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December"))
#str(rpd_2022_1)

# === menhitung untuk setiap keseluruhan ===
#rpd_ro <- rpd_2022_1 %>% drop_na() %>% group_by(ro, kegiatan, mak, modal, month) %>% summarise(rpd = sum(jml), .groups = 'drop')
#summary_rpd <- spread(rpd_ro, month, rpd)

# === kompilasi untuk setiap bulan ====
#rpd_bulan <- rpd_2022_1 %>% group_by(month) %>% summarise(rpd.bulan = sum(jml),.groups = 'drop')
#rpd_bulan <- add_column(rpd_bulan, rpd = cumsum(as.numeric(rpd_bulan$rpd.bulan)))
#rpd_bulan <- add_column(rpd_bulan, realisasi = realisasi_1$kumulatif)
#rpd_bulan <- add_column(rpd_bulan, `percent(%)` = round(100*(rpd_bulan$rpd-rpd_bulan$realisasi)/sum(rpd_bulan$rpd.bulan), digits = 2))
#rpd_bulan$month <- as.character(rpd_bulan$month)
#rpd_bulan <- rpd_bulan[c("month","rpd","realisasi","percent(%)")]

#rpd_bulan <- left_join(rpd_2022_1 %>% group_by(month) %>% summarise(rpd.bulan = sum(jml),.groups = 'drop'),
#          realisasi %>% drop_na(bulan) %>% group_by(bulan) %>% summarise(realisasi_1 = sum(jml)),
#          by = c('month'='bulan')) %>%
#  mutate(realisasi_1 = replace_na(realisasi_1,0)) %>%
#  mutate(rpd = cumsum(as.numeric(rpd.bulan)), .after = 'rpd.bulan') %>%
#  mutate(realisasi = cumsum(as.numeric(realisasi_1))) %>%
#  mutate(perc_rpd = 100*round(rpd/sum(rpd.bulan),digits = 6), .after = 'rpd') %>%
#  mutate(perc_real = 100*round(realisasi/sum(rpd.bulan),digits = 6)) %>%
#  select(month,rpd,perc_rpd,realisasi,perc_real) %>%
#  mutate(gap = 100*(rpd - realisasi)/rpd)


# === grafik untuk setiap bulan ====# 
#plot.rpd.1 <- ggplot(data = rpd_bulan, aes(x = 1:12, y = rpd)) + 
#  geom_line(aes(y = rpd), color = "#2a9d8f") +
#  geom_point(aes(y = rpd), color = "#2a9d8f") +
#  geom_text(aes(label = rpd)) +
#  geom_line(aes(y = realisasi), color = "#e9c46a") +
#  geom_point(aes(y = realisasi),color = "#e9c46a") +
#  xlim(c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December")) +
#  xlab("Bulan") + ylab ("Anggaran") +
#  theme_bw()

#======= Total Realisasi Box =========
#spp_box <- sum(realisasi %>% filter(!is.na(jml)) %>% select(jml))
#spp_box
#realisasi_box <- sum(realisasi %>% filter(!is.na(bulan) & !is.na(jml)) %>% select(jml))
#realisasi_box

#==== Tabel Realisasi =====
#realisasi %>% drop_na(rincian_output,jml) %>% filter(is.na(kontraktual)) %>% group_by(rincian_output) %>% 
#  summarise(realisasi = sum(jml),.groups = 'drop') #realisasi per ro

#pagu %>% drop_na(rincian_output) %>% group_by(rincian_output) %>% 
#  summarise(pagu = sum(jml),.groups = 'drop') # pagu per ro

#realisasi %>% drop_na(jml) %>% filter(kontraktual == "Ya",is.na(bulan)) %>%  
#  group_by(rincian_output) %>% summarise(realisasi = sum(jml)) # otsk per ro

#realisasi %>% drop_na(rincian_output,jml) %>% filter(is.na(bulan),is.na(kontraktual)) %>% 
#  group_by(rincian_output) %>% summarise(spp = sum(jml)) #spp per ro


#left_join(
#  #left join untuk outstanding
#  left_join(
#    #left join untuk spp
#    left_join(#left join untuk pagu dan realisasi
#      pagu %>% drop_na(rincian_output) %>% group_by(rincian_output) %>% summarise(pagu = sum(jml),.groups = 'drop'),
#      realisasi %>% drop_na(rincian_output,jml) %>% filter(!is.na(bulan)) %>% group_by(rincian_output) %>% summarise(realisasi = sum(jml),.groups = 'drop'),
#      by = c('rincian_output')),
#    realisasi %>% drop_na(rincian_output,jml) %>% filter(is.na(bulan),is.na(kontraktual)) %>% group_by(rincian_output) %>% summarise(spp = sum(jml)),
#    by = c('rincian_output')),
#  realisasi %>% drop_na(jml) %>% filter(kontraktual == "Ya",is.na(bulan)) %>%  group_by(rincian_output) %>% summarise(outstanding= sum(jml)), # otsk per ro
#by = c('rincian_output')  
#) %>%
#  mutate(outstanding = replace_na(outstanding,0),spp = replace_na(spp,0)) %>%
#  mutate(total = realisasi + spp + outstanding) %>%
#  mutate(perc_1 = 100*round(realisasi/pagu,digits = 4), .after = realisasi) %>%
#  mutate(perc_2 = 100*round(total/pagu,digits = 4), .after = total)
