library(shiny)
library(htmltools)
library(reactable)
library(plotly)

source("project_rpd.R")

ui <- fluidPage(
  includeCSS("www/style.css"),
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")),
  navbarPage(title = "Monev Anggaran",
#             tabPanel("Home",
#                      tags$div(
#                        column(width = 11, tags$p('Untuk melakukan update data, klik "Update"')),
#                        column(width = 1, #actionButton("update", "UPDATE")
#                        )),
#                      tags$div()
#                      ),
             tabPanel("Anggaran",
                      tags$div(
                        column(width = 11,tags$p('Untuk melakukan update data, klik "Update"')),
                        column(width = 1,actionButton("update", "UPDATE")
                        )),
                      tags$div(
                        column(width = 7, class = "col-lg-7 col-md-6 col-sm-12",
                               reactableOutput("table_4")),
                        column(width = 5, class = "col-lg-5 col-md-6 col-sm-12 vertical-center",
                               selectInput(inputId = "modal_1","Realisasi Berdasarkan Jenis Belanja dan Modal",
                                           choices = c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December"),
                                           selected = format(Sys.Date(), format = "%m-%B"),
                                           multiple = TRUE),
                               tags$p("Berdasarkan Realisasi OM SPAN"),
                               reactableOutput("table_2"),
                               tags$strong("Daftar Berkas SPP"),
                               uiOutput('spp_1'),
                               reactableOutput("table_3"))
                       )
                      ),
             tabPanel("RPD",
                      fluidRow(
                        tags$div(
                          column(width = 11,tags$p('Untuk melakukan update data, klik "Update"')),
                          column(width = 1,actionButton("update_3", "UPDATE")
                          )
                        ),
                        tags$div(
                          column(width = 6, class = "col-lg-6 col-sm-12",
                                 plotlyOutput("graph")),
                          column(width = 6, class = "col-lg-6 col-sm-12",
                                 tags$h3("Rencana Penarikan Dana tiap Bulan"),
                                 reactableOutput("table_1"))
                        ),
                        hr(),
                        tags$div(
                          column(width = 12,
                                 tags$h3("Rencana Penarikan Dana"),
                                 tags$p("Sesuai dengan revisi POK tanggal xx-xx-xxxx"),
                                 reactableOutput("table"))
                        )
                      ))
  )
)

server <- function(input, output){
  #===== Tabel Realisasi untuk Keseluruhan =========
  realisasi <- eventReactive(input$update,{
    gs4_deauth()
    read_sheet("https://docs.google.com/spreadsheets/d/10PeMfrDTBZ2F_kSB98dAHgAly3vfDLTGCfSxGhCCPIw/edit?usp=sharing", sheet = "INPUT UP TUP")
  }, ignoreNULL = FALSE)
  
  pagu <- eventReactive(input$update,{
    gs4_deauth()
    read_sheet("https://docs.google.com/spreadsheets/d/10PeMfrDTBZ2F_kSB98dAHgAly3vfDLTGCfSxGhCCPIw/edit?usp=sharing", sheet = "Rincian Kertas Kerja")
  }, ignoreNULL = FALSE)
    
  output$table_4 <- renderReactable({
    reactable(#pok_realisasi,
      left_join(pagu() %>% group_by(rincian_output,kegiatan,kode_akun1,sub_detail,detail) %>% summarise(pagu = sum(jml), .groups = 'drop') %>% filter(pagu > 0),
                realisasi() %>% group_by(rincian_output,kegiatan,kode_akun1,sub_detail,detail) %>% summarise(realisasi = sum(jml), .groups = 'drop') %>% filter(!is.na(realisasi)),
                by = c('rincian_output','kegiatan','kode_akun1','sub_detail','detail')) %>%  mutate(realisasi = replace_na(realisasi,0)),
              groupBy = c("rincian_output","kegiatan","kode_akun1"),
              columns = list(
                pagu = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
                realisasi = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
              ),filterable = TRUE, highlight = TRUE, height = 900)
  })
  #===== Tabel Realisasi untuk per Jenis Belanja =========
  output$table_2 <- renderReactable({
    modal <- function(input){
      left_join(pagu() %>% group_by(komponen, jenis_belanja, jenis_modal, .drop = FALSE) %>% 
                  summarise(pagu = sum(jml), .groups = 'drop') %>%
                  filter(pagu > 0),
                realisasi() %>% drop_na(bulan) %>% group_by(komponen,jenis_belanja,jenis_modal, .drop = FALSE) %>% 
                  filter(bulan %in% input) %>%
                  summarise(realisasi = sum(jml), .groups = 'drop'),
                by = c('komponen'='komponen','jenis_belanja' = 'jenis_belanja','jenis_modal' = 'jenis_modal'))
    }
    reactable(modal(input$modal_1),
              groupBy = "komponen",
              columns = list(
                pagu = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
                realisasi = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
              ),
              pagination = FALSE, highlight = TRUE, height = 300)
  })
  #===== Tabel Cek untuk SPP =========
  cek_spp <- function(input){
    realisasi()[which(realisasi()$no_spp %in% input),c("no_spp","kegiatan","kode_akun","detail","jml","keterangan")]
  }
  
  spp <- eventReactive(input$update,{
    as.character(unique(realisasi()$no_spp))
  },ignoreNULL = FALSE)
  
  output$spp_1 = renderUI({
    selectInput('spp_1', '', spp(), multiple = TRUE)
  })
  output$table_3 <- renderReactable({
    reactable(cek_spp(input$spp_1),
              columns = list(
                jml = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
              ),
              pagination = FALSE, highlight = TRUE, height = 300, filterable = TRUE)
  })
  #===== Output RPD per Bulan =========
  rpd_2022 <- eventReactive(input$update_3,{
    as_tibble(read.table("https://combinatronics.com/zaky31/rpd/main/rpd_2022_fix.txt", sep = ";", header = TRUE)) %>%
      rename(`01-January` = X.01.January.,
             `02-February` = X.02.February.,
             `03-March` = X.03.March.,
             `04-April` = X.04.April.,
             `05-May` = X.05.May.,
             `06-June` = X.06.June.,
             `07-July` = X.07.July.,
             `08-August` = X.08.August.,
             `09-September` = X.09.September.,
             `10-October` = X.10.October.,
             `11-November` = X.11.November.,
             `12-December` = X.12.December.) %>%
      replace(is.na(.),0)
  },ignoreNULL = FALSE)

  rpd_2022_1 <- eventReactive(input$update_3,{
    gather(rpd_2022(),month,jml,`01-January`:`12-December`)
  },ignoreNULL = FALSE)
  
  rpd_bulan <- eventReactive(input$update_3,{
    left_join(rpd_2022_1() %>% group_by(month) %>% summarise(rpd.bulan = sum(jml),.groups = 'drop'),
              realisasi() %>% drop_na(jml) %>% group_by(bulan) %>% summarise(realisasi_1 = sum(jml),.groups = 'drop'),
              by = c('month'='bulan')) %>%
      mutate(realisasi_1 = replace_na(realisasi_1,0)) %>%
      mutate(rpd = cumsum(as.numeric(rpd.bulan)), .after = 'rpd.bulan') %>%
      mutate(realisasi = cumsum(as.numeric(realisasi_1))) %>%
      mutate(perc_rpd = 100*round(rpd/sum(rpd.bulan),digits = 4), .after = 'rpd') %>%
      mutate(perc_real = 100*round(realisasi/sum(rpd.bulan),digits = 4)) %>%
      select(month,rpd,perc_rpd,realisasi,perc_real) %>%
      mutate(gap = 100*round((rpd - realisasi)/rpd,digits=4))
  },ignoreNULL = FALSE)

    output$table_1 <- renderReactable({
    reactable(#rpd_bulan,
      rpd_bulan(),
              columns = list(
                rpd = colDef(format = colFormat(separators = TRUE, digits = 0)),
                realisasi = colDef(format = colFormat(separators = TRUE, digits = 0))
              ),pagination = FALSE)
  })
    #===== Grafik Output RPD per Bulan =========
    plot.rpd.1 <- eventReactive(input$update_3,{
      ggplot(data = rpd_bulan(), aes(x = 1:12, y = rpd)) + 
        geom_line(aes(y = rpd), color = "#2a9d8f") +
        geom_point(aes(y = rpd), color = "#2a9d8f") +
        geom_text(aes(label = rpd)) +
        geom_line(aes(y = realisasi), color = "#e9c46a") +
        geom_point(aes(y = realisasi),color = "#e9c46a") +
        xlim(c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December")) +
        xlab("Bulan") + ylab ("Anggaran") +
        theme_bw()
    },ignoreNULL = FALSE)
    
    output$graph <- renderPlotly(ggplotly(plot.rpd.1(), height = 500) %>% style(textposition = "left"))
  #===== Output RPD keseluruhan =========
  summary_rpd <- eventReactive(input$update_3,{
    rpd_2022_1() %>% drop_na() %>% group_by(ro, kegiatan, mak, modal, month) %>% 
      summarise(rpd = sum(jml), .groups = 'drop') %>% spread(month, rpd)
  },ignoreNULL = FALSE)
    
  output$table <- renderReactable({
    reactable(
      summary_rpd(),
      groupBy = c("ro","kegiatan","mak"),
      columns = list(
        `01-January` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `02-February` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `03-March` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `04-April` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `05-May` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `06-June` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `07-July` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `08-August` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `09-September` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `10-October` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `11-November` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
        `12-December` = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
      ),
      filterable = TRUE
    )
  })
}

shinyApp(ui, server)
