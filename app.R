library(shiny)
library(htmltools)
library(reactable)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")),
  navbarPage(title = "Monev Anggaran",
             tabPanel("Home",
                      tags$div(
                        column(width = 11, tags$p('Untuk melakukan update data, klik "Update"')),
                        column(width = 1, actionButton("update", "UPDATE")
                        )),
                      tags$div()
                      ),
             tabPanel("Anggaran",
                      tags$div(
                        column(width = 11,tags$p('Untuk melakukan update data, klik "Update"')),
                        column(width = 1,actionButton("update", "UPDATE")
                        )),
                      tags$div(
                        column(width = 6, class = "col-lg-6 col-md-6 col-sm-12",
                               reactableOutput("table.4")),
                        column(width = 6, class = "col-lg-6 col-md-6 col-sm-12 vertical-center",
                               selectInput(inputId = "modal_1","Realisasi Berdasarkan Jenis Belanja dan Modal",
                                           choices = c("01-January","02-February","03-March","04-April","05-May","06-June","07-July","08-August","09-September","10-October","11-November","12-December"),
                                           selected = format(Sys.Date(), format = "%m-%B"),
                                           multiple = TRUE),
                               tags$p("Berdasarkan Realisasi OM SPAN"),
                               reactableOutput("table.2"),
                               tags$strong("Daftar Berkas SPP"),
                               selectInput(inputId = "spp_1","",
                                           choices = spp$no_spp,
                                           selected = format(Sys.Date(), format = "%m-%B"),
                                           multiple = TRUE),
                               reactableOutput("table.3"))
                       )
                      ),
             tabPanel("RPD",
                      fluidRow(
                        tags$div(
                          column(width = 11,tags$p('Untuk melakukan update data, klik "Update"')),
                          column(width = 1,actionButton("update", "UPDATE")
                          )
                        ),
                        tags$div(
                          column(width = 6, class = "col-lg-6 col-sm-12",
                                 plotlyOutput("graph")),
                          column(width = 6, class = "col-lg-6 col-sm-12",
                                 tags$h3("Rencana Penarikan Dana tiap Bulan"),
                                 reactableOutput("table.1"))
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
  source("project_rpd.R")
  #===== Tabel Realisasi untuk per Jenis Belanja =========
  output$table.4 <- renderReactable({
    reactable(pok_realisasi,
              groupBy = c("rincian_output","kegiatan"),
              columns = list(
                pagu = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
                realisasi = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
              ),filterable = TRUE, highlight = TRUE, height = 1000)
  })
  #===== Tabel Realisasi untuk per Jenis Belanja =========
  output$table.2 <- renderReactable({
    reactable(modal(input$modal_1),
              groupBy = "komponen",
              columns = list(
                pagu = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0)),
                realisasi = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
              ),
              pagination = FALSE, highlight = TRUE, height = 350)
  })
  #===== Tabel Cek untuk SPP =========
  output$table.3 <- renderReactable({
    reactable(cek_spp(input$spp_1),
              columns = list(
                jml = colDef(aggregate = "sum", format = colFormat(separators = TRUE, digits = 0))
              ),
              pagination = FALSE, highlight = TRUE, height = 350, filterable = TRUE)
  })
  #===== Grafik Output RPD keseluruhan =========
  output$graph <- renderPlotly(ggplotly(plot.rpd.1, height = 500) %>% style(textposition = "left"))
  #===== Output RPD keseluruhan =========
  output$table.1 <- renderReactable({
    reactable(rpd_bulan,
              columns = list(
                rpd = colDef(format = colFormat(separators = TRUE, digits = 0)),
                realisasi = colDef(format = colFormat(separators = TRUE, digits = 0))
              ),pagination = FALSE)
  })
  
  #===== Output RPD keseluruhan =========
  output$table <- renderReactable({
    reactable(
      summary_rpd,
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
