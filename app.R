library(shiny)
library(htmltools)
library(reactable)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")),
  navbarPage(title = "Monev Anggaran",
             tabPanel("Home",""),
             tabPanel("Anggaran",""),
             tabPanel("RPD",
                      fluidRow(
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
