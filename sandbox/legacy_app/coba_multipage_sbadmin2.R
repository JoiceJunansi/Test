library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
    tags$link(rel = "stylesheet", href = "sbadmin2/vendor/fontawesome-free/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Nunito:200,300,400,600,700,800,900"),
    tags$link(rel = "stylesheet", href = "sbadmin2/vendor/bootstrap/css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", href = "sbadmin2/css/sb-admin-2.min.css"),
    tags$link(rel = "stylesheet", href = "sbadmin2-overrides.css"),
    tags$script(HTML("\n      $(document).on('click', '.sb-nav-link', function(e) {\n        e.preventDefault();\n        var page = $(this).data('page');\n        Shiny.setInputValue('nav_page', page, {priority: 'event'});\n        $('.sb-nav-item').removeClass('active');\n        $(this).closest('.sb-nav-item').addClass('active');\n      });\n    "))
  ),

  tags$div(
    id = "wrapper",

    tags$ul(
      class = "navbar-nav bg-gradient-primary sidebar sidebar-dark accordion",
      id = "accordionSidebar",

      tags$a(
        class = "sidebar-brand d-flex align-items-center justify-content-center",
        href = "#",
        tags$div(class = "sidebar-brand-icon rotate-n-15", tags$i(class = "fas fa-clinic-medical")),
        tags$div(class = "sidebar-brand-text mx-3 sb-nav-brand", "KLINIK", tags$sup("v2"))
      ),

      tags$hr(class = "sidebar-divider my-0"),

      tags$li(
        class = "nav-item sb-nav-item active",
        tags$a(class = "nav-link sb-nav-link", href = "#", `data-page` = "home",
               tags$i(class = "fas fa-fw fa-home"), tags$span("Home"))
      ),
      tags$li(
        class = "nav-item sb-nav-item",
        tags$a(class = "nav-link sb-nav-link", href = "#", `data-page` = "pasien",
               tags$i(class = "fas fa-fw fa-hospital-user"), tags$span("Pasien"))
      ),
      tags$li(
        class = "nav-item sb-nav-item",
        tags$a(class = "nav-link sb-nav-link", href = "#", `data-page` = "dokter",
               tags$i(class = "fas fa-fw fa-user-md"), tags$span("Dokter"))
      ),
      tags$li(
        class = "nav-item sb-nav-item",
        tags$a(class = "nav-link sb-nav-link", href = "#", `data-page` = "obat",
               tags$i(class = "fas fa-fw fa-pills"), tags$span("Obat"))
      ),
      tags$li(
        class = "nav-item sb-nav-item",
        tags$a(class = "nav-link sb-nav-link", href = "#", `data-page` = "keuangan",
               tags$i(class = "fas fa-fw fa-wallet"), tags$span("Laporan Keuangan"))
      )
    ),

    tags$div(
      id = "content-wrapper",
      class = "d-flex flex-column",

      tags$div(
        id = "content",

        tags$nav(
          class = "navbar navbar-expand navbar-light bg-white topbar mb-4 static-top shadow",
          tags$button(class = "btn btn-link d-md-none rounded-circle mr-3", tags$i(class = "fa fa-bars")),
          tags$div(
            class = "ml-auto",
            tags$form(
              class = "form-inline",
              selectInput("filter_klinik", NULL, choices = c("Semua Cabang Klinik"), width = "100%")
            )
          )
        ),

        tags$div(
          class = "container-fluid sb-page-content",

          tabsetPanel(
            id = "current_tab",
            type = "hidden",

            tabPanel(
              title = "home",
              fluidRow(
                column(3, uiOutput("home_box_visit")),
                column(3, uiOutput("home_box_patient")),
                column(3, uiOutput("home_box_revenue")),
                column(3, uiOutput("home_box_avg_rev"))
              ),
              fluidRow(
                column(
                  8,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Tren Kunjungan Harian")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("home_plot_trend", height = "330px"))
                  )
                ),
                column(
                  4,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Metode Pembayaran")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("home_plot_payment", height = "330px"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Wordcloud Penyakit Sering Muncul")),
                    tags$div(class = "card-body short", plotOutput("home_wordcloud", height = "280px"))
                  )
                )
              )
            ),

            tabPanel(
              title = "pasien",
              fluidRow(
                column(
                  7,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Piramida Demografi")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("pasien_plot_pyramid", height = "330px"))
                  )
                ),
                column(
                  5,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Scatter BMI")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("pasien_plot_bmi", height = "330px"))
                  )
                )
              ),
              fluidRow(
                column(
                  8,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Sebaran Kota Asal Pasien")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("pasien_plot_city", height = "330px"))
                  )
                ),
                column(
                  4,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Rasio Tipe Pasien")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("pasien_plot_type", height = "330px"))
                  )
                )
              )
            ),

            tabPanel(
              title = "dokter",
              fluidRow(
                column(
                  6,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Kunjungan per Spesialis")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("dokter_plot_specialty", height = "330px"))
                  )
                ),
                column(
                  6,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Top Dokter")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("dokter_plot_top", height = "330px"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Produktivitas Dokter")),
                    tags$div(class = "card-body short", DTOutput("dokter_table_productivity"))
                  )
                )
              )
            ),

            tabPanel(
              title = "obat",
              fluidRow(
                column(
                  6,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Treemap Kategori Obat")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("obat_plot_treemap", height = "330px"))
                  )
                ),
                column(
                  6,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Top Obat")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("obat_plot_top", height = "330px"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Detail Obat")),
                    tags$div(class = "card-body short", DTOutput("obat_table_detail"))
                  )
                )
              )
            ),

            tabPanel(
              title = "keuangan",
              fluidRow(
                column(4, uiOutput("fin_box_total")),
                column(4, uiOutput("fin_box_avg")),
                column(4, uiOutput("fin_box_treatment"))
              ),
              fluidRow(
                column(
                  8,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Tren Pendapatan Bulanan")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("fin_plot_monthly", height = "330px"))
                  )
                ),
                column(
                  4,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Pendapatan per Kota")),
                    tags$div(class = "card-body sb-card-body", plotlyOutput("fin_plot_city", height = "330px"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  tags$div(
                    class = "card shadow mb-4",
                    tags$div(class = "card-header py-3", tags$h6(class = "m-0 font-weight-bold text-primary", "Komposisi Pendapatan")),
                    tags$div(class = "card-body short", plotlyOutput("fin_plot_component", height = "280px"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
