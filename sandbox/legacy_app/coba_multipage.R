library(shiny)
library(bs4Dash)
library(plotly)
library(DT)

ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  header = bs4DashNavbar(
    title = bs4DashBrand(title = "Klinik Dashboard v2", color = "primary"),
    status = "white",
    rightUi = tags$li(
      class = "nav-item dropdown",
      tags$div(
        style = "padding: 8px; min-width: 320px;",
        selectInput("filter_klinik", NULL, choices = c("Semua Cabang Klinik"), width = "100%")
      )
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    elevation = 4,
    bs4SidebarMenu(
      id = "current_tab",
      menuItem("Home", tabName = "tab_home", icon = icon("house")),
      menuItem("Pasien", tabName = "tab_pasien", icon = icon("hospital-user")),
      menuItem("Dokter", tabName = "tab_dokter", icon = icon("user-md")),
      menuItem("Obat", tabName = "tab_obat", icon = icon("pills")),
      menuItem("Laporan Keuangan", tabName = "tab_keuangan", icon = icon("money-check-dollar"))
    )
  ),
  body = bs4DashBody(
    tags$style(HTML("
      .content-wrapper { background-color: #f4f7f6 !important; }
      .card { border: none !important; border-radius: 16px !important; box-shadow: 0 4px 25px rgba(0,0,0,0.08) !important; }
      .welcome-banner {
        background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
        color: white; padding: 30px; border-radius: 20px; margin-bottom: 20px;
      }
    ")),
    tabItems(
      tabItem(
        tabName = "tab_home",
        div(
          class = "welcome-banner",
          h2("Dashboard Klinik Multi-Page"),
          p("Ringkasan cepat operasional klinik berdasarkan filter cabang.")
        ),
        fluidRow(
          bs4ValueBoxOutput("home_box_visit", width = 3),
          bs4ValueBoxOutput("home_box_patient", width = 3),
          bs4ValueBoxOutput("home_box_revenue", width = 3),
          bs4ValueBoxOutput("home_box_avg_rev", width = 3)
        ),
        fluidRow(
          box(title = "Tren Kunjungan Harian", width = 8, status = "primary", plotlyOutput("home_plot_trend")),
          box(title = "Distribusi Metode Pembayaran", width = 4, status = "info", plotlyOutput("home_plot_payment"))
        ),
        fluidRow(
          box(title = "Penyakit yang Sering Muncul (Wordcloud)", width = 12, status = "danger", plotOutput("home_wordcloud", height = "360px"))
        )
      ),
      tabItem(
        tabName = "tab_pasien",
        fluidRow(
          box(title = "Piramida Demografi", width = 7, status = "primary", plotlyOutput("pasien_plot_pyramid")),
          box(title = "Scatter BMI", width = 5, status = "info", plotlyOutput("pasien_plot_bmi"))
        ),
        fluidRow(
          box(title = "Sebaran Kota Asal Pasien", width = 8, status = "warning", plotlyOutput("pasien_plot_city")),
          box(title = "Rasio Tipe Pasien", width = 4, status = "secondary", plotlyOutput("pasien_plot_type"))
        )
      ),
      tabItem(
        tabName = "tab_dokter",
        fluidRow(
          box(title = "Kunjungan per Spesialis", width = 6, status = "primary", plotlyOutput("dokter_plot_specialty")),
          box(title = "Top Dokter", width = 6, status = "info", plotlyOutput("dokter_plot_top"))
        ),
        fluidRow(
          box(title = "Tabel Produktivitas Dokter", width = 12, status = "success", DTOutput("dokter_table_productivity"))
        )
      ),
      tabItem(
        tabName = "tab_obat",
        fluidRow(
          box(title = "Treemap Kategori Obat", width = 6, status = "primary", plotlyOutput("obat_plot_treemap")),
          box(title = "Top Obat Paling Sering Diresepkan", width = 6, status = "info", plotlyOutput("obat_plot_top"))
        ),
        fluidRow(
          box(title = "Tabel Detail Obat", width = 12, status = "success", DTOutput("obat_table_detail"))
        )
      ),
      tabItem(
        tabName = "tab_keuangan",
        fluidRow(
          bs4ValueBoxOutput("fin_box_total", width = 4),
          bs4ValueBoxOutput("fin_box_avg", width = 4),
          bs4ValueBoxOutput("fin_box_treatment", width = 4)
        ),
        fluidRow(
          box(title = "Tren Pendapatan Bulanan", width = 8, status = "primary", plotlyOutput("fin_plot_monthly")),
          box(title = "Pendapatan per Kota Klinik", width = 4, status = "warning", plotlyOutput("fin_plot_city"))
        ),
        fluidRow(
          box(title = "Komposisi Komponen Pendapatan", width = 12, status = "info", plotlyOutput("fin_plot_component"))
        )
      )
    )
  )
)

