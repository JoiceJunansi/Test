library(shiny)
library(bs4Dash)
library(plotly)
library(DT)

ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  
  # --- HEADER: SEAMLESS ---
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "Djiwa Medical", 
      color = "indigo"
    ),
    status = "white",
    rightUi = tags$li(
      class = "nav-item dropdown",
      tags$div(style = "padding: 8px; min-width: 250px;",
               selectInput("filter_klinik", NULL, choices = c("Semua Cabang Klinik"), width = "100%"))
    )
  ),
  
  # --- SIDEBAR: CLEAN ---
  sidebar = bs4DashSidebar(
    skin = "light",
    elevation = 4,
    bs4SidebarMenu(
      id = "current_tab",
      menuItem("Home", tabName = "tab_home", icon = icon("house")),
      menuItem("Operasional & Revenue", tabName = "tab_ops", icon = icon("chart-line")),
      menuItem("Profil & Kesehatan Pasien", tabName = "tab_pasien", icon = icon("hospital-user")),
      menuItem("Medis & Farmasi", tabName = "tab_medis", icon = icon("pills"))
    )
  ),
  
  body = bs4DashBody(
    tags$style("
      /* 1. GLOBAL UI: BACKGROUND & SIDEBAR SEAMLESS */
      .content-wrapper, .main-sidebar, .main-sidebar .sidebar, .brand-link, .os-content, .main-header { 
        background-color: #F4F7FC !important; 
        border: none !important;
      }
      .brand-link .brand-text { color: #4B0082 !important; font-weight: 800; text-transform: uppercase; }

      /* 2. KUSTOMISASI SEMUA TITLE BOX (SOLID BACKGROUND) */
      .card { 
        border: none !important; 
        border-radius: 20px !important; 
        box-shadow: 0 10px 30px rgba(0,0,0,0.05) !important;
        margin-bottom: 25px;
        overflow: hidden;
      }
      
      .card-header { border-bottom: none !important; }
      .card-title { color: white !important; font-weight: 700 !important; font-size: 1.1rem; }

      /* Mapping Warna Status ke Header */
      .card-primary .card-header { background-color: #6247aa !important; }
      .card-info .card-header { background-color: #4facfe !important; }
      .card-success .card-header { background-color: #28a745 !important; }
      .card-warning .card-header { background-color: #fda085 !important; }
      .card-danger .card-header { background-color: #ff758c !important; }
      .card-indigo .card-header { background-color: #6610f2 !important; }

      /* 3. KPI & BANNER */
      .welcome-banner { 
        background: linear-gradient(135deg, #6247aa 0%, #a594f9 100%); 
        color: white; padding: 45px; border-radius: 30px; margin-bottom: 30px;
        box-shadow: 0 15px 35px rgba(98, 71, 170, 0.2);
      }
      .kpi-row { display: flex; gap: 20px; margin-bottom: 25px; flex-wrap: wrap; }
      .kpi-box {
        flex: 1; min-width: 220px; border-radius: 22px; padding: 25px; color: white;
        position: relative; overflow: hidden; transition: 0.4s ease;
        box-shadow: 0 12px 20px rgba(0,0,0,0.08); min-height: 140px;
      }
      .grad-rev { background: linear-gradient(135deg, #6247aa 0%, #a594f9 100%); }
      .grad-visit { background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); }
      .grad-bpjs { background: linear-gradient(135deg, #ff758c 0%, #ff7eb3 100%); }
      .kpi-val { font-size: 28px; font-weight: 800; display: block; margin-top: 8px; }
      .kpi-lab { font-size: 13px; opacity: 0.85; font-weight: 600; text-transform: uppercase; }
      .kpi-icon { position: absolute; right: 15px; bottom: 15px; font-size: 50px; opacity: 0.2; }
    "),
    
    tabItems(
      # --- TAB 1: HOME ---
      tabItem(
        tabName = "tab_home",
        div(class = "welcome-banner",
            h1("Selamat Datang di Djiwa Medical"),
            p("Sistem manajemen data klinik terpadu untuk efisiensi layanan kesehatan Anda."),
            actionButton("btn_start", "Mulai Analisis", class = "btn btn-outline-light", style="border-radius:20px;")
        ),
        fluidRow(
          box(title = "News Kesehatan (YouTube)", width = 8, status = "primary", solidHeader = TRUE,
              textInput("yt_keyword", "Kata kunci", value = "berita kesehatan indonesia terbaru"),
              actionButton("yt_refresh", "Refresh News", icon = icon("sync")),
              br(), br(), uiOutput("youtube_embed")),
          box(title = "Link Cepat News YouTube", width = 4, status = "info", solidHeader = TRUE,
              uiOutput("youtube_news_links"))
        ),
        fluidRow(
          box(title = "Prediksi Kunjungan 14 Hari Ke Depan", width = 8, status = "success", solidHeader = TRUE,
              plotlyOutput("plot_pred_visit")),
          box(title = "Ringkasan Prediksi (Visual)", width = 4, status = "warning", solidHeader = TRUE,
              plotlyOutput("plot_pred_summary", height = "320px"),
              hr(), uiOutput("prediksi_rekomendasi"))
        ),
        fluidRow(
          box(title = "Penyakit yang Sering Muncul (Wordcloud)", width = 12, status = "danger", solidHeader = TRUE,
              plotOutput("plot_wordcloud_home", height = "360px"))
        )
      ),
      
      # --- TAB 2: OPERASIONAL & REVENUE ---
      tabItem(
        tabName = "tab_ops",
        div(class = "kpi-row",
            div(class = "kpi-box grad-rev", 
                span(class = "kpi-lab", "Total Revenue"),
                span(class = "kpi-val", textOutput("total_revenue", inline = TRUE)),
                icon("money-bill-wave", class = "kpi-icon")),
            div(class = "kpi-box grad-visit", 
                span(class = "kpi-lab", "Total Kunjungan"),
                span(class = "kpi-val", textOutput("total_visits", inline = TRUE)),
                icon("users", class = "kpi-icon")),
            div(class = "kpi-box grad-bpjs", 
                span(class = "kpi-lab", "Rasio Pasien"),
                span(class = "kpi-val", textOutput("patient_ratio", inline = TRUE)),
                icon("file-invoice-dollar", class = "kpi-icon"))
        ),
        fluidRow(
          box(title = "Heatmap Pendapatan per Wilayah", width = 7, status = "indigo", solidHeader = TRUE,
              plotlyOutput("plot_heatmap_rev")),
          box(title = "Metode Pembayaran", width = 5, status = "info", solidHeader = TRUE,
              plotlyOutput("plot_pie_payment"))
        ),
        fluidRow(
          box(title = "Tren Kunjungan Pasien (Jam/Hari)", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_line_trend"))
        )
      ),
      
      # --- TAB 3: PROFIL & KESEHATAN PASIEN ---
      tabItem(
        tabName = "tab_pasien",
        fluidRow(
          box(title = "Demografi: Piramida Usia & Gender", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("plot_pyramid")),
          box(title = "Korelasi Fisik (BMI): Tinggi vs Berat", width = 6, status = "success", solidHeader = TRUE,
              plotlyOutput("plot_bmi_scatter"))
        ),
        fluidRow(
          box(title = "Peta Sebaran Pasien (Choropleth)", width = 12, status = "info", solidHeader = TRUE,
              plotlyOutput("plot_patient_map"))
        )
      ),
      
      # --- TAB 4: MEDIS & FARMASI ---
      tabItem(
        tabName = "tab_medis",
        fluidRow(
          box(title = "Top 5 Diagnosa Penyakit", width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("plot_bar_diagnosis")),
          box(title = "Analisis Stok Obat (Treemap)", width = 6, status = "success", solidHeader = TRUE,
              plotlyOutput("plot_treemap_medicine"))
        ),
        fluidRow(
          box(title = "Produktivitas Dokter", width = 12, status = "indigo", solidHeader = TRUE,
              DTOutput("table_doctor"))
        )
      )
    )
  )
)
