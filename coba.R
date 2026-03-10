
library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(wordcloud)

ui <- bs4DashPage(
  dark = NULL,
  help = NULL,
  
  # 1. NAVBAR / HEADER
  header = bs4DashNavbar(
    title = bs4DashBrand(title = "Klinik", color = "primary"),
    status = "white",
    rightUi = tags$li(
      class = "nav-item dropdown",
      tags$div(style = "padding: 8px; min-width: 250px;",
               selectInput("filter_klinik", NULL, choices = c("Semua Cabang Klinik"), width = "100%"))
    )
  ),
  
  # 2. SIDEBAR
  sidebar = bs4DashSidebar(
    skin = "light",
    elevation = 4,
    bs4SidebarMenu(
      id = "current_tab",
      menuItem("Home", tabName = "tab_home", icon = icon("house")),
      menuItem("Dashboard Operasional", tabName = "tab_ops", icon = icon("chart-line")),
      menuItem("Analitik Pasien", tabName = "tab_pasien", icon = icon("hospital-user")),
      menuItem("Medis & Farmasi", tabName = "tab_medis", icon = icon("pills"))
    )
  ),
  
  # 3. BODY DENGAN DESAIN CUSTOM
  body = bs4DashBody(
    # --- CUSTOM CSS UNTUK TAMPILAN PREMIUM ---
    tags$style("
      .content-wrapper { background-color: #f4f7f6 !important; }
      .card { border: none !important; border-radius: 16px !important; box-shadow: 0 4px 25px rgba(0,0,0,0.08) !important; }
      
      /* Welcome Banner */
      .welcome-banner { 
        background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); 
        color: white; padding: 40px; border-radius: 24px; margin-bottom: 30px;
        box-shadow: 0 10px 30px rgba(0,123,255,0.2);
      }
      
      /* KPI Card Custom */
      .kpi-card {
        background: white; border-radius: 16px; padding: 20px;
        display: flex; align-items: center; transition: all 0.3s ease;
        border: 1px solid #f0f0f0; height: 110px; margin-bottom: 20px;
      }
      .kpi-card:hover { transform: translateY(-5px); box-shadow: 0 12px 30px rgba(0,0,0,0.1) !important; }
      .kpi-icon {
        width: 60px; height: 60px; border-radius: 12px; display: flex;
        align-items: center; justify-content: center; font-size: 24px;
        margin-right: 20px; color: white;
      }
      .kpi-title { color: #888; font-size: 14px; font-weight: 500; }
      .kpi-value { font-size: 22px; font-weight: 700; color: #333; }
      
      /* Warna Ikon */
      .bg-blue { background: #007bff; box-shadow: 0 4px 15px rgba(0,123,255,0.3); }
      .bg-teal { background: #20c997; box-shadow: 0 4px 15px rgba(32,201,151,0.3); }
      .bg-green { background: #28a745; box-shadow: 0 4px 15px rgba(40,167,69,0.3); }

      .yt-frame {
        width: 100%;
        height: 420px;
        border: 0;
        border-radius: 14px;
      }
    "),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setDisabledDates', function(data) {
        if (!data || !data.id) return;
        var el = $('#' + data.id);
        if (!el.length) return;
        el.find('input').each(function() {
          try {
            $(this).datepicker('setDatesDisabled', data.disabled || []);
          } catch (e) {}
        });
      });
    ")),
    
    tabItems(
      # --- TAB 1: HOME (LANDING PAGE) ---
      tabItem(
        tabName = "tab_home",
        div(class = "welcome-banner",
            h1("Selamat Datang di Klinik gangss!"),
            p("Sistem manajemen data klinik terpadu untuk efisiensi layanan kesehatan Anda."),
            #actionButton("btn_start", "Mulai Analisis", class = "btn btn-outline-light", style="border-radius:20px;")
        ),
        fluidRow(
          box(
            title = "News Kesehatan (YouTube)",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            textInput("yt_keyword", "Kata kunci", value = "berita kesehatan indonesia terbaru"),
            actionButton("yt_refresh", "Refresh News", icon = icon("sync")),
            br(), br(),
            uiOutput("youtube_embed")
          ),
          box(
            title = "Link Cepat News YouTube",
            width = 4,
            status = "info",
            solidHeader = TRUE,
            uiOutput("youtube_news_links")
          )
        ),
        fluidRow(
          box(
            title = "Prediksi Kunjungan 14 Hari Ke Depan",
            width = 8,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("plot_pred_visit")
          ),
          box(
            title = "Ringkasan Prediksi (Visual)",
            width = 4,
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("plot_pred_summary", height = "320px"),
            hr(),
            uiOutput("prediksi_rekomendasi")
          )
        ),
        fluidRow(
          box(
            title = "Penyakit yang Sering Muncul (Wordcloud)",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            plotOutput("plot_wordcloud_home", height = "360px")
          )
        )
      ),
      
      # --- TAB 2: OPERASIONAL (Berdasarkan Query 1A - 1H) ---
      tabItem(
        tabName = "tab_ops",
        fluidRow(
          bs4ValueBoxOutput("box_visit", width = 3), # Query 1A
          bs4ValueBoxOutput("box_rev", width = 3),   # Query 1A
          bs4ValueBoxOutput("box_bpjs", width = 3),  # Query 1D
          bs4ValueBoxOutput("box_avg", width = 3)    # Query 1A
        ),
        fluidRow(
          box(title = "Tren Kunjungan Harian", width = 8, status = "primary", 
              dateRangeInput("date_filter", "Periode:", start = Sys.Date()-30),
              plotlyOutput("plot_trend")), # Query 1F
          box(title = "Metode Pembayaran", width = 4, status = "info",
              plotlyOutput("plot_pay"))     # Query 1H
        )
      ),
      
      # --- TAB 3: PASIEN (Berdasarkan Query 2A - 2E) ---
      tabItem(
        tabName = "tab_pasien",
        fluidRow(
          box(title = "Piramida Demografi", width = 7, plotlyOutput("plot_pyramid")), # Query 2A
          box(title = "Analisis BMI (Tinggi vs Berat)", width = 5, plotlyOutput("plot_bmi")) # Query 2C
        ),
        fluidRow(
          box(title = "Sebaran Kota Asal Pasien", width = 12, status = "warning",
              plotlyOutput("plot_city_dist")) # Query 2D
        )
      ),
      
      # --- TAB 4: MEDIS (Berdasarkan Query 3A - 3D) ---
      tabItem(
        tabName = "tab_medis",
        fluidRow(
          box(title = "Top 5 Diagnosa", width = 6, status = "danger", plotlyOutput("plot_diag")), # Query 3A
          box(title = "Stok", width = 6, plotlyOutput("plot_treemap")) # Query 3C
        ),
        fluidRow(
          box(title = "Produktivitas & Revenue Dokter", width = 12, status = "success", 
              DTOutput("table_doctor")) # Query 3D
        )
      )
    )
  )
)
