addResourcePath("sbadmin2", "startbootstrap-sb-admin-2-gh-pages")

source("R/app/ui_dashboard_sbadmin2_auth.R")

library(DBI)
library(RMariaDB)
library(scales)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

read_query_map <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  marker_idx <- grep("^--\\s*[A-Z][0-9]+\\.", lines)
  if (!length(marker_idx)) return(list())

  marker_code <- sub("^--\\s*([A-Z][0-9]+)\\..*$", "\\1", lines[marker_idx])
  out <- list()
  for (i in seq_along(marker_idx)) {
    start <- marker_idx[i] + 1
    end <- if (i < length(marker_idx)) marker_idx[i + 1] - 1 else length(lines)
    block <- trimws(paste(lines[start:end], collapse = "\n"))
    if (nzchar(block)) out[[marker_code[i]]] <- block
  }
  out
}

fmt_rp <- function(x) {
  paste0("Rp ", format(round(as.numeric(x), 0), big.mark = ".", decimal.mark = ",", scientific = FALSE))
}

kpi_card <- function(value, subtitle, icon, border_class = "primary") {
  tags$div(
    class = paste("card border-left-", border_class, " shadow h-100 py-2 mb-4 sb-kpi-card", sep = ""),
    tags$div(
      class = "card-body",
      tags$div(
        class = "row no-gutters align-items-center",
        tags$div(
          class = "col mr-2",
          tags$div(class = "sb-kpi-subtitle", subtitle),
          tags$div(class = "sb-kpi-value mb-0", value)
        ),
        tags$div(class = "col-auto", tags$i(class = paste("fas fa-2x text-gray-300", icon)))
      )
    )
  )
}

server <- function(input, output, session) {
  sql_path <- "sql/05_mysql_query_visualisasi_multipage.sql"
  query_map <- read_query_map(sql_path)

  auth_user <- Sys.getenv("APP_USERNAME", "admin")
  auth_pass <- Sys.getenv("APP_PASSWORD", "admin123")

  auth <- reactiveValues(stage = "landing", logged_in = FALSE)

  output$auth_stage <- reactive({ auth$stage })
  outputOptions(output, "auth_stage", suspendWhenHidden = FALSE)

  observeEvent(input$go_login, {
    auth$stage <- "login"
  })

  observeEvent(input$back_landing, {
    auth$stage <- "landing"
  })

  observeEvent(input$btn_login, {
    user_in <- trimws(input$username %||% "")
    pass_in <- input$password %||% ""

    if (identical(user_in, auth_user) && identical(pass_in, auth_pass)) {
      auth$logged_in <- TRUE
      auth$stage <- "app"
      updateTabsetPanel(session, "current_tab", selected = "home")
      showNotification("Login berhasil. Selamat datang.", type = "message", duration = 2)
    } else {
      showNotification("Username atau password salah.", type = "error", duration = 4)
    }
  })

  observeEvent(input$btn_logout, {
    auth$logged_in <- FALSE
    auth$stage <- "landing"
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
  })

  observeEvent(input$nav_page, {
    req(auth$logged_in)
    page <- input$nav_page
    if (!is.null(page) && nzchar(page)) {
      updateTabsetPanel(session, "current_tab", selected = page)
    }
  }, ignoreInit = TRUE)

  con <- NULL
  tryCatch({
    con <- DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("MYSQL_HOST", "127.0.0.1"),
      port = as.integer(Sys.getenv("MYSQL_PORT", "3306")),
      user = Sys.getenv("MYSQL_USER", "root"),
      password = Sys.getenv("MYSQL_PASSWORD", ""),
      dbname = Sys.getenv("MYSQL_DB", "db_klinik_normalisasi")
    )
  }, error = function(e) {
    showNotification(paste("Koneksi database gagal:", e$message), type = "error", duration = NULL)
  })

  onStop(function() {
    if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  })

  selected_clinic <- debounce(reactive({
    x <- input$filter_klinik
    if (is.null(x) || !nzchar(trimws(x))) "Semua Cabang Klinik" else x
  }), 250)

  observe({
    req(auth$stage == "app")
    req(!is.null(con) && DBI::dbIsValid(con))

    clinic_df <- tryCatch(
      DBI::dbGetQuery(con, "SELECT DISTINCT clinic_name FROM clinic ORDER BY clinic_name"),
      error = function(e) data.frame(clinic_name = character(0))
    )
    updateSelectInput(
      session,
      "filter_klinik",
      choices = c("Semua Cabang Klinik", clinic_df$clinic_name),
      selected = "Semua Cabang Klinik"
    )
  })

  run_query <- function(code, clinic_name) {
    req(auth$logged_in)
    if (is.null(con) || !DBI::dbIsValid(con)) return(data.frame())
    qry <- query_map[[code]]
    if (is.null(qry) || !nzchar(qry)) return(data.frame())

    clinic_cond <- if (identical(clinic_name, "Semua Cabang Klinik")) {
      "1=1"
    } else {
      paste0("c.clinic_name = ", as.character(DBI::dbQuoteString(con, clinic_name)))
    }

    qry <- gsub("\\{\\{CLINIC_FILTER\\}\\}", clinic_cond, qry)
    tryCatch(DBI::dbGetQuery(con, qry), error = function(e) data.frame())
  }

  query_cache <- new.env(parent = emptyenv())
  fetch <- function(code) {
    reactive({
      req(auth$logged_in)
      clinic_name <- selected_clinic()
      key <- paste(code, clinic_name, sep = "||")
      if (exists(key, envir = query_cache, inherits = FALSE)) {
        return(get(key, envir = query_cache, inherits = FALSE))
      }
      d <- run_query(code, clinic_name)
      assign(key, d, envir = query_cache)
      d
    })
  }

  h1 <- fetch("H1")
  h2 <- fetch("H2")
  h3 <- fetch("H3")
  h4 <- fetch("H4")
  p1 <- fetch("P1")
  p2 <- fetch("P2")
  p3 <- fetch("P3")
  p4 <- fetch("P4")
  d1 <- fetch("D1")
  d2 <- fetch("D2")
  d3 <- fetch("D3")
  o1 <- fetch("O1")
  o2 <- fetch("O2")
  o3 <- fetch("O3")
  f1 <- fetch("F1")
  f2 <- fetch("F2")
  f3 <- fetch("F3")
  f4 <- fetch("F4")

  output$home_box_visit <- renderUI({
    x <- h1(); n <- if (nrow(x)) as.numeric(x$total_kunjungan[1]) else 0
    kpi_card(comma(n, big.mark = "."), "Total Kunjungan", "fa-users", "primary")
  })
  output$home_box_patient <- renderUI({
    x <- h1(); n <- if (nrow(x)) as.numeric(x$total_pasien_unik[1]) else 0
    kpi_card(comma(n, big.mark = "."), "Pasien Unik", "fa-user-check", "info")
  })
  output$home_box_revenue <- renderUI({
    x <- h1(); n <- if (nrow(x)) as.numeric(x$total_revenue[1]) else 0
    kpi_card(fmt_rp(n), "Total Revenue", "fa-money-bill-wave", "success")
  })
  output$home_box_avg_rev <- renderUI({
    x <- h1(); n <- if (nrow(x)) as.numeric(x$avg_revenue_per_visit[1]) else 0
    kpi_card(fmt_rp(n), "Avg Revenue / Visit", "fa-chart-line", "warning")
  })

  output$home_plot_trend <- renderPlotly({
    d <- h2(); req(nrow(d) > 0)
    d$visit_date <- as.Date(d$visit_date)
    plot_ly(d, x = ~visit_date, y = ~total_kunjungan, type = "scatter", mode = "lines+markers",
            line = list(color = "#4e73df")) %>%
      layout(xaxis = list(title = "Tanggal"), yaxis = list(title = "Kunjungan"))
  })
  output$home_plot_payment <- renderPlotly({
    d <- h3(); req(nrow(d) > 0)
    plot_ly(d, labels = ~payment_method, values = ~total_transaksi, type = "pie", textinfo = "label+percent")
  })
  output$home_wordcloud <- renderPlot({
    d <- h4(); req(nrow(d) > 0)
    if (!requireNamespace("wordcloud", quietly = TRUE)) {
      plot.new(); text(0.5, 0.5, "Install paket wordcloud: install.packages('wordcloud')"); return(invisible(NULL))
    }
    d <- d[order(d$total_kasus, decreasing = TRUE), , drop = FALSE]
    wordcloud::wordcloud(
      words = as.character(d$diagnosis_name),
      freq = as.numeric(d$total_kasus),
      min.freq = 1,
      max.words = 120,
      random.order = FALSE,
      colors = grDevices::colorRampPalette(c("#1cc88a", "#36b9cc", "#4e73df", "#224abe"))(8)
    )
  })

  output$pasien_plot_pyramid <- renderPlotly({
    d <- p2(); req(nrow(d) > 0)
    plot_ly(d, x = ~pyramid_value, y = ~age_group, color = ~gender, type = "bar", orientation = "h") %>%
      layout(barmode = "relative", xaxis = list(title = "Jumlah Kunjungan"), yaxis = list(title = "Kelompok Umur"))
  })
  output$pasien_plot_bmi <- renderPlotly({
    d <- p3(); req(nrow(d) > 0)
    plot_ly(d, x = ~height, y = ~weight, type = "scatter", mode = "markers", color = ~gender,
            text = ~paste("Patient:", patient_id, "<br>BMI:", bmi)) %>%
      layout(xaxis = list(title = "Tinggi (cm)"), yaxis = list(title = "Berat (kg)"))
  })
  output$pasien_plot_city <- renderPlotly({
    d <- p4(); req(nrow(d) > 0)
    d <- head(d[order(d$total_kunjungan, decreasing = TRUE), ], 20)
    plot_ly(d, x = ~reorder(patient_city, total_kunjungan), y = ~total_kunjungan, type = "bar",
            marker = list(color = "#f6c23e")) %>%
      layout(xaxis = list(title = "Kota Asal"), yaxis = list(title = "Kunjungan"))
  })
  output$pasien_plot_type <- renderPlotly({
    d <- p1(); req(nrow(d) > 0)
    plot_ly(d, labels = ~patient_type, values = ~total_kunjungan, type = "pie", textinfo = "label+percent")
  })

  output$dokter_plot_specialty <- renderPlotly({
    d <- d1(); req(nrow(d) > 0)
    plot_ly(d, x = ~reorder(doctor_specialty, total_kunjungan), y = ~total_kunjungan, type = "bar",
            marker = list(color = "#36b9cc")) %>%
      layout(xaxis = list(title = "Spesialis"), yaxis = list(title = "Kunjungan"))
  })
  output$dokter_plot_top <- renderPlotly({
    d <- d2(); req(nrow(d) > 0)
    plot_ly(d, x = ~reorder(doctor_name, total_kunjungan), y = ~total_kunjungan, type = "bar",
            marker = list(color = "#4e73df")) %>%
      layout(xaxis = list(title = "Dokter"), yaxis = list(title = "Kunjungan"))
  })
  output$dokter_table_productivity <- renderDT({
    d <- d3(); req(nrow(d) > 0)
    datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$obat_plot_treemap <- renderPlotly({
    d <- o1(); req(nrow(d) > 0)
    plot_ly(d, type = "treemap", labels = ~medicine_category, parents = "", values = ~total_item_resep,
            textinfo = "label+value+percent entry")
  })
  output$obat_plot_top <- renderPlotly({
    d <- o2(); req(nrow(d) > 0)
    plot_ly(d, x = ~reorder(medicine_name, total_resep), y = ~total_resep, type = "bar",
            marker = list(color = "#1cc88a")) %>%
      layout(xaxis = list(title = "Obat"), yaxis = list(title = "Jumlah Resep"))
  })
  output$obat_table_detail <- renderDT({
    d <- o3(); req(nrow(d) > 0)
    datatable(d, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$fin_box_total <- renderUI({
    d <- f1(); x <- if (nrow(d)) as.numeric(d$total_revenue[1]) else 0
    kpi_card(fmt_rp(x), "Total Revenue", "fa-wallet", "success")
  })
  output$fin_box_avg <- renderUI({
    d <- f1(); x <- if (nrow(d)) as.numeric(d$avg_revenue_per_visit[1]) else 0
    kpi_card(fmt_rp(x), "Rata-rata / Kunjungan", "fa-coins", "info")
  })
  output$fin_box_treatment <- renderUI({
    d <- f1(); x <- if (nrow(d)) as.numeric(d$total_treatment_fee[1]) else 0
    kpi_card(fmt_rp(x), "Total Treatment Fee", "fa-file-invoice-dollar", "warning")
  })

  output$fin_plot_monthly <- renderPlotly({
    d <- f2(); req(nrow(d) > 0)
    plot_ly(d, x = ~year_month, y = ~total_revenue, type = "scatter", mode = "lines+markers",
            line = list(color = "#1cc88a")) %>%
      layout(xaxis = list(title = "Bulan"), yaxis = list(title = "Pendapatan"))
  })
  output$fin_plot_city <- renderPlotly({
    d <- f3(); req(nrow(d) > 0)
    plot_ly(d, x = ~reorder(clinic_city, total_revenue), y = ~total_revenue, type = "bar",
            marker = list(color = "#f6c23e")) %>%
      layout(xaxis = list(title = "Kota"), yaxis = list(title = "Pendapatan"))
  })
  output$fin_plot_component <- renderPlotly({
    d <- f4(); req(nrow(d) > 0)
    plot_ly(d, labels = ~komponen, values = ~nominal, type = "pie", textinfo = "label+percent")
  })
}

shinyApp(ui, server)
