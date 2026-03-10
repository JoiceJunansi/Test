source("coba.R")

library(DBI)
library(RMariaDB)
library(scales)
library(htmltools)

read_query_map <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  marker_idx <- grep("^--\\s*[0-9]+[A-Z]\\.", lines)
  if (!length(marker_idx)) return(list())

  marker_code <- sub("^--\\s*([0-9]+[A-Z])\\..*$", "\\1", lines[marker_idx])
  out <- list()

  for (i in seq_along(marker_idx)) {
    start <- marker_idx[i] + 1
    end <- if (i < length(marker_idx)) marker_idx[i + 1] - 1 else length(lines)
    block <- paste(lines[start:end], collapse = "\n")
    block <- trimws(block)
    if (nzchar(block)) out[[marker_code[i]]] <- block
  }
  out
}

fmt_rp <- function(x) {
  paste0("Rp ", format(round(as.numeric(x), 0), big.mark = ".", decimal.mark = ",", scientific = FALSE))
}

server <- function(input, output, session) {
  sql_path <- "sql/04_mysql_query_visualisasi_dashboard.sql"
  query_map <- read_query_map(sql_path)
  con <- NULL

  tryCatch({
    con <- dbConnect(
      RMariaDB::MariaDB(),
      host = Sys.getenv("MYSQL_HOST", "127.0.0.1"),
      port = as.integer(Sys.getenv("MYSQL_PORT", "3310")),
      user = Sys.getenv("MYSQL_USER", "root"),
      password = Sys.getenv("MYSQL_PASSWORD", ""),
      dbname = Sys.getenv("MYSQL_DB", "db_klinik_normalisasi")
    )
  }, error = function(e) {
    showNotification(
      paste("Koneksi database gagal:", e$message),
      type = "error",
      duration = NULL
    )
  })

  onStop(function() {
    if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  })

  selected_clinic_raw <- reactive({
    x <- input$filter_klinik
    if (is.null(x) || !nzchar(trimws(x))) "Semua Cabang Klinik" else x
  })
  selected_clinic <- debounce(selected_clinic_raw, 250)

  run_query <- function(code, clinic_name = "Semua Cabang Klinik") {
    if (is.null(con) || !DBI::dbIsValid(con)) return(data.frame())

    clinic_cond <- if (identical(clinic_name, "Semua Cabang Klinik")) {
      "1=1"
    } else {
      paste0("c.clinic_name = ", as.character(DBI::dbQuoteString(con, clinic_name)))
    }

    qry <- switch(
      code,
      "1A" = paste0(
        "SELECT ",
        "COUNT(*) AS total_kunjungan, ",
        "COUNT(DISTINCT v.patient_id) AS total_pasien_unik, ",
        "ROUND(SUM(COALESCE(t.administration_fee,0)),2) AS total_administration_fee, ",
        "ROUND(SUM(COALESCE(t.doctor_consultation_fee,0)),2) AS total_doctor_consultation_fee, ",
        "ROUND(SUM(COALESCE(t.treatment_total,0)),2) AS total_treatment_fee, ",
        "ROUND(SUM(COALESCE(t.administration_fee,0)+COALESCE(t.doctor_consultation_fee,0)+COALESCE(t.treatment_total,0)),2) AS total_revenue_operasional, ",
        "ROUND(SUM(COALESCE(t.total_amount,0)),2) AS total_revenue_all_component ",
        "FROM visit v ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "LEFT JOIN transactions t ON t.visit_id = v.visit_id ",
        "WHERE ", clinic_cond
      ),
      "1D" = paste0(
        "SELECT p.patient_type, ",
        "COUNT(*) AS total_kunjungan, ",
        "COUNT(DISTINCT v.patient_id) AS total_pasien_unik, ",
        "ROUND(100*COUNT(*)/SUM(COUNT(*)) OVER(),2) AS persen_kunjungan ",
        "FROM visit v ",
        "JOIN patient p ON p.patient_id = v.patient_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY p.patient_type ",
        "ORDER BY total_kunjungan DESC"
      ),
      "1F" = paste0(
        "SELECT DATE(v.visit_datetime) AS visit_date, COUNT(*) AS total_kunjungan ",
        "FROM visit v ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY DATE(v.visit_datetime) ",
        "ORDER BY visit_date"
      ),
      "1H" = paste0(
        "SELECT COALESCE(t.payment_method,'unknown') AS payment_method, ",
        "COUNT(*) AS total_transaksi, ",
        "ROUND(100*COUNT(*)/SUM(COUNT(*)) OVER(),2) AS persen_transaksi ",
        "FROM transactions t ",
        "JOIN visit v ON v.visit_id = t.visit_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY COALESCE(t.payment_method,'unknown') ",
        "ORDER BY total_transaksi DESC"
      ),
      "2A" = paste0(
        "WITH age_base AS (",
        "SELECT p.gender, TIMESTAMPDIFF(YEAR,p.date_of_birth,v.visit_datetime) AS age_years ",
        "FROM visit v ",
        "JOIN patient p ON p.patient_id = v.patient_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE p.date_of_birth IS NOT NULL AND v.visit_datetime IS NOT NULL AND ", clinic_cond,
        "), age_bucket AS (",
        "SELECT CASE ",
        "WHEN age_years < 5 THEN '00-04' ",
        "WHEN age_years BETWEEN 5 AND 12 THEN '05-12' ",
        "WHEN age_years BETWEEN 13 AND 17 THEN '13-17' ",
        "WHEN age_years BETWEEN 18 AND 35 THEN '18-35' ",
        "WHEN age_years BETWEEN 36 AND 59 THEN '36-59' ",
        "ELSE '60+' END AS age_group, gender ",
        "FROM age_base) ",
        "SELECT age_group, gender, COUNT(*) AS total_kunjungan, ",
        "CASE WHEN gender='L' THEN -COUNT(*) ELSE COUNT(*) END AS pyramid_value ",
        "FROM age_bucket ",
        "GROUP BY age_group, gender ",
        "ORDER BY FIELD(age_group,'00-04','05-12','13-17','18-35','36-59','60+'), gender"
      ),
      "2C" = paste0(
        "SELECT DISTINCT p.patient_id, p.gender, p.height, p.weight, ",
        "ROUND(p.weight/POW(p.height/100,2),2) AS bmi, ",
        "CASE ",
        "WHEN (p.weight/POW(p.height/100,2)) < 18.5 THEN 'Underweight' ",
        "WHEN (p.weight/POW(p.height/100,2)) < 25 THEN 'Normal' ",
        "WHEN (p.weight/POW(p.height/100,2)) < 30 THEN 'Overweight' ",
        "ELSE 'Obese' END AS bmi_category ",
        "FROM patient p ",
        "JOIN visit v ON v.patient_id = p.patient_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "AND p.height IS NOT NULL AND p.weight IS NOT NULL AND p.height > 0 AND p.weight > 0"
      ),
      "2D" = paste0(
        "SELECT p.patient_city, p.patient_province, COUNT(*) AS total_kunjungan, ",
        "COUNT(DISTINCT v.patient_id) AS total_pasien_unik ",
        "FROM visit v ",
        "JOIN patient p ON p.patient_id = v.patient_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY p.patient_city, p.patient_province ",
        "ORDER BY total_kunjungan DESC"
      ),
      "3A" = paste0(
        "SELECT d.diagnosis_name, COUNT(*) AS total_kasus ",
        "FROM visit_diagnosis vd ",
        "JOIN diagnosis d ON d.diagnosis_id = vd.diagnosis_id ",
        "JOIN visit v ON v.visit_id = vd.visit_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY d.diagnosis_name ",
        "ORDER BY total_kasus DESC ",
        "LIMIT 5"
      ),
      "3E" = paste0(
        "SELECT d.diagnosis_name, COUNT(*) AS total_kasus ",
        "FROM visit_diagnosis vd ",
        "JOIN diagnosis d ON d.diagnosis_id = vd.diagnosis_id ",
        "JOIN visit v ON v.visit_id = vd.visit_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY d.diagnosis_name ",
        "ORDER BY total_kasus DESC"
      ),
      "3C" = paste0(
        "SELECT m.medicine_category, COUNT(*) AS total_item_resep, ",
        "ROUND(SUM(COALESCE(m.medicine_dosage_per_day,0)*COALESCE(m.medicine_duration_days,0)),2) AS estimasi_total_unit_konsumsi ",
        "FROM visit_medicine vm ",
        "JOIN medicine m ON m.medicine_id = vm.medicine_id ",
        "JOIN visit v ON v.visit_id = vm.visit_id ",
        "JOIN clinic c ON c.clinic_id = v.clinic_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY m.medicine_category ",
        "ORDER BY total_item_resep DESC"
      ),
      "3D" = paste0(
        "SELECT d.doctor_name, d.doctor_specialty, c.clinic_name, ",
        "COUNT(v.visit_id) AS total_kunjungan, ",
        "COUNT(DISTINCT v.patient_id) AS total_pasien_unik, ",
        "ROUND(SUM(COALESCE(t.total_amount,0)),2) AS total_revenue_dokter ",
        "FROM doctor d ",
        "JOIN clinic c ON c.clinic_id = d.clinic_id ",
        "LEFT JOIN visit v ON v.doctor_id = d.doctor_id ",
        "LEFT JOIN transactions t ON t.visit_id = v.visit_id ",
        "WHERE ", clinic_cond, " ",
        "GROUP BY d.doctor_name, d.doctor_specialty, c.clinic_name ",
        "ORDER BY total_pasien_unik DESC, total_kunjungan DESC"
      ),
      NULL
    )

    if (is.null(qry) || !nzchar(qry)) qry <- query_map[[code]]
    if (is.null(qry) || !nzchar(qry)) return(data.frame())
    tryCatch(DBI::dbGetQuery(con, qry), error = function(e) data.frame())
  }

  query_cache <- new.env(parent = emptyenv())

  fetch_query <- function(code) {
    reactive({
      clinic_now <- selected_clinic()
      key <- paste(code, clinic_now, sep = "||")
      if (exists(key, envir = query_cache, inherits = FALSE)) {
        return(get(key, envir = query_cache, inherits = FALSE))
      }
      dat <- run_query(code, clinic_now)
      assign(key, dat, envir = query_cache)
      dat
    })
  }

  q1A_data <- fetch_query("1A")
  q1D_data <- fetch_query("1D")
  q1F_data <- fetch_query("1F")
  q1H_data <- fetch_query("1H")
  q2A_data <- fetch_query("2A")
  q2C_data <- fetch_query("2C")
  q2D_data <- fetch_query("2D")
  q3A_data <- fetch_query("3A")
  q3E_data <- fetch_query("3E")
  q3C_data <- fetch_query("3C")
  q3D_data <- fetch_query("3D")

  observeEvent(input$btn_start, {
    updatebs4SidebarMenu(session, "current_tab", selected = "tab_ops")
  })

  yt_keyword <- reactive({
    raw_k <- if (is.null(input$yt_keyword)) "" else input$yt_keyword
    k <- trimws(raw_k)
    if (!nzchar(k)) "berita kesehatan indonesia terbaru" else k
  })

  observeEvent(input$yt_refresh, {
    yt_keyword()
  })

  output$youtube_embed <- renderUI({
    q <- URLencode(yt_keyword(), reserved = TRUE)
    src <- paste0("https://www.youtube.com/embed?listType=search&list=", q)
    tags$iframe(
      class = "yt-frame",
      src = src,
      allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
      allowfullscreen = NA
    )
  })

  output$youtube_news_links <- renderUI({
    base_topics <- c(
      "berita kesehatan indonesia hari ini",
      "kemenkes update kesehatan",
      "WHO health news",
      "wabah demam berdarah indonesia",
      "tips hidup sehat dokter"
    )
    links <- lapply(base_topics, function(topic) {
      href <- paste0("https://www.youtube.com/results?search_query=", URLencode(topic, reserved = TRUE))
      tags$li(tags$a(href = href, target = "_blank", rel = "noopener noreferrer", topic))
    })

    custom_query <- paste0("https://www.youtube.com/results?search_query=", URLencode(yt_keyword(), reserved = TRUE))
    tagList(
      tags$p(strong("Topik default:")),
      tags$ul(links),
      tags$hr(),
      tags$p(
        tags$strong("Sesuai kata kunci kamu: "),
        tags$a(href = custom_query, target = "_blank", rel = "noopener noreferrer", yt_keyword())
      )
    )
  })

  pred_visit <- reactive({
    d <- q1F_data()
    if (!nrow(d)) return(data.frame())

    d$visit_date <- as.Date(d$visit_date)
    d <- d[order(d$visit_date), , drop = FALSE]
    d <- d[!is.na(d$visit_date), , drop = FALSE]
    if (!nrow(d)) return(data.frame())

    # Gunakan maksimal 180 hari terakhir agar prediksi lebih adaptif
    if (nrow(d) > 180) d <- tail(d, 180)

    d$t <- seq_len(nrow(d))
    d$wday <- factor(weekdays(d$visit_date))

    fit <- tryCatch(
      lm(total_kunjungan ~ t + wday, data = d),
      error = function(e) NULL
    )
    if (is.null(fit)) return(data.frame())

    horizon <- 14
    future_dates <- seq(max(d$visit_date) + 1, by = "day", length.out = horizon)
    newd <- data.frame(
      t = max(d$t) + seq_len(horizon),
      wday = factor(weekdays(future_dates), levels = levels(d$wday))
    )
    pred <- suppressWarnings(as.numeric(predict(fit, newdata = newd)))
    pred <- pmax(round(pred), 0)

    data.frame(
      visit_date = future_dates,
      pred_kunjungan = pred
    )
  })

  if (!is.null(con) && DBI::dbIsValid(con)) {
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
  }

  observe({
    d <- q1F_data()
    if (!nrow(d)) return()

    d$visit_date <- as.Date(d$visit_date)
    d <- d[!is.na(d$visit_date), , drop = FALSE]
    if (!nrow(d)) return()

    available_dates <- sort(unique(d$visit_date))
    min_date <- min(available_dates)
    max_date <- max(available_dates)
    all_dates <- seq(min_date, max_date, by = "day")
    disabled_dates <- setdiff(all_dates, available_dates)
    disabled_str <- format(disabled_dates, "%Y-%m-%d")

    cur <- input$date_filter
    start_sel <- min_date
    end_sel <- max_date
    if (!is.null(cur) && length(cur) == 2) {
      cur_start <- as.Date(cur[1])
      cur_end <- as.Date(cur[2])
      if (!is.na(cur_start) && cur_start %in% available_dates) start_sel <- cur_start
      if (!is.na(cur_end) && cur_end %in% available_dates) end_sel <- cur_end
      if (end_sel < start_sel) end_sel <- start_sel
    }

    updateDateRangeInput(
      session,
      "date_filter",
      start = start_sel,
      end = end_sel,
      min = min_date,
      max = max_date
    )

    session$sendCustomMessage(
      "setDisabledDates",
      list(
        id = "date_filter",
        disabled = unname(disabled_str)
      )
    )
  })

  output$box_visit <- renderbs4ValueBox({
    k <- q1A_data()
    total <- if (nrow(k)) as.numeric(k$total_kunjungan[1]) else 0
    bs4ValueBox(
      value = comma(total, big.mark = "."),
      subtitle = "Total Kunjungan",
      icon = icon("users"),
      color = "primary"
    )
  })

  output$box_rev <- renderbs4ValueBox({
    k <- q1A_data()
    rev <- if (nrow(k)) k$total_revenue_operasional[1] else 0
    bs4ValueBox(
      value = fmt_rp(rev),
      subtitle = "Revenue Operasional",
      icon = icon("money-bill-wave"),
      color = "success"
    )
  })

  output$box_bpjs <- renderbs4ValueBox({
    r <- q1D_data()
    pct <- 0
    if (nrow(r)) {
      idx <- which(toupper(trimws(r$patient_type)) == "BPJS")
      if (length(idx)) pct <- r$persen_kunjungan[idx[1]]
    }
    bs4ValueBox(
      value = paste0(round(as.numeric(pct), 2), "%"),
      subtitle = "Rasio Kunjungan BPJS",
      icon = icon("user-shield"),
      color = "warning"
    )
  })

  output$box_avg <- renderbs4ValueBox({
    k <- q1A_data()
    avg_rev <- 0
    if (nrow(k) && k$total_kunjungan[1] > 0) {
      avg_rev <- k$total_revenue_operasional[1] / k$total_kunjungan[1]
    }
    bs4ValueBox(
      value = fmt_rp(avg_rev),
      subtitle = "Rata-rata Revenue / Kunjungan",
      icon = icon("chart-line"),
      color = "info"
    )
  })

  output$plot_trend <- renderPlotly({
    d <- q1F_data()
    req(nrow(d) > 0)
    d$visit_date <- as.Date(d$visit_date)

    if (!is.null(input$date_filter) && length(input$date_filter) == 2) {
      d <- d[d$visit_date >= input$date_filter[1] & d$visit_date <= input$date_filter[2], , drop = FALSE]
    }
    req(nrow(d) > 0)

    plot_ly(d, x = ~visit_date, y = ~total_kunjungan, type = "scatter", mode = "lines+markers",
            line = list(color = "#007bff"), marker = list(size = 6)) %>%
      layout(xaxis = list(title = "Tanggal"), yaxis = list(title = "Jumlah Kunjungan"))
  })

  output$plot_pay <- renderPlotly({
    d <- q1H_data()
    req(nrow(d) > 0)
    plot_ly(
      d,
      labels = ~payment_method,
      values = ~total_transaksi,
      type = "pie",
      textinfo = "label+percent"
    )
  })

  output$plot_pred_visit <- renderPlotly({
    hist <- q1F_data()
    pred <- pred_visit()
    req(nrow(hist) > 0, nrow(pred) > 0)

    hist$visit_date <- as.Date(hist$visit_date)
    hist <- hist[order(hist$visit_date), , drop = FALSE]
    hist <- tail(hist, 45)

    plot_ly() %>%
      add_lines(
        data = hist,
        x = ~visit_date, y = ~total_kunjungan,
        name = "Aktual",
        line = list(color = "#007bff", width = 2)
      ) %>%
      add_lines(
        data = pred,
        x = ~visit_date, y = ~pred_kunjungan,
        name = "Prediksi",
        line = list(color = "#28a745", width = 2, dash = "dash")
      ) %>%
      layout(
        xaxis = list(title = "Tanggal"),
        yaxis = list(title = "Jumlah Kunjungan"),
        legend = list(orientation = "h", x = 0, y = 1.08)
      )
  })

  output$plot_pred_summary <- renderPlotly({
    pred <- pred_visit()
    hist <- q1F_data()
    req(nrow(pred) > 0, nrow(hist) > 0)

    hist$visit_date <- as.Date(hist$visit_date)
    hist <- hist[order(hist$visit_date), , drop = FALSE]
    recent_avg <- mean(tail(hist$total_kunjungan, 30), na.rm = TRUE)
    pred_avg <- mean(pred$pred_kunjungan, na.rm = TRUE)
    peak_idx <- which.max(pred$pred_kunjungan)
    peak_val <- pred$pred_kunjungan[peak_idx]
    peak_date <- format(pred$visit_date[peak_idx], "%d %b %Y")

    gauge_max <- max(c(recent_avg, pred_avg, peak_val), na.rm = TRUE)
    if (!is.finite(gauge_max) || gauge_max <= 0) gauge_max <- 1
    gauge_max <- ceiling(gauge_max * 1.2)

    plot_ly() %>%
      add_trace(
        type = "indicator",
        mode = "number+delta+gauge",
        value = pred_avg,
        title = list(text = "Rata-rata Prediksi (14 hari)"),
        delta = list(reference = recent_avg, relative = TRUE, valueformat = ".1%"),
        number = list(valueformat = ",.0f"),
        gauge = list(
          axis = list(range = list(0, gauge_max)),
          bar = list(color = "#28a745"),
          bgcolor = "#f3f6f9"
        ),
        domain = list(x = c(0, 1), y = c(0.52, 1))
      ) %>%
      add_trace(
        type = "indicator",
        mode = "number+gauge",
        value = peak_val,
        title = list(text = paste0("Puncak Prediksi (", peak_date, ")")),
        number = list(valueformat = ",.0f"),
        gauge = list(
          axis = list(range = list(0, gauge_max)),
          bar = list(color = "#ffc107"),
          bgcolor = "#f3f6f9"
        ),
        domain = list(x = c(0, 1), y = c(0, 0.48))
      ) %>%
      layout(margin = list(l = 20, r = 20, b = 20, t = 20))
  })

  output$prediksi_rekomendasi <- renderUI({
    pred <- pred_visit()
    hist <- q1F_data()
    diag <- q3A_data()
    req(nrow(pred) > 0, nrow(hist) > 0)

    peak_idx <- which.max(pred$pred_kunjungan)
    peak_date <- as.character(pred$visit_date[peak_idx])
    peak_val <- pred$pred_kunjungan[peak_idx]

    hist$visit_date <- as.Date(hist$visit_date)
    hist <- hist[order(hist$visit_date), , drop = FALSE]
    recent_avg <- mean(tail(hist$total_kunjungan, 30), na.rm = TRUE)
    pred_avg <- mean(pred$pred_kunjungan, na.rm = TRUE)
    delta_pct <- if (isTRUE(recent_avg > 0)) ((pred_avg - recent_avg) / recent_avg) * 100 else 0

    top_diag <- if (nrow(diag)) as.character(diag$diagnosis_name[1]) else "Tidak tersedia"

    trend_label <- if (delta_pct >= 0) "naik" else "turun"

    tagList(
      tags$p(tags$strong("Ringkasan Prediksi:")),
      tags$ul(
        tags$li(paste0("Puncak kunjungan diperkirakan pada ", peak_date, " dengan ±", peak_val, " kunjungan.")),
        tags$li(paste0("Rata-rata 14 hari ke depan diproyeksikan ", trend_label, " ", sprintf("%.1f", abs(delta_pct)), "% dibanding 30 hari terakhir.")),
        tags$li(paste0("Diagnosis dominan saat ini: ", top_diag, "."))
      ),
      tags$hr(),
      tags$p(tags$strong("Saran untuk masyarakat:")),
      tags$ul(
        tags$li("Untuk keluhan ringan, hindari jam ramai dan manfaatkan reservasi online jika tersedia."),
        tags$li("Siapkan obat rutin pribadi lebih awal agar tidak menumpuk saat hari puncak."),
        tags$li("Perkuat pencegahan (istirahat, hidrasi, kebersihan tangan, masker saat gejala muncul).")
      )
    )
  })

  output$plot_wordcloud_home <- renderPlot({
    d <- q3E_data()
    req(nrow(d) > 0)

    if (!requireNamespace("wordcloud", quietly = TRUE)) {
      plot.new()
      text(
        0.5, 0.5,
        "Paket 'wordcloud' belum terpasang.\nInstall dulu: install.packages('wordcloud')",
        cex = 1
      )
      return(invisible(NULL))
    }

    d <- d[order(d$total_kasus, decreasing = TRUE), , drop = FALSE]
    d <- head(d, 100)
    words <- as.character(d$diagnosis_name)
    freq <- as.numeric(d$total_kasus)

    suppressWarnings({
      wordcloud::wordcloud(
        words = words,
        freq = freq,
        scale = c(4.2, 0.8),
        min.freq = 1,
        max.words = 100,
        random.order = FALSE,
        rot.per = 0.2,
        colors = grDevices::colorRampPalette(c("#b30000", "#e34a33", "#fdbb84", "#fee8c8"))(8)
      )
    })
  })

  output$plot_pyramid <- renderPlotly({
    d <- q2A_data()
    req(nrow(d) > 0)
    plot_ly(
      d,
      x = ~pyramid_value,
      y = ~age_group,
      color = ~gender,
      colors = c("#007bff", "#e83e8c"),
      type = "bar",
      orientation = "h"
    ) %>%
      layout(barmode = "relative", xaxis = list(title = "Jumlah Kunjungan"), yaxis = list(title = "Kelompok Umur"))
  })

  output$plot_bmi <- renderPlotly({
    d <- q2C_data()
    req(nrow(d) > 0)
    plot_ly(
      d,
      x = ~height,
      y = ~weight,
      type = "scatter",
      mode = "markers",
      color = ~bmi_category,
      colors = "Set2",
      marker = list(size = 8, opacity = 0.7),
      text = ~paste("Patient:", patient_id, "<br>BMI:", bmi)
    ) %>%
      layout(xaxis = list(title = "Tinggi (cm)"), yaxis = list(title = "Berat (kg)"))
  })

  output$plot_city_dist <- renderPlotly({
    d <- q2D_data()
    req(nrow(d) > 0)
    d <- d[order(d$total_kunjungan, decreasing = TRUE), , drop = FALSE]
    d <- head(d, 20)
    plot_ly(
      d,
      x = ~reorder(patient_city, total_kunjungan),
      y = ~total_kunjungan,
      type = "bar",
      marker = list(color = "#ffc107")
    ) %>%
      layout(xaxis = list(title = "Kota Asal Pasien"), yaxis = list(title = "Total Kunjungan"))
  })

  output$plot_diag <- renderPlotly({
    d <- q3A_data()
    req(nrow(d) > 0)
    plot_ly(
      d,
      x = ~reorder(diagnosis_name, total_kasus),
      y = ~total_kasus,
      type = "bar",
      marker = list(color = "#dc3545")
    ) %>%
      layout(xaxis = list(title = "Diagnosis"), yaxis = list(title = "Total Kasus"))
  })

  output$plot_treemap <- renderPlotly({
    d <- q3C_data()
    req(nrow(d) > 0)
    plot_ly(
      d,
      type = "treemap",
      labels = ~medicine_category,
      parents = "",
      values = ~total_item_resep,
      textinfo = "label+value+percent entry"
    )
  })

  output$table_doctor <- renderDT({
    d <- q3D_data()
    req(nrow(d) > 0)
    datatable(
      d,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
