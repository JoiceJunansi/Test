#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(jsonlite)
})

input_path <- "dataset/Dataset_Klinik_Raw.csv"
output_dir <- "output/sql/normalisasi_klinik"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

to_decimal <- function(x) {
  suppressWarnings(as.numeric(x))
}

normalize_key_value <- function(v) {
  if (inherits(v, "Date") || inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
    v <- as.character(v)
  }
  if (is.numeric(v)) {
    v <- format(v, trim = TRUE, scientific = FALSE, digits = 15)
  }
  v <- trimws(as.character(v))
  v[is.na(v) | v == ""] <- "#NA#"
  v
}

make_key <- function(df, cols) {
  key_parts <- lapply(cols, function(col_name) normalize_key_value(df[[col_name]]))
  do.call(paste, c(key_parts, sep = "||"))
}

safe_parse_json <- function(s, numeric = FALSE) {
  out <- tryCatch(fromJSON(s), error = function(e) NULL)
  if (is.null(out)) {
    return(if (numeric) numeric(0) else character(0))
  }
  out <- unlist(out, recursive = TRUE, use.names = FALSE)
  if (length(out) == 0) {
    return(if (numeric) numeric(0) else character(0))
  }
  if (numeric) {
    return(suppressWarnings(as.numeric(out)))
  }
  as.character(out)
}

parse_json_column <- function(col, numeric = FALSE) {
  clean <- ifelse(is.na(col) | trimws(col) == "", "[]", trimws(col))
  uniq <- unique(clean)
  parsed <- lapply(uniq, safe_parse_json, numeric = numeric)
  parsed[match(clean, uniq)]
}

parse_dmy <- function(x) {
  suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
}

parse_dmyhm <- function(x, tz = "Asia/Jakarta") {
  suppressWarnings(as.POSIXct(x, format = "%d/%m/%Y %H:%M", tz = tz))
}

as_datetime_string <- function(x) {
  out <- ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M:%S"))
  out
}

as_date_string <- function(x) {
  out <- ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d"))
  out
}

clean_text <- function(x) {
  y <- trimws(as.character(x))
  y[y == ""] <- NA_character_
  y
}

median_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::median(x))
}

write_csv_utf8 <- function(df, file_name) {
  write.csv(df, file = file.path(output_dir, file_name), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

expand_lists <- function(visit_ids, list_cols, seq_col = "line_no") {
  if (!length(list_cols)) return(data.frame())
  lens <- lapply(list_cols, lengths)
  max_len <- Reduce(pmax, lens)
  keep <- which(max_len > 0L)
  if (!length(keep)) return(data.frame())

  idx <- rep(keep, max_len[keep])
  seq_no <- sequence(max_len[keep])

  out <- data.frame(
    visit_id = visit_ids[idx],
    stringsAsFactors = FALSE
  )
  out[[seq_col]] <- as.integer(seq_no)

  for (nm in names(list_cols)) {
    out[[nm]] <- mapply(
      function(i, s) {
        v <- list_cols[[nm]][[i]]
        if (length(v) >= s) v[[s]] else NA
      },
      idx,
      seq_no,
      USE.NAMES = FALSE
    )
  }

  detail_cols <- setdiff(names(out), c("visit_id", seq_col))
  is_all_empty <- apply(
    as.data.frame(lapply(out[detail_cols], function(v) is.na(v) | trimws(as.character(v)) == "")),
    1,
    all
  )
  out <- out[!is_all_empty, , drop = FALSE]
  rownames(out) <- NULL
  out
}

build_single_detail <- function(visit_ids, values, seq_col, value_col) {
  lens <- lengths(values)
  keep <- which(lens > 0L)
  if (!length(keep)) return(data.frame())

  idx <- rep(keep, lens[keep])
  seq_no <- sequence(lens[keep])
  out <- data.frame(
    visit_id = visit_ids[idx],
    stringsAsFactors = FALSE
  )
  out[[seq_col]] <- as.integer(seq_no)
  out[[value_col]] <- unlist(values[keep], use.names = FALSE)
  out <- out[!(is.na(out[[value_col]]) | trimws(as.character(out[[value_col]])) == ""), , drop = FALSE]
  rownames(out) <- NULL
  out
}

message("Membaca dataset...")
raw <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
visit_datetime_raw <- raw$visit_datetime
clinic_open_date_raw <- raw$clinic_open_date
date_of_birth_raw <- raw$date_of_birth

raw$visit_datetime <- as_datetime_string(parse_dmyhm(visit_datetime_raw))
raw$clinic_open_date <- as_date_string(parse_dmy(clinic_open_date_raw))
raw$date_of_birth <- as_date_string(parse_dmy(date_of_birth_raw))

raw$height <- suppressWarnings(as.integer(raw$height))
raw$weight <- suppressWarnings(as.integer(raw$weight))
raw$administration_fee <- to_decimal(raw$administration_fee)
raw$doctor_consultation_fee <- to_decimal(raw$doctor_consultation_fee)

message("Parsing kolom list JSON...")
diagnosis_list <- parse_json_column(raw$diagnosis, numeric = FALSE)
treatment_list <- parse_json_column(raw$treatment, numeric = FALSE)
treatment_fee_list <- parse_json_column(raw$treatment_fee, numeric = TRUE)
medicine_name_list <- parse_json_column(raw$medicine_name, numeric = FALSE)
medicine_category_list <- parse_json_column(raw$medicine_category, numeric = FALSE)
medicine_unit_price_list <- parse_json_column(raw$medicine_unit_price, numeric = TRUE)
medicine_dose_list <- parse_json_column(raw$medicine_dosage_per_day, numeric = TRUE)
medicine_duration_list <- parse_json_column(raw$medicine_duration_days, numeric = TRUE)

message("Menyusun EDA...")
missing_df <- data.frame(
  column_name = names(raw),
  missing_count = sapply(raw, function(x) sum(is.na(x) | trimws(as.character(x)) == "")),
  stringsAsFactors = FALSE
)
missing_df$missing_pct <- round(missing_df$missing_count / nrow(raw) * 100, 2)
missing_df <- missing_df[order(-missing_df$missing_count), ]

unique_diagnosis <- length(unique(clean_text(unlist(diagnosis_list, use.names = FALSE))))
unique_treatment <- length(unique(clean_text(unlist(treatment_list, use.names = FALSE))))
unique_medicine <- length(unique(clean_text(unlist(medicine_name_list, use.names = FALSE))))

top_n_counts <- function(x, n = 10) {
  tab <- sort(table(clean_text(x)), decreasing = TRUE)
  tab <- tab[!is.na(names(tab))]
  if (!length(tab)) return(data.frame(category = character(0), total = integer(0)))
  tab <- head(tab, n)
  data.frame(category = names(tab), total = as.integer(tab), stringsAsFactors = FALSE)
}

top_payment <- top_n_counts(raw$payment_method, n = 10)
top_patient_type <- top_n_counts(raw$patient_type, n = 10)
top_doctor_specialty <- top_n_counts(raw$doctor_specialty, n = 10)

visit_dt_values <- parse_dmyhm(visit_datetime_raw)
visit_dt_min <- suppressWarnings(min(visit_dt_values, na.rm = TRUE))
visit_dt_max <- suppressWarnings(max(visit_dt_values, na.rm = TRUE))

open_dt_values <- parse_dmy(clinic_open_date_raw)
open_dt_min <- suppressWarnings(min(open_dt_values, na.rm = TRUE))
open_dt_max <- suppressWarnings(max(open_dt_values, na.rm = TRUE))

all_treatment_fee <- to_decimal(unlist(treatment_fee_list, use.names = FALSE))
all_medicine_price <- to_decimal(unlist(medicine_unit_price_list, use.names = FALSE))
all_medicine_dose <- to_decimal(unlist(medicine_dose_list, use.names = FALSE))
all_medicine_duration <- to_decimal(unlist(medicine_duration_list, use.names = FALSE))
medicine_item_total <- all_medicine_price * all_medicine_dose * all_medicine_duration

numeric_summary <- data.frame(
  metric = c(
    "administration_fee", "doctor_consultation_fee", "treatment_fee_item", "medicine_unit_price_item", "medicine_subtotal_item"
  ),
  min_value = c(
    min(raw$administration_fee, na.rm = TRUE),
    min(raw$doctor_consultation_fee, na.rm = TRUE),
    min(all_treatment_fee, na.rm = TRUE),
    min(all_medicine_price, na.rm = TRUE),
    min(medicine_item_total, na.rm = TRUE)
  ),
  median_value = c(
    median(raw$administration_fee, na.rm = TRUE),
    median(raw$doctor_consultation_fee, na.rm = TRUE),
    median(all_treatment_fee, na.rm = TRUE),
    median(all_medicine_price, na.rm = TRUE),
    median(medicine_item_total, na.rm = TRUE)
  ),
  max_value = c(
    max(raw$administration_fee, na.rm = TRUE),
    max(raw$doctor_consultation_fee, na.rm = TRUE),
    max(all_treatment_fee, na.rm = TRUE),
    max(all_medicine_price, na.rm = TRUE),
    max(medicine_item_total, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

eda_summary_lines <- c(
  "=== EDA DATASET_KLINIK ===",
  sprintf("Total baris: %s", format(nrow(raw), big.mark = ",")),
  sprintf("Total kolom: %s", ncol(raw)),
  sprintf("visit_id unik: %s", ifelse(length(unique(raw$visit_id)) == nrow(raw), "YA", "TIDAK")),
  sprintf("Jumlah klinik unik: %s", length(unique(clean_text(raw$clinic_name)))),
  sprintf("Jumlah dokter unik: %s", length(unique(clean_text(raw$doctor_name)))),
  sprintf("Jumlah pasien unik: %s", length(unique(clean_text(raw$patient_name)))),
  sprintf("Jumlah diagnosis unik: %s", unique_diagnosis),
  sprintf("Jumlah treatment unik: %s", unique_treatment),
  sprintf("Jumlah obat unik: %s", unique_medicine),
  sprintf("Rentang visit_datetime: %s s/d %s", as.character(visit_dt_min), as.character(visit_dt_max)),
  sprintf("Rentang clinic_open_date: %s s/d %s", as.character(open_dt_min), as.character(open_dt_max))
)

writeLines(eda_summary_lines, con = file.path(output_dir, "eda_ringkasan.txt"))
write_csv_utf8(missing_df, "eda_missing_values.csv")
write_csv_utf8(numeric_summary, "eda_numeric_summary.csv")
write_csv_utf8(top_payment, "eda_top_payment_method.csv")
write_csv_utf8(top_patient_type, "eda_top_patient_type.csv")
write_csv_utf8(top_doctor_specialty, "eda_top_doctor_specialty.csv")

message("Menyusun 1NF...")
visit_cols <- c(
  "visit_id", "visit_datetime", "clinic_name", "clinic_city", "clinic_province", "clinic_open_date",
  "head_doctor_name_of_clinic", "doctor_name", "doctor_gender", "doctor_specialty",
  "patient_name", "gender", "date_of_birth", "height", "weight", "patient_city", "patient_province",
  "patient_type", "complaint", "administration_fee", "doctor_consultation_fee", "payment_method"
)
nf1_visit <- unique(raw[visit_cols])

nf1_visit_line <- expand_lists(
  visit_ids = raw$visit_id,
  list_cols = list(
    diagnosis_name = diagnosis_list,
    treatment_name = treatment_list,
    treatment_fee = treatment_fee_list,
    medicine_name = medicine_name_list,
    medicine_category = medicine_category_list,
    medicine_unit_price = medicine_unit_price_list,
    medicine_dosage_per_day = medicine_dose_list,
    medicine_duration_days = medicine_duration_list
  ),
  seq_col = "line_no"
)

nf1_visit_line$treatment_fee <- to_decimal(nf1_visit_line$treatment_fee)
nf1_visit_line$medicine_unit_price <- to_decimal(nf1_visit_line$medicine_unit_price)
nf1_visit_line$medicine_dosage_per_day <- to_decimal(nf1_visit_line$medicine_dosage_per_day)
nf1_visit_line$medicine_duration_days <- suppressWarnings(as.integer(to_decimal(nf1_visit_line$medicine_duration_days)))

message("Menyusun 2NF...")
nf2_visit <- nf1_visit

nf2_visit_diagnosis <- build_single_detail(
  visit_ids = raw$visit_id,
  values = diagnosis_list,
  seq_col = "diagnosis_seq",
  value_col = "diagnosis_name"
)
nf2_visit_diagnosis$diagnosis_name <- clean_text(nf2_visit_diagnosis$diagnosis_name)
nf2_visit_diagnosis <- nf2_visit_diagnosis[!is.na(nf2_visit_diagnosis$diagnosis_name), , drop = FALSE]

nf2_visit_treatment <- expand_lists(
  visit_ids = raw$visit_id,
  list_cols = list(
    treatment_name = treatment_list,
    treatment_fee = treatment_fee_list
  ),
  seq_col = "treatment_seq"
)
nf2_visit_treatment$treatment_name <- clean_text(nf2_visit_treatment$treatment_name)
nf2_visit_treatment$treatment_fee <- to_decimal(nf2_visit_treatment$treatment_fee)
nf2_visit_treatment <- nf2_visit_treatment[!is.na(nf2_visit_treatment$treatment_name), , drop = FALSE]

nf2_visit_medicine <- expand_lists(
  visit_ids = raw$visit_id,
  list_cols = list(
    medicine_name = medicine_name_list,
    medicine_category = medicine_category_list,
    medicine_unit_price = medicine_unit_price_list,
    medicine_dosage_per_day = medicine_dose_list,
    medicine_duration_days = medicine_duration_list
  ),
  seq_col = "medicine_seq"
)
nf2_visit_medicine$medicine_name <- clean_text(nf2_visit_medicine$medicine_name)
nf2_visit_medicine$medicine_category <- clean_text(nf2_visit_medicine$medicine_category)
nf2_visit_medicine$medicine_unit_price <- to_decimal(nf2_visit_medicine$medicine_unit_price)
nf2_visit_medicine$medicine_dosage_per_day <- to_decimal(nf2_visit_medicine$medicine_dosage_per_day)
nf2_visit_medicine$medicine_duration_days <- suppressWarnings(as.integer(to_decimal(nf2_visit_medicine$medicine_duration_days)))
nf2_visit_medicine <- nf2_visit_medicine[!is.na(nf2_visit_medicine$medicine_name), , drop = FALSE]

message("Menyusun 3NF...")
clinic_cols <- c("clinic_name", "clinic_city", "clinic_province", "clinic_open_date", "head_doctor_name_of_clinic")
clinic_dim <- unique(nf2_visit[clinic_cols])
clinic_dim$clinic_key <- make_key(clinic_dim, clinic_cols)
clinic_dim <- clinic_dim[order(clinic_dim$clinic_key), , drop = FALSE]
clinic_dim <- clinic_dim[!duplicated(clinic_dim$clinic_key), , drop = FALSE]

clinic_fee_source <- data.frame(
  clinic_key = make_key(nf2_visit, clinic_cols),
  administration_fee = nf2_visit$administration_fee,
  stringsAsFactors = FALSE
)
clinic_fee <- aggregate(administration_fee ~ clinic_key, data = clinic_fee_source, FUN = median_or_na)
clinic_dim <- merge(clinic_dim, clinic_fee, by = "clinic_key", all.x = TRUE, sort = FALSE)
clinic_dim$clinic_id <- sprintf("CLN%05d", seq_len(nrow(clinic_dim)))
nf3_clinic <- clinic_dim[c("clinic_id", clinic_cols, "administration_fee")]

visit_work <- nf2_visit
visit_work$clinic_key <- make_key(visit_work, clinic_cols)
clinic_id_map <- setNames(nf3_clinic$clinic_id, clinic_dim$clinic_key)
visit_work$clinic_id <- unname(clinic_id_map[visit_work$clinic_key])

doctor_cols <- c("doctor_name", "doctor_gender", "doctor_specialty", "clinic_id")
doctor_dim <- unique(visit_work[doctor_cols])
doctor_dim$doctor_key <- make_key(doctor_dim, doctor_cols)
doctor_dim <- doctor_dim[order(doctor_dim$doctor_key), , drop = FALSE]
doctor_dim <- doctor_dim[!duplicated(doctor_dim$doctor_key), , drop = FALSE]

doctor_fee_source <- data.frame(
  doctor_key = make_key(visit_work, doctor_cols),
  doctor_consultation_fee = visit_work$doctor_consultation_fee,
  stringsAsFactors = FALSE
)
doctor_fee <- aggregate(doctor_consultation_fee ~ doctor_key, data = doctor_fee_source, FUN = median_or_na)
doctor_dim <- merge(doctor_dim, doctor_fee, by = "doctor_key", all.x = TRUE, sort = FALSE)
doctor_dim$doctor_id <- sprintf("DOC%06d", seq_len(nrow(doctor_dim)))
nf3_doctor <- doctor_dim[c("doctor_id", "clinic_id", "doctor_name", "doctor_gender", "doctor_specialty", "doctor_consultation_fee")]

doctor_id_map <- setNames(nf3_doctor$doctor_id, doctor_dim$doctor_key)
visit_work$doctor_key <- make_key(visit_work, doctor_cols)
visit_work$doctor_id <- unname(doctor_id_map[visit_work$doctor_key])

patient_cols <- c("patient_name", "gender", "date_of_birth", "height", "weight", "patient_city", "patient_province", "patient_type")
patient_dim <- unique(visit_work[patient_cols])
patient_dim$patient_key <- make_key(patient_dim, patient_cols)
patient_dim <- patient_dim[order(patient_dim$patient_key), , drop = FALSE]
patient_dim <- patient_dim[!duplicated(patient_dim$patient_key), , drop = FALSE]
patient_dim$patient_id <- sprintf("PAT%07d", seq_len(nrow(patient_dim)))
nf3_patient <- patient_dim[c("patient_id", patient_cols)]

patient_id_map <- setNames(nf3_patient$patient_id, patient_dim$patient_key)
visit_work$patient_key <- make_key(visit_work, patient_cols)
visit_work$patient_id <- unname(patient_id_map[visit_work$patient_key])

nf3_visit <- unique(visit_work[c("visit_id", "visit_datetime", "clinic_id", "doctor_id", "patient_id", "complaint")])

diagnosis_names <- sort(unique(clean_text(nf2_visit_diagnosis$diagnosis_name)))
diagnosis_names <- diagnosis_names[!is.na(diagnosis_names)]
nf3_diagnosis <- data.frame(
  diagnosis_id = sprintf("DIA%05d", seq_along(diagnosis_names)),
  diagnosis_name = diagnosis_names,
  stringsAsFactors = FALSE
)
diagnosis_id_map <- setNames(nf3_diagnosis$diagnosis_id, nf3_diagnosis$diagnosis_name)
nf3_visit_diagnosis <- nf2_visit_diagnosis[c("visit_id", "diagnosis_seq", "diagnosis_name")]
nf3_visit_diagnosis$diagnosis_id <- unname(diagnosis_id_map[nf3_visit_diagnosis$diagnosis_name])
nf3_visit_diagnosis <- nf3_visit_diagnosis[!is.na(nf3_visit_diagnosis$diagnosis_id), c("visit_id", "diagnosis_seq", "diagnosis_id"), drop = FALSE]

treatment_work <- nf2_visit_treatment[c("visit_id", "treatment_seq", "treatment_name", "treatment_fee")]
treatment_work <- treatment_work[!is.na(treatment_work$treatment_name), , drop = FALSE]
treatment_key_df <- treatment_work[c("treatment_name", "treatment_fee")]
treatment_work$treatment_key <- make_key(treatment_key_df, c("treatment_name", "treatment_fee"))
treatment_dim <- unique(treatment_work[c("treatment_key", "treatment_name", "treatment_fee")])
treatment_dim <- treatment_dim[order(treatment_dim$treatment_key), , drop = FALSE]
treatment_dim$treatment_id <- sprintf("TRT%05d", seq_len(nrow(treatment_dim)))
nf3_treatment <- treatment_dim[c("treatment_id", "treatment_name", "treatment_fee")]
treatment_id_map <- setNames(treatment_dim$treatment_id, treatment_dim$treatment_key)
treatment_work$treatment_id <- unname(treatment_id_map[treatment_work$treatment_key])
nf3_visit_treatment <- treatment_work[c("visit_id", "treatment_seq", "treatment_id")]

medicine_work <- nf2_visit_medicine[c(
  "visit_id", "medicine_seq", "medicine_name", "medicine_category", "medicine_unit_price",
  "medicine_dosage_per_day", "medicine_duration_days"
)]
medicine_work <- medicine_work[!is.na(medicine_work$medicine_name), , drop = FALSE]
medicine_key_cols <- c("medicine_name", "medicine_category", "medicine_unit_price", "medicine_dosage_per_day", "medicine_duration_days")
medicine_work$medicine_key <- make_key(medicine_work, medicine_key_cols)
medicine_dim <- unique(medicine_work[c("medicine_key", medicine_key_cols)])
medicine_dim <- medicine_dim[order(medicine_dim$medicine_key), , drop = FALSE]
medicine_dim$medicine_id <- sprintf("MED%06d", seq_len(nrow(medicine_dim)))
nf3_medicine <- medicine_dim[c("medicine_id", medicine_key_cols)]
medicine_id_map <- setNames(medicine_dim$medicine_id, medicine_dim$medicine_key)
medicine_work$medicine_id <- unname(medicine_id_map[medicine_work$medicine_key])
nf3_visit_medicine <- medicine_work[c("visit_id", "medicine_seq", "medicine_id")]

treatment_totals <- aggregate(treatment_fee ~ visit_id, data = nf2_visit_treatment, FUN = function(x) sum(x, na.rm = TRUE))
medicine_calc <- nf2_visit_medicine
medicine_calc$medicine_subtotal <- ifelse(is.na(medicine_calc$medicine_unit_price), 0, medicine_calc$medicine_unit_price) *
  ifelse(is.na(medicine_calc$medicine_dosage_per_day), 0, medicine_calc$medicine_dosage_per_day) *
  ifelse(is.na(medicine_calc$medicine_duration_days), 0, medicine_calc$medicine_duration_days)
medicine_totals <- aggregate(medicine_subtotal ~ visit_id, data = medicine_calc, FUN = function(x) sum(x, na.rm = TRUE))

first_diag <- nf3_visit_diagnosis[order(nf3_visit_diagnosis$visit_id, nf3_visit_diagnosis$diagnosis_seq), , drop = FALSE]
first_diag <- first_diag[!duplicated(first_diag$visit_id), c("visit_id", "diagnosis_id"), drop = FALSE]
names(first_diag)[2] <- "primary_diagnosis_id"

nf3_transactions <- unique(visit_work[c("visit_id", "payment_method", "administration_fee", "doctor_consultation_fee")])
nf3_transactions <- merge(nf3_transactions, treatment_totals, by = "visit_id", all.x = TRUE, sort = FALSE)
names(nf3_transactions)[names(nf3_transactions) == "treatment_fee"] <- "treatment_total"
nf3_transactions <- merge(nf3_transactions, medicine_totals, by = "visit_id", all.x = TRUE, sort = FALSE)
names(nf3_transactions)[names(nf3_transactions) == "medicine_subtotal"] <- "medicine_total"
nf3_transactions <- merge(nf3_transactions, first_diag, by = "visit_id", all.x = TRUE, sort = FALSE)

nf3_transactions$treatment_total[is.na(nf3_transactions$treatment_total)] <- 0
nf3_transactions$medicine_total[is.na(nf3_transactions$medicine_total)] <- 0
nf3_transactions$administration_fee[is.na(nf3_transactions$administration_fee)] <- 0
nf3_transactions$doctor_consultation_fee[is.na(nf3_transactions$doctor_consultation_fee)] <- 0
nf3_transactions$total_amount <- round(
  nf3_transactions$administration_fee +
    nf3_transactions$doctor_consultation_fee +
    nf3_transactions$treatment_total +
    nf3_transactions$medicine_total,
  2
)
nf3_transactions <- nf3_transactions[order(nf3_transactions$visit_id), , drop = FALSE]
nf3_transactions$transaction_id <- seq_len(nrow(nf3_transactions))
nf3_transactions$transaction_number <- sprintf("TRX-%08d", nf3_transactions$transaction_id)
nf3_transactions <- nf3_transactions[c(
  "transaction_id", "transaction_number", "visit_id", "primary_diagnosis_id", "payment_method",
  "administration_fee", "doctor_consultation_fee", "treatment_total", "medicine_total", "total_amount"
)]

message("Menulis output CSV (EDA + 1NF + 2NF + 3NF)...")
write_csv_utf8(nf1_visit, "nf1_visit.csv")
write_csv_utf8(nf1_visit_line, "nf1_visit_line.csv")
write_csv_utf8(nf2_visit, "nf2_visit.csv")
write_csv_utf8(nf2_visit_diagnosis, "nf2_visit_diagnosis.csv")
write_csv_utf8(nf2_visit_treatment, "nf2_visit_treatment.csv")
write_csv_utf8(nf2_visit_medicine, "nf2_visit_medicine.csv")
write_csv_utf8(nf3_clinic, "nf3_clinic.csv")
write_csv_utf8(nf3_doctor, "nf3_doctor.csv")
write_csv_utf8(nf3_patient, "nf3_patient.csv")
write_csv_utf8(nf3_visit, "nf3_visit.csv")
write_csv_utf8(nf3_diagnosis, "nf3_diagnosis.csv")
write_csv_utf8(nf3_treatment, "nf3_treatment.csv")
write_csv_utf8(nf3_medicine, "nf3_medicine.csv")
write_csv_utf8(nf3_visit_diagnosis, "nf3_visit_diagnosis.csv")
write_csv_utf8(nf3_visit_treatment, "nf3_visit_treatment.csv")
write_csv_utf8(nf3_visit_medicine, "nf3_visit_medicine.csv")
write_csv_utf8(nf3_transactions, "nf3_transactions.csv")

normalization_summary <- data.frame(
  table_name = c(
    "nf1_visit", "nf1_visit_line",
    "nf2_visit", "nf2_visit_diagnosis", "nf2_visit_treatment", "nf2_visit_medicine",
    "nf3_clinic", "nf3_doctor", "nf3_patient", "nf3_visit",
    "nf3_diagnosis", "nf3_treatment", "nf3_medicine",
    "nf3_visit_diagnosis", "nf3_visit_treatment", "nf3_visit_medicine", "nf3_transactions"
  ),
  row_count = c(
    nrow(nf1_visit), nrow(nf1_visit_line),
    nrow(nf2_visit), nrow(nf2_visit_diagnosis), nrow(nf2_visit_treatment), nrow(nf2_visit_medicine),
    nrow(nf3_clinic), nrow(nf3_doctor), nrow(nf3_patient), nrow(nf3_visit),
    nrow(nf3_diagnosis), nrow(nf3_treatment), nrow(nf3_medicine),
    nrow(nf3_visit_diagnosis), nrow(nf3_visit_treatment), nrow(nf3_visit_medicine), nrow(nf3_transactions)
  ),
  stringsAsFactors = FALSE
)
write_csv_utf8(normalization_summary, "normalisasi_ringkasan_jumlah_baris.csv")

message("Selesai. Semua file output ada di: ", output_dir)

