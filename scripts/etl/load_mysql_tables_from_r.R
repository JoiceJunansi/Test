#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(DBI)
  library(RMariaDB)
})

# =============================
# Konfigurasi koneksi (env var)
# =============================
# Set env sebelum run, contoh:
# export MYSQL_HOST=127.0.0.1
# export MYSQL_PORT=3306
# export MYSQL_USER=root
# export MYSQL_PASSWORD=xxxxx
# export MYSQL_DB=db_klinik_normalisasi

mysql_host <- Sys.getenv("MYSQL_HOST", unset = "127.0.0.1")
mysql_port <- as.integer(Sys.getenv("MYSQL_PORT", unset = "3306"))
mysql_user <- Sys.getenv("MYSQL_USER", unset = "root")
mysql_password <- Sys.getenv("MYSQL_PASSWORD", unset = "")
mysql_db <- Sys.getenv("MYSQL_DB", unset = "db_klinik_normalisasi")

output_dir <- "output/sql/normalisasi_klinik"

run_schema <- identical(Sys.getenv("RUN_SCHEMA", unset = "0"), "1")
schema_file <- "sql/01_mysql_schema_normalisasi_klinik.sql"

read_csv_fast <- function(path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::fread(path, na.strings = c("", "NA"), encoding = "UTF-8", data.table = FALSE))
  }
  read.csv(path, na.strings = c("", "NA"), check.names = FALSE, encoding = "UTF-8")
}

execute_sql_file <- function(con, file_path) {
  sql_text <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  statements <- trimws(unlist(strsplit(sql_text, ";", fixed = TRUE)))
  statements <- statements[nzchar(statements)]
  for (stmt in statements) {
    dbExecute(con, stmt)
  }
}

if (!dir.exists(output_dir)) {
  stop("Folder output tidak ditemukan: ", output_dir,
       "\nJalankan dulu: Rscript scripts/eda_normalisasi_klinik_mysql.R")
}

cat("Membuka koneksi MySQL...\n")
con <- dbConnect(
  RMariaDB::MariaDB(),
  host = mysql_host,
  port = mysql_port,
  user = mysql_user,
  password = mysql_password,
  dbname = "mysql"
)

on.exit({
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
}, add = TRUE)

if (run_schema) {
  if (!file.exists(schema_file)) stop("File schema tidak ada: ", schema_file)
  cat("Menjalankan schema SQL...\n")
  execute_sql_file(con, schema_file)
}

dbExecute(con, sprintf("USE `%s`", mysql_db))

load_order <- c(
  "nf1_visit",
  "nf1_visit_line",
  "nf2_visit",
  "nf2_visit_diagnosis",
  "nf2_visit_treatment",
  "nf2_visit_medicine",
  "nf3_clinic",
  "nf3_doctor",
  "nf3_patient",
  "nf3_diagnosis",
  "nf3_treatment",
  "nf3_medicine",
  "nf3_visit",
  "nf3_visit_diagnosis",
  "nf3_visit_treatment",
  "nf3_visit_medicine",
  "nf3_transactions"
)

truncate_order <- rev(load_order)

cat("Truncate tabel lama...\n")
dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")
for (tbl in truncate_order) {
  dbExecute(con, sprintf("TRUNCATE TABLE `%s`", tbl))
}
dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1")

cat("Mulai load data CSV -> MySQL...\n")
for (tbl in load_order) {
  csv_path <- file.path(output_dir, paste0(tbl, ".csv"))
  if (!file.exists(csv_path)) stop("CSV tidak ditemukan: ", csv_path)

  cat(sprintf(" - Loading %s ... ", tbl))
  df <- read_csv_fast(csv_path)
  DBI::dbWriteTable(con, name = tbl, value = df, append = TRUE, row.names = FALSE)
  total <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM `%s`", tbl))$n
  cat(sprintf("OK (%s rows)\n", format(total, big.mark = ",")))
}

cat("\nSelesai. Semua tabel sudah terisi.\n")

