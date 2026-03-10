# Dashboard Klinik dengan R Shiny

Project ini berisi pipeline pengolahan data klinik, normalisasi database MySQL, dan dashboard R Shiny multipage dengan template SB Admin 2.

## Entry Point Utama

- App terstruktur: `app_structured.R`
- App versi lama tetap dipertahankan di root dan di `sandbox/legacy_app/`

## Struktur Project

```text
Tugas_Kelompok1/
├─ app_structured.R
├─ README.md
├─ .gitignore
├─ R/
│  └─ app/
│     ├─ app_dashboard_sbadmin2_auth.R
│     └─ ui_dashboard_sbadmin2_auth.R
├─ data/
│  ├─ raw/
│  │  └─ Dataset_Klinik_Raw.csv
│  └─ processed/
├─ docs/
│  ├─ erd/
│  ├─ reports/
│  └─ screenshots/
├─ scripts/
│  └─ etl/
│     ├─ eda_normalisasi_klinik_mysql.R
│     └─ load_mysql_tables_from_r.R
├─ sql/
│  ├─ schema/
│  │  └─ 01_mysql_schema_normalisasi_klinik.sql
│  ├─ load/
│  │  └─ 02_mysql_load_data_output_r.sql
│  ├─ analytics/
│  │  └─ 03_mysql_query_relasi_dan_analitik.sql
│  └─ dashboard/
│     ├─ 04_mysql_query_visualisasi_dashboard.sql
│     └─ 05_mysql_query_visualisasi_multipage.sql
├─ www/
│  └─ sbadmin2-overrides.css
├─ startbootstrap-sb-admin-2-gh-pages/
├─ sandbox/
│  ├─ legacy_app/
│  └─ sql_drafts/
└─ dataset/
```

## Fungsi Tiap Folder

- `R/app/`: file aplikasi Shiny yang sudah dirapikan.
- `data/raw/`: dataset mentah asli.
- `data/processed/`: hasil olahan atau ekspor siap analisis.
- `docs/`: ERD, laporan, dan screenshot.
- `scripts/etl/`: script EDA, cleaning, wrangling, load database.
- `sql/schema/`: DDL database.
- `sql/load/`: query load data.
- `sql/analytics/`: query relasi dan analitik.
- `sql/dashboard/`: query untuk visualisasi dashboard.
- `www/`: CSS tambahan untuk tampilan Shiny.
- `startbootstrap-sb-admin-2-gh-pages/`: asset template frontend.
- `sandbox/`: file lama/eksperimen yang tetap disimpan.

## Menjalankan App

```r
setwd("/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1")

Sys.setenv(
  MYSQL_HOST = "127.0.0.1",
  MYSQL_PORT = "3306",
  MYSQL_USER = "root",
  MYSQL_PASSWORD = "PASSWORD_MYSQL_ANDA",
  MYSQL_DB = "db_klinik_normalisasi",
  APP_USERNAME = "admin",
  APP_PASSWORD = "admin123"
)

source("app_structured.R")
```

## Catatan

- File di root belum dihapus agar workflow lama tetap aman.
- Untuk repo GitHub final, file eksperimen di root sebaiknya dipindahkan bertahap atau diarsipkan.
