# Alur Eksekusi (MySQL + TablePlus)

1. Jalankan EDA + normalisasi dengan R:

```bash
Rscript scripts/eda_normalisasi_klinik_mysql.R
```

2. Buat database dan semua tabel (1NF, 2NF, 3NF):

```sql
SOURCE sql/01_mysql_schema_normalisasi_klinik.sql;
```

3. Load CSV hasil R ke tabel MySQL:

```sql
SOURCE sql/02_mysql_load_data_output_r.sql;
```

4. Jalankan query relasi/analitik:

```sql
SOURCE sql/03_mysql_query_relasi_dan_analitik.sql;
```

## Alternatif isi tabel langsung dari R (tanpa LOCAL INFILE)

Jika `LOAD DATA LOCAL INFILE` diblokir client/server, gunakan script ini:

```bash
export MYSQL_HOST=127.0.0.1
export MYSQL_PORT=3306
export MYSQL_USER=root
export MYSQL_PASSWORD='password_mysql_kamu'
export MYSQL_DB=db_klinik_normalisasi
export RUN_SCHEMA=0
Rscript scripts/load_mysql_tables_from_r.R
```

## Output penting

- Ringkasan EDA:
  - `output/sql/normalisasi_klinik/eda_ringkasan.txt`
  - `output/sql/normalisasi_klinik/eda_missing_values.csv`
  - `output/sql/normalisasi_klinik/eda_numeric_summary.csv`
- Ringkasan jumlah baris normalisasi:
  - `output/sql/normalisasi_klinik/normalisasi_ringkasan_jumlah_baris.csv`
