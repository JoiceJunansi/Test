-- ==========================================================
-- LOAD DATA CSV HASIL R (EDA + NORMALISASI) KE MYSQL
-- Sumber CSV:
-- /Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik
-- ==========================================================

USE `db_klinik_normalisasi`;

-- Jika muncul error LOCAL INFILE, aktifkan di MySQL server/client.
-- Contoh:
-- SET GLOBAL local_infile = 1;

SET FOREIGN_KEY_CHECKS = 0;

TRUNCATE TABLE `nf3_transactions`;
TRUNCATE TABLE `nf3_visit_medicine`;
TRUNCATE TABLE `nf3_visit_treatment`;
TRUNCATE TABLE `nf3_visit_diagnosis`;
TRUNCATE TABLE `nf3_visit`;
TRUNCATE TABLE `nf3_medicine`;
TRUNCATE TABLE `nf3_treatment`;
TRUNCATE TABLE `nf3_diagnosis`;
TRUNCATE TABLE `nf3_patient`;
TRUNCATE TABLE `nf3_doctor`;
TRUNCATE TABLE `nf3_clinic`;

TRUNCATE TABLE `nf2_visit_medicine`;
TRUNCATE TABLE `nf2_visit_treatment`;
TRUNCATE TABLE `nf2_visit_diagnosis`;
TRUNCATE TABLE `nf2_visit`;

TRUNCATE TABLE `nf1_visit_line`;
TRUNCATE TABLE `nf1_visit`;

SET FOREIGN_KEY_CHECKS = 1;

-- ----------------------
-- LOAD 1NF
-- ----------------------
LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf1_visit.csv'
INTO TABLE `nf1_visit`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf1_visit_line.csv'
INTO TABLE `nf1_visit_line`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

-- ----------------------
-- LOAD 2NF
-- ----------------------
LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf2_visit.csv'
INTO TABLE `nf2_visit`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf2_visit_diagnosis.csv'
INTO TABLE `nf2_visit_diagnosis`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf2_visit_treatment.csv'
INTO TABLE `nf2_visit_treatment`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf2_visit_medicine.csv'
INTO TABLE `nf2_visit_medicine`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

-- ----------------------
-- LOAD 3NF (master dulu, lalu relasi/fact)
-- ----------------------
LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_clinic.csv'
INTO TABLE `nf3_clinic`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_doctor.csv'
INTO TABLE `nf3_doctor`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_patient.csv'
INTO TABLE `nf3_patient`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_diagnosis.csv'
INTO TABLE `nf3_diagnosis`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_treatment.csv'
INTO TABLE `nf3_treatment`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_medicine.csv'
INTO TABLE `nf3_medicine`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_visit.csv'
INTO TABLE `nf3_visit`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_visit_diagnosis.csv'
INTO TABLE `nf3_visit_diagnosis`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_visit_treatment.csv'
INTO TABLE `nf3_visit_treatment`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_visit_medicine.csv'
INTO TABLE `nf3_visit_medicine`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

LOAD DATA LOCAL INFILE '/Users/joicejunansitandirerung/Documents/KULIAH/SEMESTER 2/1. STA2562 PEMROSESAN DATA BESAR/Tugas_Kelompok1/output/sql/normalisasi_klinik/nf3_transactions.csv'
INTO TABLE `nf3_transactions`
CHARACTER SET utf8mb4
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

