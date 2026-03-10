-- ==========================================================
-- QUERY DATASET DASHBOARD MULTI-PAGE (MySQL)
-- Placeholder filter klinik:
--   {{CLINIC_FILTER}}
-- ==========================================================

USE `db_klinik_normalisasi`;

-- H1. KPI HOME
SELECT
  COUNT(*) AS total_kunjungan,
  COUNT(DISTINCT v.patient_id) AS total_pasien_unik,
  ROUND(SUM(COALESCE(t.total_amount, 0)), 2) AS total_revenue,
  ROUND(AVG(COALESCE(t.total_amount, 0)), 2) AS avg_revenue_per_visit
FROM `visit` v
JOIN `clinic` c ON c.clinic_id = v.clinic_id
LEFT JOIN `transactions` t ON t.visit_id = v.visit_id
WHERE {{CLINIC_FILTER}};

-- H2. TREN KUNJUNGAN HARIAN
SELECT
  DATE(v.visit_datetime) AS visit_date,
  COUNT(*) AS total_kunjungan
FROM `visit` v
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY DATE(v.visit_datetime)
ORDER BY visit_date;

-- H3. DISTRIBUSI METODE PEMBAYARAN
SELECT
  COALESCE(t.payment_method, 'unknown') AS payment_method,
  COUNT(*) AS total_transaksi
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY COALESCE(t.payment_method, 'unknown')
ORDER BY total_transaksi DESC;

-- H4. WORDCLOUD DIAGNOSA
SELECT
  d.diagnosis_name,
  COUNT(*) AS total_kasus
FROM `visit_diagnosis` vd
JOIN `diagnosis` d ON d.diagnosis_id = vd.diagnosis_id
JOIN `visit` v ON v.visit_id = vd.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY d.diagnosis_name
ORDER BY total_kasus DESC;

-- P1. RASIO PATIENT TYPE
SELECT
  p.patient_type,
  COUNT(*) AS total_kunjungan,
  COUNT(DISTINCT v.patient_id) AS total_pasien_unik
FROM `visit` v
JOIN `patient` p ON p.patient_id = v.patient_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY p.patient_type
ORDER BY total_kunjungan DESC;

-- P2. PIRAMIDA UMUR X GENDER
WITH age_base AS (
  SELECT
    p.gender,
    TIMESTAMPDIFF(YEAR, p.date_of_birth, v.visit_datetime) AS age_years
  FROM `visit` v
  JOIN `patient` p ON p.patient_id = v.patient_id
  JOIN `clinic` c ON c.clinic_id = v.clinic_id
  WHERE {{CLINIC_FILTER}}
    AND p.date_of_birth IS NOT NULL
    AND v.visit_datetime IS NOT NULL
),
age_bucket AS (
  SELECT
    CASE
      WHEN age_years < 5 THEN '00-04'
      WHEN age_years BETWEEN 5 AND 12 THEN '05-12'
      WHEN age_years BETWEEN 13 AND 17 THEN '13-17'
      WHEN age_years BETWEEN 18 AND 35 THEN '18-35'
      WHEN age_years BETWEEN 36 AND 59 THEN '36-59'
      ELSE '60+'
    END AS age_group,
    gender
  FROM age_base
)
SELECT
  age_group,
  gender,
  COUNT(*) AS total_kunjungan,
  CASE WHEN gender = 'L' THEN -COUNT(*) ELSE COUNT(*) END AS pyramid_value
FROM age_bucket
GROUP BY age_group, gender
ORDER BY FIELD(age_group, '00-04', '05-12', '13-17', '18-35', '36-59', '60+'), gender;

-- P3. SCATTER BMI
SELECT DISTINCT
  p.patient_id,
  p.gender,
  p.height,
  p.weight,
  ROUND(p.weight / POW(p.height / 100, 2), 2) AS bmi
FROM `patient` p
JOIN `visit` v ON v.patient_id = p.patient_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
  AND p.height IS NOT NULL
  AND p.weight IS NOT NULL
  AND p.height > 0
  AND p.weight > 0;

-- P4. SEBARAN KOTA ASAL PASIEN
SELECT
  p.patient_city,
  p.patient_province,
  COUNT(*) AS total_kunjungan
FROM `visit` v
JOIN `patient` p ON p.patient_id = v.patient_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY p.patient_city, p.patient_province
ORDER BY total_kunjungan DESC;

-- D1. KUNJUNGAN PER SPESIALIS
SELECT
  d.doctor_specialty,
  COUNT(v.visit_id) AS total_kunjungan
FROM `doctor` d
LEFT JOIN `visit` v ON v.doctor_id = d.doctor_id
JOIN `clinic` c ON c.clinic_id = d.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY d.doctor_specialty
ORDER BY total_kunjungan DESC;

-- D2. TOP DOKTER BERDASARKAN KUNJUNGAN
SELECT
  d.doctor_name,
  d.doctor_specialty,
  COUNT(v.visit_id) AS total_kunjungan
FROM `doctor` d
LEFT JOIN `visit` v ON v.doctor_id = d.doctor_id
JOIN `clinic` c ON c.clinic_id = d.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY d.doctor_name, d.doctor_specialty
ORDER BY total_kunjungan DESC
LIMIT 20;

-- D3. PRODUKTIVITAS DOKTER (TABEL)
SELECT
  d.doctor_name,
  d.doctor_specialty,
  c.clinic_name,
  COUNT(v.visit_id) AS total_kunjungan,
  COUNT(DISTINCT v.patient_id) AS total_pasien_unik,
  ROUND(SUM(COALESCE(t.total_amount, 0)), 2) AS total_revenue_dokter
FROM `doctor` d
JOIN `clinic` c ON c.clinic_id = d.clinic_id
LEFT JOIN `visit` v ON v.doctor_id = d.doctor_id
LEFT JOIN `transactions` t ON t.visit_id = v.visit_id
WHERE {{CLINIC_FILTER}}
GROUP BY d.doctor_name, d.doctor_specialty, c.clinic_name
ORDER BY total_pasien_unik DESC, total_kunjungan DESC;

-- O1. TREEMAP KATEGORI OBAT
SELECT
  m.medicine_category,
  COUNT(*) AS total_item_resep,
  ROUND(SUM(COALESCE(m.medicine_dosage_per_day, 0) * COALESCE(m.medicine_duration_days, 0)), 2) AS estimasi_total_unit
FROM `visit_medicine` vm
JOIN `medicine` m ON m.medicine_id = vm.medicine_id
JOIN `visit` v ON v.visit_id = vm.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY m.medicine_category
ORDER BY total_item_resep DESC;

-- O2. TOP OBAT PALING SERING DIRESEPKAN
SELECT
  m.medicine_name,
  m.medicine_category,
  COUNT(*) AS total_resep
FROM `visit_medicine` vm
JOIN `medicine` m ON m.medicine_id = vm.medicine_id
JOIN `visit` v ON v.visit_id = vm.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY m.medicine_name, m.medicine_category
ORDER BY total_resep DESC
LIMIT 20;

-- O3. TABEL DETAIL OBAT
SELECT
  m.medicine_name,
  m.medicine_category,
  m.medicine_unit_price,
  m.medicine_dosage_per_day,
  m.medicine_duration_days,
  COUNT(*) AS total_item_resep
FROM `visit_medicine` vm
JOIN `medicine` m ON m.medicine_id = vm.medicine_id
JOIN `visit` v ON v.visit_id = vm.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY
  m.medicine_name,
  m.medicine_category,
  m.medicine_unit_price,
  m.medicine_dosage_per_day,
  m.medicine_duration_days
ORDER BY total_item_resep DESC;

-- F1. KPI LAPORAN KEUANGAN
SELECT
  ROUND(SUM(COALESCE(t.total_amount, 0)), 2) AS total_revenue,
  ROUND(SUM(COALESCE(t.administration_fee, 0)), 2) AS total_administration_fee,
  ROUND(SUM(COALESCE(t.doctor_consultation_fee, 0)), 2) AS total_consultation_fee,
  ROUND(SUM(COALESCE(t.treatment_total, 0)), 2) AS total_treatment_fee,
  ROUND(SUM(COALESCE(t.medicine_total, 0)), 2) AS total_medicine_fee,
  ROUND(AVG(COALESCE(t.total_amount, 0)), 2) AS avg_revenue_per_visit
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}};

-- F2. TREN PENDAPATAN BULANAN
SELECT
  DATE_FORMAT(v.visit_datetime, '%Y-%m') AS year_month,
  ROUND(SUM(COALESCE(t.total_amount, 0)), 2) AS total_revenue
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY DATE_FORMAT(v.visit_datetime, '%Y-%m')
ORDER BY year_month;

-- F3. PENDAPATAN PER KOTA KLINIK
SELECT
  c.clinic_city,
  ROUND(SUM(COALESCE(t.total_amount, 0)), 2) AS total_revenue
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
GROUP BY c.clinic_city
ORDER BY total_revenue DESC;

-- F4. KOMPOSISI KOMPONEN PENDAPATAN
SELECT 'administration_fee' AS komponen, ROUND(SUM(COALESCE(t.administration_fee, 0)), 2) AS nominal
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
UNION ALL
SELECT 'doctor_consultation_fee' AS komponen, ROUND(SUM(COALESCE(t.doctor_consultation_fee, 0)), 2) AS nominal
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
UNION ALL
SELECT 'treatment_total' AS komponen, ROUND(SUM(COALESCE(t.treatment_total, 0)), 2) AS nominal
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}}
UNION ALL
SELECT 'medicine_total' AS komponen, ROUND(SUM(COALESCE(t.medicine_total, 0)), 2) AS nominal
FROM `transactions` t
JOIN `visit` v ON v.visit_id = t.visit_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
WHERE {{CLINIC_FILTER}};

