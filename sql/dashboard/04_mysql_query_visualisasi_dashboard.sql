

USE `db_klinik_normalisasi`;


-- 1A. KPI Cards (akumulasi total, ditampilkan kotak/card)
SELECT
  COUNT(*) AS total_kunjungan,
  COUNT(DISTINCT v.patient_id) AS total_pasien_unik,
  ROUND(SUM(COALESCE(t.administration_fee, 0)), 2) AS total_administration_fee,
  ROUND(SUM(COALESCE(t.doctor_consultation_fee, 0)), 2) AS total_doctor_consultation_fee,
  ROUND(SUM(COALESCE(t.treatment_total, 0)), 2) AS total_treatment_fee,
  ROUND(SUM(
    COALESCE(t.administration_fee, 0) +
    COALESCE(t.doctor_consultation_fee, 0) +
    COALESCE(t.treatment_total, 0)
  ), 2) AS total_revenue_operasional,
  ROUND(SUM(COALESCE(t.total_amount, 0)), 2) AS total_revenue_all_component
FROM `visit` v
LEFT JOIN `transactions` t ON t.visit_id = v.visit_id;

-- 1B. Total Revenue per Wilayah (Kota)
-- Variabel sesuai permintaan: treatment_fee + administration_fee + doctor_consultation_fee
SELECT
  c.clinic_city,
  COUNT(DISTINCT v.visit_id) AS total_kunjungan,
  ROUND(SUM(COALESCE(t.administration_fee, 0)), 2) AS total_administration_fee,
  ROUND(SUM(COALESCE(t.doctor_consultation_fee, 0)), 2) AS total_doctor_consultation_fee,
  ROUND(SUM(COALESCE(t.treatment_total, 0)), 2) AS total_treatment_fee,
  ROUND(SUM(
    COALESCE(t.administration_fee, 0) +
    COALESCE(t.doctor_consultation_fee, 0) +
    COALESCE(t.treatment_total, 0)
  ), 2) AS total_revenue_operasional
FROM `visit` v
JOIN `clinic` c ON c.clinic_id = v.clinic_id
LEFT JOIN `transactions` t ON t.visit_id = v.visit_id
GROUP BY c.clinic_city
ORDER BY total_revenue_operasional DESC;

-- 1C. Total Revenue per Wilayah (Provinsi)
SELECT
  c.clinic_province,
  COUNT(DISTINCT v.visit_id) AS total_kunjungan,
  ROUND(SUM(COALESCE(t.administration_fee, 0)), 2) AS total_administration_fee,
  ROUND(SUM(COALESCE(t.doctor_consultation_fee, 0)), 2) AS total_doctor_consultation_fee,
  ROUND(SUM(COALESCE(t.treatment_total, 0)), 2) AS total_treatment_fee,
  ROUND(SUM(
    COALESCE(t.administration_fee, 0) +
    COALESCE(t.doctor_consultation_fee, 0) +
    COALESCE(t.treatment_total, 0)
  ), 2) AS total_revenue_operasional
FROM `visit` v
JOIN `clinic` c ON c.clinic_id = v.clinic_id
LEFT JOIN `transactions` t ON t.visit_id = v.visit_id
GROUP BY c.clinic_province
ORDER BY total_revenue_operasional DESC;

-- 1D. Rasio Pasien per Tipe (BPJS vs Umum vs Asuransi, dll)
SELECT
  p.patient_type,
  COUNT(*) AS total_kunjungan,
  COUNT(DISTINCT v.patient_id) AS total_pasien_unik,
  ROUND(100 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS persen_kunjungan
FROM `visit` v
JOIN `patient` p ON p.patient_id = v.patient_id
GROUP BY p.patient_type
ORDER BY total_kunjungan DESC;

-- 1E. Heatmap Pendapatan Wilayah (Kota) + skor intensitas warna 0-100
WITH revenue_city AS (
  SELECT
    c.clinic_city,
    ROUND(SUM(
      COALESCE(t.administration_fee, 0) +
      COALESCE(t.doctor_consultation_fee, 0) +
      COALESCE(t.treatment_total, 0)
    ), 2) AS total_revenue_operasional
  FROM `visit` v
  JOIN `clinic` c ON c.clinic_id = v.clinic_id
  LEFT JOIN `transactions` t ON t.visit_id = v.visit_id
  GROUP BY c.clinic_city
)
SELECT
  clinic_city,
  total_revenue_operasional,
  ROUND(
    100 * (total_revenue_operasional - MIN(total_revenue_operasional) OVER ())
    / NULLIF(MAX(total_revenue_operasional) OVER () - MIN(total_revenue_operasional) OVER (), 0),
    2
  ) AS intensity_score_0_100
FROM revenue_city
ORDER BY total_revenue_operasional DESC;

-- 1F. Line Chart Tren Kunjungan per Hari
SELECT
  DATE(v.visit_datetime) AS visit_date,
  COUNT(*) AS total_kunjungan
FROM `visit` v
GROUP BY DATE(v.visit_datetime)
ORDER BY visit_date;

-- 1G. Line Chart Jam Sibuk per Hari-Minggu
SELECT
  DAYNAME(v.visit_datetime) AS day_name,
  HOUR(v.visit_datetime) AS visit_hour,
  COUNT(*) AS total_kunjungan
FROM `visit` v
GROUP BY DAYNAME(v.visit_datetime), HOUR(v.visit_datetime)
ORDER BY
  FIELD(DAYNAME(v.visit_datetime), 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
  visit_hour;

-- 1H. Pie Chart Metode Pembayaran
SELECT
  COALESCE(t.payment_method, 'unknown') AS payment_method,
  COUNT(*) AS total_transaksi,
  ROUND(100 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS persen_transaksi
FROM `transactions` t
GROUP BY COALESCE(t.payment_method, 'unknown')
ORDER BY total_transaksi DESC;


-- 2A. Piramida Penduduk (Distribusi umur x gender, berbasis kunjungan)
-- Catatan: nilai laki-laki dibuat negatif agar mudah diplot menjadi piramida.
WITH age_base AS (
  SELECT
    p.gender,
    TIMESTAMPDIFF(YEAR, p.date_of_birth, v.visit_datetime) AS age_years
  FROM `visit` v
  JOIN `patient` p ON p.patient_id = v.patient_id
  WHERE p.date_of_birth IS NOT NULL
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
  CASE
    WHEN gender = 'L' THEN -COUNT(*)
    ELSE COUNT(*)
  END AS pyramid_value
FROM age_bucket
GROUP BY age_group, gender
ORDER BY FIELD(age_group, '00-04', '05-12', '13-17', '18-35', '36-59', '60+'), gender;

-- 2B. Segmentasi Usia (Anak-anak / Dewasa / Lansia)
SELECT
  CASE
    WHEN TIMESTAMPDIFF(YEAR, p.date_of_birth, v.visit_datetime) < 18 THEN 'Anak-anak'
    WHEN TIMESTAMPDIFF(YEAR, p.date_of_birth, v.visit_datetime) < 60 THEN 'Dewasa'
    ELSE 'Lansia'
  END AS segmen_usia,
  COUNT(*) AS total_kunjungan,
  ROUND(100 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS persen_kunjungan
FROM `visit` v
JOIN `patient` p ON p.patient_id = v.patient_id
WHERE p.date_of_birth IS NOT NULL
GROUP BY segmen_usia
ORDER BY FIELD(segmen_usia, 'Anak-anak', 'Dewasa', 'Lansia');

-- 2C. Scatterplot BMI (height vs weight)
SELECT
  p.patient_id,
  p.gender,
  p.height,
  p.weight,
  ROUND(p.weight / POW(p.height / 100, 2), 2) AS bmi,
  CASE
    WHEN (p.weight / POW(p.height / 100, 2)) < 18.5 THEN 'Underweight'
    WHEN (p.weight / POW(p.height / 100, 2)) < 25 THEN 'Normal'
    WHEN (p.weight / POW(p.height / 100, 2)) < 30 THEN 'Overweight'
    ELSE 'Obese'
  END AS bmi_category
FROM `patient` p
WHERE p.height IS NOT NULL
  AND p.weight IS NOT NULL
  AND p.height > 0
  AND p.weight > 0;

-- 2D. Choropleth map - sebaran pasien berdasarkan kota asal
SELECT
  p.patient_city,
  p.patient_province,
  COUNT(*) AS total_kunjungan,
  COUNT(DISTINCT v.patient_id) AS total_pasien_unik
FROM `visit` v
JOIN `patient` p ON p.patient_id = v.patient_id
GROUP BY p.patient_city, p.patient_province
ORDER BY total_kunjungan DESC;

-- 2E. Origin-Destination pasien (patient_city vs clinic_city)
SELECT
  p.patient_city,
  p.patient_province,
  c.clinic_city,
  c.clinic_province,
  CASE
    WHEN p.patient_city = c.clinic_city THEN 'Dalam Kota'
    ELSE 'Lintas Kota'
  END AS travel_type,
  COUNT(*) AS total_kunjungan
FROM `visit` v
JOIN `patient` p ON p.patient_id = v.patient_id
JOIN `clinic` c ON c.clinic_id = v.clinic_id
GROUP BY p.patient_city, p.patient_province, c.clinic_city, c.clinic_province, travel_type
ORDER BY total_kunjungan DESC;

-- 3A. Top 5 Diagnosa Nasional
SELECT
  d.diagnosis_name,
  COUNT(*) AS total_kasus
FROM `visit_diagnosis` vd
JOIN `diagnosis` d ON d.diagnosis_id = vd.diagnosis_id
GROUP BY d.diagnosis_name
ORDER BY total_kasus DESC
LIMIT 5;

-- 3B. Top 5 Diagnosa per Provinsi (untuk lihat penyakit dominan per lokasi)
WITH diag_prov AS (
  SELECT
    c.clinic_province,
    d.diagnosis_name,
    COUNT(*) AS total_kasus,
    ROW_NUMBER() OVER (
      PARTITION BY c.clinic_province
      ORDER BY COUNT(*) DESC
    ) AS rn
  FROM `visit_diagnosis` vd
  JOIN `diagnosis` d ON d.diagnosis_id = vd.diagnosis_id
  JOIN `visit` v ON v.visit_id = vd.visit_id
  JOIN `clinic` c ON c.clinic_id = v.clinic_id
  GROUP BY c.clinic_province, d.diagnosis_name
)
SELECT
  clinic_province,
  diagnosis_name,
  total_kasus
FROM diag_prov
WHERE rn <= 5
ORDER BY clinic_province, total_kasus DESC;

-- 3C. Treemap Analisis "Stok" Obat (proxy: frekuensi dan estimasi konsumsi)
-- Catatan: karena tidak ada tabel stok aktual, dipakai proxy penggunaan resep.
SELECT
  m.medicine_category,
  COUNT(*) AS total_item_resep,
  ROUND(SUM(COALESCE(m.medicine_dosage_per_day, 0) * COALESCE(m.medicine_duration_days, 0)), 2) AS estimasi_total_unit_konsumsi
FROM `visit_medicine` vm
JOIN `medicine` m ON m.medicine_id = vm.medicine_id
GROUP BY m.medicine_category
ORDER BY total_item_resep DESC;

-- 3D. Produktivitas Dokter (jumlah pasien tertinggi)
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
GROUP BY d.doctor_name, d.doctor_specialty, c.clinic_name
ORDER BY total_pasien_unik DESC, total_kunjungan DESC;

-- 3E. Dataset Wordcloud Diagnosa (semua frekuensi, tanpa limit)
SELECT
  d.diagnosis_name,
  COUNT(*) AS total_kasus
FROM `visit_diagnosis` vd
JOIN `diagnosis` d ON d.diagnosis_id = vd.diagnosis_id
GROUP BY d.diagnosis_name
ORDER BY total_kasus DESC;
