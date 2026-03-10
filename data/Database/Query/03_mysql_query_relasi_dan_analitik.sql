-- ==========================================================
-- QUERY RELASI + ANALITIK (MySQL)
-- ==========================================================

USE `db_klinik_normalisasi`;

-- 1) Cek jumlah data per tabel inti 3NF
SELECT 'nf3_clinic' AS table_name, COUNT(*) AS total_rows FROM nf3_clinic
UNION ALL SELECT 'nf3_doctor', COUNT(*) FROM nf3_doctor
UNION ALL SELECT 'nf3_patient', COUNT(*) FROM nf3_patient
UNION ALL SELECT 'nf3_visit', COUNT(*) FROM nf3_visit
UNION ALL SELECT 'nf3_diagnosis', COUNT(*) FROM nf3_diagnosis
UNION ALL SELECT 'nf3_treatment', COUNT(*) FROM nf3_treatment
UNION ALL SELECT 'nf3_medicine', COUNT(*) FROM nf3_medicine
UNION ALL SELECT 'nf3_transactions', COUNT(*) FROM nf3_transactions;

-- 2) Query relasi utama (visit + pasien + dokter + klinik + transaksi)
SELECT
  v.visit_id,
  v.visit_datetime,
  p.patient_name,
  p.gender AS patient_gender,
  d.doctor_name,
  d.doctor_specialty,
  c.clinic_name,
  c.clinic_city,
  c.clinic_province,
  t.payment_method,
  t.total_amount
FROM nf3_visit v
JOIN nf3_patient p ON p.patient_id = v.patient_id
JOIN nf3_doctor d ON d.doctor_id = v.doctor_id
JOIN nf3_clinic c ON c.clinic_id = v.clinic_id
LEFT JOIN nf3_transactions t ON t.visit_id = v.visit_id
ORDER BY v.visit_datetime DESC
LIMIT 100;

-- 3) Detail diagnosis per kunjungan
SELECT
  v.visit_id,
  v.visit_datetime,
  p.patient_name,
  dg.diagnosis_name,
  vd.diagnosis_seq
FROM nf3_visit_diagnosis vd
JOIN nf3_visit v ON v.visit_id = vd.visit_id
JOIN nf3_patient p ON p.patient_id = v.patient_id
JOIN nf3_diagnosis dg ON dg.diagnosis_id = vd.diagnosis_id
ORDER BY v.visit_datetime DESC, vd.diagnosis_seq ASC
LIMIT 200;

-- 4) Detail treatment per kunjungan
SELECT
  v.visit_id,
  v.visit_datetime,
  p.patient_name,
  tr.treatment_name,
  tr.treatment_fee,
  vt.treatment_seq
FROM nf3_visit_treatment vt
JOIN nf3_visit v ON v.visit_id = vt.visit_id
JOIN nf3_patient p ON p.patient_id = v.patient_id
JOIN nf3_treatment tr ON tr.treatment_id = vt.treatment_id
ORDER BY v.visit_datetime DESC, vt.treatment_seq ASC
LIMIT 200;

-- 5) Detail obat per kunjungan
SELECT
  v.visit_id,
  v.visit_datetime,
  p.patient_name,
  m.medicine_name,
  m.medicine_category,
  m.medicine_unit_price,
  m.medicine_dosage_per_day,
  m.medicine_duration_days,
  (m.medicine_unit_price * m.medicine_dosage_per_day * m.medicine_duration_days) AS medicine_subtotal
FROM nf3_visit_medicine vm
JOIN nf3_visit v ON v.visit_id = vm.visit_id
JOIN nf3_patient p ON p.patient_id = v.patient_id
JOIN nf3_medicine m ON m.medicine_id = vm.medicine_id
ORDER BY v.visit_datetime DESC, vm.medicine_seq ASC
LIMIT 200;

-- 6) 10 diagnosis terbanyak
SELECT
  dg.diagnosis_name,
  COUNT(*) AS total_case
FROM nf3_visit_diagnosis vd
JOIN nf3_diagnosis dg ON dg.diagnosis_id = vd.diagnosis_id
GROUP BY dg.diagnosis_name
ORDER BY total_case DESC
LIMIT 10;

-- 7) Pendapatan per klinik per bulan
SELECT
  c.clinic_name,
  DATE_FORMAT(v.visit_datetime, '%Y-%m') AS year_month,
  COUNT(*) AS total_visit,
  SUM(t.total_amount) AS total_revenue
FROM nf3_transactions t
JOIN nf3_visit v ON v.visit_id = t.visit_id
JOIN nf3_clinic c ON c.clinic_id = v.clinic_id
GROUP BY c.clinic_name, DATE_FORMAT(v.visit_datetime, '%Y-%m')
ORDER BY year_month DESC, total_revenue DESC;

-- 8) Dokter dengan jumlah kunjungan terbanyak
SELECT
  d.doctor_name,
  d.doctor_specialty,
  c.clinic_name,
  COUNT(*) AS total_visit
FROM nf3_visit v
JOIN nf3_doctor d ON d.doctor_id = v.doctor_id
JOIN nf3_clinic c ON c.clinic_id = d.clinic_id
GROUP BY d.doctor_name, d.doctor_specialty, c.clinic_name
ORDER BY total_visit DESC
LIMIT 20;

