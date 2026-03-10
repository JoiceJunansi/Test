-- ==========================================================
-- DATABASE + STRUKTUR TABEL NORMALISASI DATASET KLINIK (MySQL)
-- Tahap: 1NF, 2NF, 3NF
-- -- ==========================================================
-- DATABASE + STRUKTUR TABEL NORMALISASI DATASET KLINIK (MySQL)
-- Tahap: 1NF, 2NF, 3NF
-- ==========================================================

CREATE DATABASE IF NOT EXISTS `db_klinik_normalisasi`
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_unicode_ci;
USE `db_klinik_normalisasi`;
-- ======================
-- 1NF
-- ======================
-- 1NF: data sudah atomik (tidak ada array/list dalam satu kolom)

CREATE TABLE IF NOT EXISTS `nf1_visit` (
  `visit_id` VARCHAR(20) NOT NULL,
  `visit_datetime` DATETIME NULL,
  `clinic_name` VARCHAR(200) NULL,
  `clinic_city` VARCHAR(100) NULL,
  `clinic_province` VARCHAR(100) NULL,
  `clinic_open_date` DATE NULL,
  `head_doctor_name_of_clinic` VARCHAR(200) NULL,
  `doctor_name` VARCHAR(200) NULL,
  `doctor_gender` CHAR(1) NULL,
  `doctor_specialty` VARCHAR(100) NULL,
  `patient_name` VARCHAR(200) NULL,
  `gender` CHAR(1) NULL,
  `date_of_birth` DATE NULL,
  `height` SMALLINT NULL,
  `weight` SMALLINT NULL,
  `patient_city` VARCHAR(100) NULL,
  `patient_province` VARCHAR(100) NULL,
  `patient_type` VARCHAR(50) NULL,
  `complaint` TEXT NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  `payment_method` VARCHAR(30) NULL,
  PRIMARY KEY (`visit_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `nf1_visit_line` (
  `visit_id` VARCHAR(20) NOT NULL,
  `line_no` INT NOT NULL,
  `diagnosis_name` VARCHAR(200) NULL,
  `treatment_name` VARCHAR(200) NULL,
  `treatment_fee` DECIMAL(14,2) NULL,
  `medicine_name` VARCHAR(255) NULL,
  `medicine_category` VARCHAR(100) NULL,
  `medicine_unit_price` DECIMAL(14,2) NULL,
  `medicine_dosage_per_day` DECIMAL(10,2) NULL,
  `medicine_duration_days` INT NULL,
  PRIMARY KEY (`visit_id`, `line_no`),
  CONSTRAINT `fk_nf1_visit_line_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf1_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- ======================
-- 2NF
-- ======================
-- 2NF: pisahkan ketergantungan parsial dari data detail kunjungan

CREATE TABLE IF NOT EXISTS `nf2_visit` (
  `visit_id` VARCHAR(20) NOT NULL,
  `visit_datetime` DATETIME NULL,
  `clinic_name` VARCHAR(200) NULL,
  `clinic_city` VARCHAR(100) NULL,
  `clinic_province` VARCHAR(100) NULL,
  `clinic_open_date` DATE NULL,
  `head_doctor_name_of_clinic` VARCHAR(200) NULL,
  `doctor_name` VARCHAR(200) NULL,
  `doctor_gender` CHAR(1) NULL,
  `doctor_specialty` VARCHAR(100) NULL,
  `patient_name` VARCHAR(200) NULL,
  `gender` CHAR(1) NULL,
  `date_of_birth` DATE NULL,
  `height` SMALLINT NULL,
  `weight` SMALLINT NULL,
  `patient_city` VARCHAR(100) NULL,
  `patient_province` VARCHAR(100) NULL,
  `patient_type` VARCHAR(50) NULL,
  `complaint` TEXT NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  `payment_method` VARCHAR(30) NULL,
  PRIMARY KEY (`visit_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `nf2_visit_diagnosis` (
  `visit_id` VARCHAR(20) NOT NULL,
  `diagnosis_seq` INT NOT NULL,
  `diagnosis_name` VARCHAR(200) NOT NULL,
  PRIMARY KEY (`visit_id`, `diagnosis_seq`),
  KEY `idx_nf2_diagnosis_name` (`diagnosis_name`),
  CONSTRAINT `fk_nf2_diagnosis_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf2_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `nf2_visit_treatment` (
  `visit_id` VARCHAR(20) NOT NULL,
  `treatment_seq` INT NOT NULL,
  `treatment_name` VARCHAR(200) NOT NULL,
  `treatment_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`visit_id`, `treatment_seq`),
  KEY `idx_nf2_treatment_name` (`treatment_name`),
  CONSTRAINT `fk_nf2_treatment_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf2_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `nf2_visit_medicine` (
  `visit_id` VARCHAR(20) NOT NULL,
  `medicine_seq` INT NOT NULL,
  `medicine_name` VARCHAR(255) NOT NULL,
  `medicine_category` VARCHAR(100) NULL,
  `medicine_unit_price` DECIMAL(14,2) NULL,
  `medicine_dosage_per_day` DECIMAL(10,2) NULL,
  `medicine_duration_days` INT NULL,
  PRIMARY KEY (`visit_id`, `medicine_seq`),
  KEY `idx_nf2_medicine_name` (`medicine_name`),
  CONSTRAINT `fk_nf2_medicine_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf2_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
-- ======================
-- 3NF
-- ======================
-- 3NF: pisahkan dependensi transitif ke entitas master + relasi

CREATE TABLE IF NOT EXISTS `clinic` (
  `clinic_id` VARCHAR(12) NOT NULL,
  `clinic_name` VARCHAR(200) NOT NULL,
  `clinic_city` VARCHAR(100) NOT NULL,
  `clinic_province` VARCHAR(100) NOT NULL,
  `clinic_open_date` DATE NULL,
  `head_doctor_name_of_clinic` VARCHAR(200) NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`clinic_id`),
  UNIQUE KEY `uk_clinic_natural` (`clinic_name`, `clinic_city`, `clinic_province`, `clinic_open_date`, `head_doctor_name_of_clinic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `doctor` (
  `doctor_id` VARCHAR(12) NOT NULL,
  `clinic_id` VARCHAR(12) NOT NULL,
  `doctor_name` VARCHAR(200) NOT NULL,
  `doctor_gender` CHAR(1) NULL,
  `doctor_specialty` VARCHAR(100) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`doctor_id`),
  UNIQUE KEY `uk_doctor_natural` (`clinic_id`, `doctor_name`, `doctor_gender`, `doctor_specialty`),
  KEY `idx_doctor_clinic` (`clinic_id`),
  CONSTRAINT `fk_doctor_clinic`
    FOREIGN KEY (`clinic_id`) REFERENCES `clinic` (`clinic_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `patient` (
  `patient_id` VARCHAR(14) NOT NULL,
  `patient_name` VARCHAR(200) NOT NULL,
  `gender` CHAR(1) NULL,
  `date_of_birth` DATE NULL,
  `height` SMALLINT NULL,
  `weight` SMALLINT NULL,
  `patient_city` VARCHAR(100) NULL,
  `patient_province` VARCHAR(100) NULL,
  `patient_type` VARCHAR(50) NULL,
  PRIMARY KEY (`patient_id`),
  UNIQUE KEY `uk_patient_natural` (`patient_name`, `gender`, `date_of_birth`, `height`, `weight`, `patient_city`, `patient_province`, `patient_type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `visit` (
  `visit_id` VARCHAR(20) NOT NULL,
  `visit_datetime` DATETIME NULL,
  `clinic_id` VARCHAR(12) NOT NULL,
  `doctor_id` VARCHAR(12) NOT NULL,
  `patient_id` VARCHAR(14) NOT NULL,
  `complaint` TEXT NULL,
  PRIMARY KEY (`visit_id`),
  KEY `idx_visit_datetime` (`visit_datetime`),
  KEY `idx_visit_clinic` (`clinic_id`),
  KEY `idx_visit_doctor` (`doctor_id`),
  KEY `idx_visit_patient` (`patient_id`),
  CONSTRAINT `fk_visit_clinic`
    FOREIGN KEY (`clinic_id`) REFERENCES `clinic` (`clinic_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT `fk_visit_doctor`
    FOREIGN KEY (`doctor_id`) REFERENCES `doctor` (`doctor_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT `fk_visit_patient`
    FOREIGN KEY (`patient_id`) REFERENCES `patient` (`patient_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `diagnosis` (
  `diagnosis_id` VARCHAR(12) NOT NULL,
  `diagnosis_name` VARCHAR(200) NOT NULL,
  PRIMARY KEY (`diagnosis_id`),
  UNIQUE KEY `uk_diagnosis_name` (`diagnosis_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `treatment` (
  `treatment_id` VARCHAR(12) NOT NULL,
  `treatment_name` VARCHAR(200) NOT NULL,
  `treatment_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`treatment_id`),
  UNIQUE KEY `uk_treatment_name_fee` (`treatment_name`, `treatment_fee`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `medicine` (
  `medicine_id` VARCHAR(12) NOT NULL,
  `medicine_name` VARCHAR(255) NOT NULL,
  `medicine_category` VARCHAR(100) NULL,
  `medicine_unit_price` DECIMAL(14,2) NULL,
  `medicine_dosage_per_day` DECIMAL(10,2) NULL,
  `medicine_duration_days` INT NULL,
  PRIMARY KEY (`medicine_id`),
  UNIQUE KEY `uk_medicine_natural` (`medicine_name`, `medicine_category`, `medicine_unit_price`, `medicine_dosage_per_day`, `medicine_duration_days`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `visit_diagnosis` (
  `visit_id` VARCHAR(20) NOT NULL,
  `diagnosis_seq` INT NOT NULL,
  `diagnosis_id` VARCHAR(12) NOT NULL,
  PRIMARY KEY (`visit_id`, `diagnosis_seq`),
  KEY `idx_visit_diagnosis_diag` (`diagnosis_id`),
  CONSTRAINT `fk_visit_diagnosis_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT `fk_visit_diagnosis_diagnosis`
    FOREIGN KEY (`diagnosis_id`) REFERENCES `diagnosis` (`diagnosis_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `visit_treatment` (
  `visit_id` VARCHAR(20) NOT NULL,
  `treatment_seq` INT NOT NULL,
  `treatment_id` VARCHAR(12) NOT NULL,
  PRIMARY KEY (`visit_id`, `treatment_seq`),
  KEY `idx_visit_treatment_treatment` (`treatment_id`),
  CONSTRAINT `fk_visit_treatment_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT `fk_visit_treatment_treatment`
    FOREIGN KEY (`treatment_id`) REFERENCES `treatment` (`treatment_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `visit_medicine` (
  `visit_id` VARCHAR(20) NOT NULL,
  `medicine_seq` INT NOT NULL,
  `medicine_id` VARCHAR(12) NOT NULL,
  PRIMARY KEY (`visit_id`, `medicine_seq`),
  KEY `idx_visit_medicine_medicine` (`medicine_id`),
  CONSTRAINT `fk_visit_medicine_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT `fk_visit_medicine_medicine`
    FOREIGN KEY (`medicine_id`) REFERENCES `medicine` (`medicine_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
CREATE TABLE IF NOT EXISTS `transactions` (
  `transaction_id` BIGINT NOT NULL,
  `transaction_number` VARCHAR(30) NOT NULL,
  `visit_id` VARCHAR(20) NOT NULL,
  `primary_diagnosis_id` VARCHAR(12) NULL,
  `payment_method` VARCHAR(30) NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  `treatment_total` DECIMAL(14,2) NULL,
  `medicine_total` DECIMAL(14,2) NULL,
  `total_amount` DECIMAL(14,2) NULL,
  `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`transaction_id`),
  UNIQUE KEY `uk_transactions_number` (`transaction_number`),
  UNIQUE KEY `uk_transactions_visit` (`visit_id`),
  KEY `idx_transactions_diagnosis` (`primary_diagnosis_id`),
  CONSTRAINT `fk_transactions_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT `fk_transactions_diagnosis`
    FOREIGN KEY (`primary_diagnosis_id`) REFERENCES `diagnosis` (`diagnosis_id`)
    ON UPDATE CASCADE ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;==========================================================

CREATE DATABASE IF NOT EXISTS `db_klinik_normalisasi`
  CHARACTER SET utf8mb4
  COLLATE utf8mb4_unicode_ci;

USE `db_klinik_normalisasi`;

-- ======================
-- 1NF
-- ======================
-- 1NF: data sudah atomik (tidak ada array/list dalam satu kolom)

CREATE TABLE IF NOT EXISTS `nf1_visit` (
  `visit_id` VARCHAR(20) NOT NULL,
  `visit_datetime` DATETIME NULL,
  `clinic_name` VARCHAR(200) NULL,
  `clinic_city` VARCHAR(100) NULL,
  `clinic_province` VARCHAR(100) NULL,
  `clinic_open_date` DATE NULL,
  `head_doctor_name_of_clinic` VARCHAR(200) NULL,
  `doctor_name` VARCHAR(200) NULL,
  `doctor_gender` CHAR(1) NULL,
  `doctor_specialty` VARCHAR(100) NULL,
  `patient_name` VARCHAR(200) NULL,
  `gender` CHAR(1) NULL,
  `date_of_birth` DATE NULL,
  `height` SMALLINT NULL,
  `weight` SMALLINT NULL,
  `patient_city` VARCHAR(100) NULL,
  `patient_province` VARCHAR(100) NULL,
  `patient_type` VARCHAR(50) NULL,
  `complaint` TEXT NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  `payment_method` VARCHAR(30) NULL,
  PRIMARY KEY (`visit_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `nf1_visit_line` (
  `visit_id` VARCHAR(20) NOT NULL,
  `line_no` INT NOT NULL,
  `diagnosis_name` VARCHAR(200) NULL,
  `treatment_name` VARCHAR(200) NULL,
  `treatment_fee` DECIMAL(14,2) NULL,
  `medicine_name` VARCHAR(255) NULL,
  `medicine_category` VARCHAR(100) NULL,
  `medicine_unit_price` DECIMAL(14,2) NULL,
  `medicine_dosage_per_day` DECIMAL(10,2) NULL,
  `medicine_duration_days` INT NULL,
  PRIMARY KEY (`visit_id`, `line_no`),
  CONSTRAINT `fk_nf1_visit_line_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf1_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- ======================
-- 2NF
-- ======================
-- 2NF: pisahkan ketergantungan parsial dari data detail kunjungan

CREATE TABLE IF NOT EXISTS `nf2_visit` (
  `visit_id` VARCHAR(20) NOT NULL,
  `visit_datetime` DATETIME NULL,
  `clinic_name` VARCHAR(200) NULL,
  `clinic_city` VARCHAR(100) NULL,
  `clinic_province` VARCHAR(100) NULL,
  `clinic_open_date` DATE NULL,
  `head_doctor_name_of_clinic` VARCHAR(200) NULL,
  `doctor_name` VARCHAR(200) NULL,
  `doctor_gender` CHAR(1) NULL,
  `doctor_specialty` VARCHAR(100) NULL,
  `patient_name` VARCHAR(200) NULL,
  `gender` CHAR(1) NULL,
  `date_of_birth` DATE NULL,
  `height` SMALLINT NULL,
  `weight` SMALLINT NULL,
  `patient_city` VARCHAR(100) NULL,
  `patient_province` VARCHAR(100) NULL,
  `patient_type` VARCHAR(50) NULL,
  `complaint` TEXT NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  `payment_method` VARCHAR(30) NULL,
  PRIMARY KEY (`visit_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `nf2_visit_diagnosis` (
  `visit_id` VARCHAR(20) NOT NULL,
  `diagnosis_seq` INT NOT NULL,
  `diagnosis_name` VARCHAR(200) NOT NULL,
  PRIMARY KEY (`visit_id`, `diagnosis_seq`),
  KEY `idx_nf2_diagnosis_name` (`diagnosis_name`),
  CONSTRAINT `fk_nf2_diagnosis_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf2_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `nf2_visit_treatment` (
  `visit_id` VARCHAR(20) NOT NULL,
  `treatment_seq` INT NOT NULL,
  `treatment_name` VARCHAR(200) NOT NULL,
  `treatment_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`visit_id`, `treatment_seq`),
  KEY `idx_nf2_treatment_name` (`treatment_name`),
  CONSTRAINT `fk_nf2_treatment_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf2_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `nf2_visit_medicine` (
  `visit_id` VARCHAR(20) NOT NULL,
  `medicine_seq` INT NOT NULL,
  `medicine_name` VARCHAR(255) NOT NULL,
  `medicine_category` VARCHAR(100) NULL,
  `medicine_unit_price` DECIMAL(14,2) NULL,
  `medicine_dosage_per_day` DECIMAL(10,2) NULL,
  `medicine_duration_days` INT NULL,
  PRIMARY KEY (`visit_id`, `medicine_seq`),
  KEY `idx_nf2_medicine_name` (`medicine_name`),
  CONSTRAINT `fk_nf2_medicine_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `nf2_visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- ======================
-- 3NF
-- ======================
-- 3NF: pisahkan dependensi transitif ke entitas master + relasi

CREATE TABLE IF NOT EXISTS `clinic` (
  `clinic_id` VARCHAR(12) NOT NULL,
  `clinic_name` VARCHAR(200) NOT NULL,
  `clinic_city` VARCHAR(100) NOT NULL,
  `clinic_province` VARCHAR(100) NOT NULL,
  `clinic_open_date` DATE NULL,
  `head_doctor_name_of_clinic` VARCHAR(200) NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`clinic_id`),
  UNIQUE KEY `uk_clinic_natural` (`clinic_name`, `clinic_city`, `clinic_province`, `clinic_open_date`, `head_doctor_name_of_clinic`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `doctor` (
  `doctor_id` VARCHAR(12) NOT NULL,
  `clinic_id` VARCHAR(12) NOT NULL,
  `doctor_name` VARCHAR(200) NOT NULL,
  `doctor_gender` CHAR(1) NULL,
  `doctor_specialty` VARCHAR(100) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`doctor_id`),
  UNIQUE KEY `uk_doctor_natural` (`clinic_id`, `doctor_name`, `doctor_gender`, `doctor_specialty`),
  KEY `idx_doctor_clinic` (`clinic_id`),
  CONSTRAINT `fk_doctor_clinic`
    FOREIGN KEY (`clinic_id`) REFERENCES `clinic` (`clinic_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `patient` (
  `patient_id` VARCHAR(14) NOT NULL,
  `patient_name` VARCHAR(200) NOT NULL,
  `gender` CHAR(1) NULL,
  `date_of_birth` DATE NULL,
  `height` SMALLINT NULL,
  `weight` SMALLINT NULL,
  `patient_city` VARCHAR(100) NULL,
  `patient_province` VARCHAR(100) NULL,
  `patient_type` VARCHAR(50) NULL,
  PRIMARY KEY (`patient_id`),
  UNIQUE KEY `uk_patient_natural` (`patient_name`, `gender`, `date_of_birth`, `height`, `weight`, `patient_city`, `patient_province`, `patient_type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `visit` (
  `visit_id` VARCHAR(20) NOT NULL,
  `visit_datetime` DATETIME NULL,
  `clinic_id` VARCHAR(12) NOT NULL,
  `doctor_id` VARCHAR(12) NOT NULL,
  `patient_id` VARCHAR(14) NOT NULL,
  `complaint` TEXT NULL,
  PRIMARY KEY (`visit_id`),
  KEY `idx_visit_datetime` (`visit_datetime`),
  KEY `idx_visit_clinic` (`clinic_id`),
  KEY `idx_visit_doctor` (`doctor_id`),
  KEY `idx_visit_patient` (`patient_id`),
  CONSTRAINT `fk_visit_clinic`
    FOREIGN KEY (`clinic_id`) REFERENCES `clinic` (`clinic_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT `fk_visit_doctor`
    FOREIGN KEY (`doctor_id`) REFERENCES `doctor` (`doctor_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT `fk_visit_patient`
    FOREIGN KEY (`patient_id`) REFERENCES `patient` (`patient_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `diagnosis` (
  `diagnosis_id` VARCHAR(12) NOT NULL,
  `diagnosis_name` VARCHAR(200) NOT NULL,
  PRIMARY KEY (`diagnosis_id`),
  UNIQUE KEY `uk_diagnosis_name` (`diagnosis_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `treatment` (
  `treatment_id` VARCHAR(12) NOT NULL,
  `treatment_name` VARCHAR(200) NOT NULL,
  `treatment_fee` DECIMAL(14,2) NULL,
  PRIMARY KEY (`treatment_id`),
  UNIQUE KEY `uk_treatment_name_fee` (`treatment_name`, `treatment_fee`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `medicine` (
  `medicine_id` VARCHAR(12) NOT NULL,
  `medicine_name` VARCHAR(255) NOT NULL,
  `medicine_category` VARCHAR(100) NULL,
  `medicine_unit_price` DECIMAL(14,2) NULL,
  `medicine_dosage_per_day` DECIMAL(10,2) NULL,
  `medicine_duration_days` INT NULL,
  PRIMARY KEY (`medicine_id`),
  UNIQUE KEY `uk_medicine_natural` (`medicine_name`, `medicine_category`, `medicine_unit_price`, `medicine_dosage_per_day`, `medicine_duration_days`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `visit_diagnosis` (
  `visit_id` VARCHAR(20) NOT NULL,
  `diagnosis_seq` INT NOT NULL,
  `diagnosis_id` VARCHAR(12) NOT NULL,
  PRIMARY KEY (`visit_id`, `diagnosis_seq`),
  KEY `idx_visit_diagnosis_diag` (`diagnosis_id`),
  CONSTRAINT `fk_visit_diagnosis_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT `fk_visit_diagnosis_diagnosis`
    FOREIGN KEY (`diagnosis_id`) REFERENCES `diagnosis` (`diagnosis_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `visit_treatment` (
  `visit_id` VARCHAR(20) NOT NULL,
  `treatment_seq` INT NOT NULL,
  `treatment_id` VARCHAR(12) NOT NULL,
  PRIMARY KEY (`visit_id`, `treatment_seq`),
  KEY `idx_visit_treatment_treatment` (`treatment_id`),
  CONSTRAINT `fk_visit_treatment_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT `fk_visit_treatment_treatment`
    FOREIGN KEY (`treatment_id`) REFERENCES `treatment` (`treatment_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `visit_medicine` (
  `visit_id` VARCHAR(20) NOT NULL,
  `medicine_seq` INT NOT NULL,
  `medicine_id` VARCHAR(12) NOT NULL,
  PRIMARY KEY (`visit_id`, `medicine_seq`),
  KEY `idx_visit_medicine_medicine` (`medicine_id`),
  CONSTRAINT `fk_visit_medicine_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT `fk_visit_medicine_medicine`
    FOREIGN KEY (`medicine_id`) REFERENCES `medicine` (`medicine_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS `transactions` (
  `transaction_id` BIGINT NOT NULL,
  `transaction_number` VARCHAR(30) NOT NULL,
  `visit_id` VARCHAR(20) NOT NULL,
  `primary_diagnosis_id` VARCHAR(12) NULL,
  `payment_method` VARCHAR(30) NULL,
  `administration_fee` DECIMAL(14,2) NULL,
  `doctor_consultation_fee` DECIMAL(14,2) NULL,
  `treatment_total` DECIMAL(14,2) NULL,
  `medicine_total` DECIMAL(14,2) NULL,
  `total_amount` DECIMAL(14,2) NULL,
  `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`transaction_id`),
  UNIQUE KEY `uk_transactions_number` (`transaction_number`),
  UNIQUE KEY `uk_transactions_visit` (`visit_id`),
  KEY `idx_transactions_diagnosis` (`primary_diagnosis_id`),
  CONSTRAINT `fk_transactions_visit`
    FOREIGN KEY (`visit_id`) REFERENCES `visit` (`visit_id`)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT `fk_transactions_diagnosis`
    FOREIGN KEY (`primary_diagnosis_id`) REFERENCES `diagnosis` (`diagnosis_id`)
    ON UPDATE CASCADE ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

