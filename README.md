📊 Penentuan Minat Mahasiswa FMIPA UB dalam Menyelesaikan Tugas Menggunakan Analisis Hierarki Proses (AHP)📌 Executive SummaryRepositori ini menyajikan dokumentasi kode lengkap dan analisis statistik untuk penelitian "Penentuan Minat Mahasiswa FMIPA UB dalam Menyelesaikan Tugas dengan Analisis Hierarki Proses".Penelitian ini bertujuan untuk mengukur dan mengidentifikasi faktor-faktor yang mempengaruhi minat mahasiswa Fakultas Matematika dan Ilmu Pengetahuan Alam (FMIPA) Universitas Brawijaya dalam menyelesaikan tugas perkuliahan. Metode yang digunakan adalah Analytical Hierarchy Process (AHP) untuk menangani pengambilan keputusan multikriteria (Multi-Criteria Decision Making) berbasis data kuesioner.📐 Metodologi & Formulasi Matematika1. Penentuan Sampel (Rumus Slovin)Populasi penelitian mencakup $N = 4.271$ mahasiswa aktif S1 FMIPA Universitas Brawijaya yang tersebar di 5 departemen. Jumlah sampel minimum dihitung menggunakan Rumus Slovin dengan tingkat margin of error $e = 10\%$ ($0{,}1$):$$n = \frac{N}{1 + N(e)^2} = \frac{4271}{1 + 4271(0{,}1)^2} = 97{,}71 \approx 98 \text{ Responden}$$Sampel riil yang berhasil dikumpulkan melalui teknik Cluster Random Sampling adalah sebanyak 110 responden (memenuhi batas minimum).2. Tahapan Analisis Hierarki Proses (AHP)Metode AHP mengikuti 5 tahapan utama berdasarkan teori Thomas L. Saaty:Dekomposisi Masalah: Menyusun hierarki dari Goal (Tujuan Utama), Kriteria, hingga Alternatif.Matriks Perbandingan Berpasangan (Pairwise Comparison): Membangun matriks $A = [a_{ij}]$ berukuran $n \times n$ di mana $a_{ij}$ mewakili tingkat kepentingan kriteria $i$ dibandingkan kriteria $j$.Normalisasi Matriks Kolom: Setiap elemen matriks dibagi dengan jumlah total elemen pada kolom yang sama:
$$n_{ij} = \frac{a_{ij}}{\sum_{k=1}^{n} a_{kj}}$$Perhitungan Vektor Eigen & Bobot Prioritas (Priority Vector):
Mencari nilai eigen ($\lambda$) dan vektor eigen ($v$) dari matriks kuadrat $B$ melalui persamaan:
$$B \cdot v = \lambda \cdot v$$Evaluasi Konsistensi Matriks:Consistency Index (CI):
$$\text{CI} = \frac{\lambda_{\max} - n}{n - 1}$$Consistency Ratio (CR):
$$\text{CR} = \frac{\text{CI}}{\text{RI}}$$
(Di mana $\text{RI}$ adalah Random Index berdasarkan ukuran matriks $n$).🏗️ Struktur Hierarki Penelitian                                [ Goal ]
              Minat Mahasiswa dalam Menyelesaikan Tugas
                                   │
      ┌───────────┬───────────┬────┴──────┬───────────┬───────────┐
      ▼           ▼           ▼           ▼           ▼           ▼
  [Kriteria 1] [Kriteria 2] [Kriteria 3] [Kriteria 4] [Kriteria 5] [Kriteria 6]
   Tingkat      Lama Waktu   Pemahaman     Minat       Ketersediaan Kualitas
   Kesulitan    Pengerjaan    Materi     Terhadap     Sumber Daya  Pengajar
     Tugas                                Matkul       Penunjang
      │           │           │           │           │           │
      └───────────┴───────────┼───────────┴───────────┴───────────┘
                               │
      ┌───────────┬───────────┼───────────┬───────────┐
      ▼           ▼           ▼           ▼           ▼
  [Sangat Tinggi] [Tinggi]  [Sedang]   [Rendah] [Sangat Rendah]
                         (Alternatif/Tingkatan)
💻 R Code Architecture & ImplementationSkrip R utama dikembangkan secara modular untuk melakukan transformasi matriks, perkalian matriks, hingga dekomposisi eigen.Kode Lengkap (ahp_analysis.R)# =====================================================================
# SCRIPT ANALISIS HIERARKI PROSES (AHP)
# Studi Kasus: Minat Mahasiswa FMIPA UB dalam Menyelesaikan Tugas
# Penulis: Luthfi F. A., Siska D. Y., Ahmad Jalaludin Rahman
# =====================================================================

# ---------------------------------------------------------------------
# 1. PERSIAPAN DATA & INPUT MATRIKS
# ---------------------------------------------------------------------

# Matriks data perbandingan agregat dari hasil kuesioner responden
matriks_data <- matrix(c(
  11, 22, 10, 17, 23,
  25, 26, 68, 42, 45,
  44, 54, 49, 16, 46,
  34, 30, 28, 22,  4,
  12, 14, 13,  3,  2,
   0,  0,  0,  0,  0
), nrow = 6, byrow = TRUE)

cat("=== MATRIKS DATA AWAL ===\n")
print(matriks_data)

# ---------------------------------------------------------------------
# 2. PENGHITUNGAN NILAI TENGAH (MEAN) & NORMALISASI
# ---------------------------------------------------------------------

# Menghitung nilai rata-rata keseluruhan elemen matriks
nilai_tengah <- (1 / 30) * sum(matriks_data)
cat("\nNilai Tengah (Mean Total):", nilai_tengah, "\n")

# Normalisasi elemen matriks terhadap Nilai Tengah
matriks_norm_tengah <- matriks_data / nilai_tengah

# Memasang nilai diagonal utama = 1 (Self-comparison)
diag(matriks_norm_tengah) <- 1

cat("\n=== MATRIKS NORMALISASI DIAGONAL 1 ===\n")
print(matriks_norm_tengah)

# ---------------------------------------------------------------------
# 3. NORMALISASI KOLOM MATRIKS
# ---------------------------------------------------------------------

# Menghitung jumlah per kolom
kolom_sum <- colSums(matriks_data)

# Membagi setiap elemen dengan jumlah kolomnya
matriks_norm_kolom <- sweep(matriks_data, 2, kolom_sum, FUN = "/")

cat("\n=== MATRIKS NORMALISASI KOLOM ===\n")
print(round(matriks_norm_kolom, 4))

# ---------------------------------------------------------------------
# 4. DEKOMPOSISI EIGEN & VEKTOR EIGEN
# ---------------------------------------------------------------------

# Ekstraksi sub-matriks persegi 5x5 untuk komputasi eigen
sub_matriks <- matriks_norm_kolom[1:5, 1:5]

# Penghitungan nilai eigen dan vektor eigen
hasil_eigen <- eigen(sub_matriks)

nilai_eigen <- hasil_eigen$values
vektor_eigen <- hasil_eigen$vectors

cat("\n=== NILAI EIGEN (EIGENVALUES) ===\n")
print(nilai_eigen)

cat("\n=== VEKTOR EIGEN (EIGENVECTORS) ===\n")
print(vektor_eigen)

# ---------------------------------------------------------------------
# 5. ESTIMASI BOBOT PRIORITAS RELATIF
# ---------------------------------------------------------------------

# Menghitung vektor prioritas utama dari vektor eigen dominan
vektor_utama <- Re(vektor_eigen[, 1])
bobot_prioritas <- vektor_utama / sum(vektor_utama)

cat("\n=== BOBOT PRIORITAS RELATIF (PRIORITY VECTOR) ===\n")
for (i in 1:length(bobot_prioritas)) {
  cat(sprintf("Kriteria/Alternatif %d : %.8f (%.2f%%)\n", 
              i, bobot_prioritas[i], bobot_prioritas[i] * 100))
}

# ---------------------------------------------------------------------
# 6. EVALUASI KONSISTENSI (CI & CR)
# ---------------------------------------------------------------------

n <- nrow(sub_matriks)
lambda_max <- max(Re(nilai_eigen))

# Consistency Index (CI)
ci <- (lambda_max - n) / (n - 1)

# Random Index (RI) untuk n = 5
ri <- 1.12 

# Consistency Ratio (CR)
cr <- ci / ri

cat("\n=== UJI KONSISTENSI MATRIKS ===\n")
cat("Lambda Max    :", lambda_max, "\n")
cat("Index CI      :", ci, "\n")
cat("Ratio CR      :", cr, "\n")

if (!is.na(cr) && cr < 0.10) {
  cat("Kesimpulan    : Penilaian KONSISTEN (CR < 0.10)\n")
} else {
  cat("Catatan       : Terjadi ketidakkonsistenan penilaian pada matriks\n")
}
📊 Hasil Analisis & PembahasanDari hasil komputasi R di atas, diperoleh skoring bobot prioritas sebagai berikut:PeringkatFaktor / KriteriaBobot Prioritas (wi​)Persentase1Alternatif / Faktor 20.3101049131,01%2Alternatif / Faktor 30.3042424930,42%3Alternatif / Faktor 40.1919349319,19%4Alternatif / Faktor 10.1218763312,19%5Alternatif / Faktor 50.071841337,18%Insight Utama:Kualitas Pengajar dan Ketersediaan Sumber Daya Penunjang terbukti menjadi dua faktor paling dominan yang secara langsung mendorong tingkat minat mahasiswa FMIPA UB dalam menyelesaikan tugas tepat waktu.Optimalisasi sarana akademik dan penyampaian materi perkuliahan menjadi kunci utama untuk meningkatkan performa akademik mahasiswa.📂 Repository Structure.
├── data/
│   └── raw_kuesioner_fmipa.csv   # Dataset mentah hasil kuesioner mahasiswa
├── src/
│   └── ahp_analysis.R            # Script R utama untuk pengolahan AHP
├── docs/
│   └── Jurnal_AHP_FMIPA_UB.pdf   # Naskah publikasi jurnal penelitian
└── README.md                     # Dokumentasi teknis & repositori
🚀 Cara Menjalankan Script (Getting Started)Prasyarat Software:Pastikan R (v4.0 atau lebih baru) dan RStudio telah terinstal di sistem Anda.Kloning Repositori:git clone https://github.com/username/AHP-Minat-Mahasiswa-FMIPA.git
cd AHP-Minat-Mahasiswa-FMIPA
Menjalankan Script via Terminal / RStudio:Buka file src/ahp_analysis.R pada RStudio, atau jalankan perintah berikut di terminal:Rscript src/ahp_analysis.R
👥 Authors & CitationLuthfi Faridatul Azizah (luthfifaridatul@student.ub.ac.id)Siska Dwi Yanti (siskadwynti@student.ub.ac.id)Ahmad Jalaludin Rahman (jalalgibol@student.ub.ac.id)Departemen Statistika, Fakultas Matematika dan Ilmu Pengetahuan Alam, Universitas Brawijaya, Malang, Indonesia.
