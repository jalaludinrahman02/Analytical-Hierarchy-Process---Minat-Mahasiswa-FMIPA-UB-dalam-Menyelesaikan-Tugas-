# Matriks data dari nilai-nilai yang diberikan
data_matrix <- matrix(c(
  11, 22, 10, 17, 23, 25,
  26, 68, 42, 45, 44, 54,
  49, 16, 46, 34, 30, 28,
  22, 4, 12, 14, 13, 3,
  2, 0, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)
data_matrix

# Hitung nilai tengah
mid_value <- mean(data_matrix)
mid_value

# Hitung nilai untuk membuat nilai tengah 1 di diagonal utama
n_matrix <- data_matrix / mid_value
n_matrix

# Set nilai diagonal utama menjadi 1
diag(n_matrix) <- 1

# Tampilkan matriks berpasangan AHP
print(n_matrix)

# Hitung rata-rata geometrik dari setiap kolom matriks data
geometric_means <- apply(data_matrix, 2, function(x) prod(x) ^ (1 / length(x)))

# Tampilkan rata-rata geometrik
print("Rata-rata Geometrik:")
print(geometric_means)

# Hitung selisih terbesar antara dua kolom matriks data
max_difference <- max(abs(apply(data_matrix, 2, diff)))

# Tampilkan selisih terbesar
print("Selisih Terbesar:")
print(max_difference)

# Normalisasi matriks
normalize <- function(matrix) {
  n <- nrow(matrix)
  m <- ncol(matrix)
  normalized_matrix <- matrix(0, nrow = n, ncol = m)
  for (i in 1:m) {
    col_sum <- sum(matrix[, i])
    normalized_matrix[, i] <- matrix[, i] / col_sum
  }
  return(normalized_matrix)
}

# Matriks data dari nilai-nilai yang diberikan
data_matrix <- matrix(c(
  11, 22, 10, 17, 23, 25,
  26, 68, 42, 45, 44, 54,
  49, 16, 46, 34, 30, 28,
  22, 4, 12, 14, 13, 3,
  2, 0, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)

# Tampilkan matriks data
print("Matriks Data:")
print(data_matrix)

# Normalisasi matriks
normalized_matrix <- normalize(data_matrix)

# Tampilkan matriks normalisasi
print("Matriks Normalisasi:")
print(normalized_matrix)


# Matriks data dari nilai-nilai yang diberikan
data_matrix <- matrix(c(
  11, 22, 10, 17, 23, 25,
  26, 68, 42, 45, 44, 54,
  49, 16, 46, 34, 30, 28,
  22, 4, 12, 14, 13, 3,
  2, 0, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)

# Normalisasi matriks
normalize <- function(matrix) {
  n <- nrow(matrix)
  m <- ncol(matrix)
  normalized_matrix <- matrix(0, nrow = n, ncol = m)
  for (i in 1:m) {
    col_sum <- sum(matrix[, i])
    normalized_matrix[, i] <- matrix[, i] / col_sum
  }
  return(normalized_matrix)
}

# Normalisasi matriks
normalized_matrix <- normalize(data_matrix)

# Ubah menjadi matriks persegi dengan mengambil bagian atas kiri
square_matrix <- normalized_matrix[1:min(dim(normalized_matrix)), 1:min(dim(normalized_matrix))]

# Hitung nilai eigen
eigen <- eigen(square_matrix)
eigen

# Tampilkan nilai eigen
print("Eigenvalues:")
print(eigen$values)

# Tampilkan vektor eigen
print("Eigenvectors:")
print(eigen$vectors)


# Pilih vektor eigen utama
main_eigenvector <- eigen$vectors[, which.max(eigen$values)]

# Hitung prioritas relatif
relative_priority <- main_eigenvector / sum(main_eigenvector)

# Tampilkan prioritas relatif
print("Prioritas Relatif:")
print(relative_priority)

# Tampilkan nilai eigen
print("Eigenvalues:")
print(eigen$values)

# Jumlah kriteria (dari matriks persegi yang sudah dihitung eigen-nya)
n <- dim(square_matrix)[1]

# Hitung Consistency Index (CI)
CI <- (eigen$values[1] - n) / (n - 1)

# Hitung Random Index (RI) berdasarkan jumlah kriteria (n)
RI_values <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)
RI <- RI_values[n]

# Hitung Consistency Ratio (CR)
CR <- Re(CI / RI)

# Tampilkan hasil
print(paste("Consistency Index (CI):", CI))
print(paste("Random Index (RI):", RI))
print(paste("Consistency Ratio (CR):", CR))

# Tentukan apakah matriks berpasangan konsisten atau tidak berdasarkan nilai CR
if (CR <= 0.1) {
  print("Matriks berpasangan konsisten.")
} else {
  print("Matriks berpasangan tidak konsisten. Perlu revisi.")
}
