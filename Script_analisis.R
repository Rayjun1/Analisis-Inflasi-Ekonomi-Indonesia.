# --- 1. MEMUAT LIBRARY ---
library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)

# --- 2. MEMBANGUN DATA FRAME ---
# Data input per triwulan dari tahun 2011 hingga 2023
tahun <- rep(2011:2023, each = 4)
triwulan <- rep(c("I", "II", "III", "IV"), times = 13)
inflasi <- c(6.84, 5.89, 4.67, 4.12, 3.73, 4.49, 4.48, 4.41, 5.26, 5.65, 8.60, 8.36, 
             7.76, 7.09, 4.35, 6.47, 6.54, 7.07, 7.09, 4.83, 4.34, 3.46, 3.02, 3.30, 
             3.64, 4.29, 3.81, 3.50, 3.28, 3.25, 3.09, 3.17, 2.62, 2.81, 3.40, 2.95, 
             2.87, 2.27, 1.43, 1.57, 1.43, 1.48, 1.57, 1.76, 2.29, 3.79, 5.19, 5.55, 
             5.24, 3.95, 2.87, 2.68)
pertumbuhan_ekonomi <- c(6.48, 6.27, 6.01, 5.94, 6.11, 6.21, 5.94, 5.87, 5.54, 
                         5.59, 5.52, 5.58, 5.12, 4.94, 4.93, 5.05, 4.83, 4.74,
                         4.78, 5.15, 4.94, 5.21, 5.03, 4.94, 5.01, 5.01,
                         5.06, 5.19, 5.07, 5.27, 5.17, 5.18, 5.06, 5.05,
                         5.01, 4.96, 2.97, -5.32, -3.49, -2.17, -0.69, 7.08,
                         3.53, 5.03, 5.02, 5.46, 5.73, 5.01, 5.04, 5.17, 4.94, 5.04)

# Gabungkan data ke dalam data frame
data <- data.frame(
  Tahun = tahun,
  Triwulan = triwulan,
  Inflasi = inflasi,
  PertumbuhanEkonomi = pertumbuhan_ekonomi
)

# Membuat kolom YearQuarter untuk keperluan deret waktu
data <- data %>%
  mutate(
    QuarterNum = case_when(
      Triwulan == "I"   ~ 1,
      Triwulan == "II"  ~ 2,
      Triwulan == "III" ~ 3,
      Triwulan == "IV"  ~ 4,
      TRUE ~ NA_real_
    ),
    YearQuarter = as.yearqtr(paste(Tahun, QuarterNum), format = "%Y %q")
  )

# --- 3. PENGGOLONGAN DATA BERDASARKAN TINGKAT INFLASI ---
# Tujuan uji t: Mengetahui apakah perbedaan tingkat inflasi (rendah vs. tinggi)
# berpengaruh terhadap pertumbuhan ekonomi.
# Gunakan median inflasi sebagai batas pengelompokan.
median_inflasi <- median(data$Inflasi)
data <- data %>%
  mutate(InflationGroup = if_else(Inflasi <= median_inflasi, "Low", "High"))

# Tampilkan ringkasan per kelompok
data %>% 
  group_by(InflationGroup) %>% 
  summarise(
    Rata2_Inflasi = mean(Inflasi),
    Rata2_Pertumbuhan = mean(PertumbuhanEkonomi),
    n = n()
  )

# --- 4. ANALISIS DESKRIPTIF ---
summary(data)
str(data)

# --- 5. VISUALISASI DATA ---
## 5.1. Plot deret waktu untuk Inflasi
plot_inflasi <- ggplot(data, aes(x = YearQuarter, y = Inflasi)) +
  geom_line(color = "blue") +
  geom_point(color = "darkblue") +
  labs(title = "Quarterly Inflation (2011-2023)",
       x = "Year (Quarter)",
       y = "Inflation (%)") +
  theme_minimal()

## 5.2. Plot deret waktu untuk Pertumbuhan Ekonomi
plot_pertumbuhan <- ggplot(data, aes(x = YearQuarter, y = PertumbuhanEkonomi)) +
  geom_line(color = "green") +
  geom_point(color = "darkgreen") +
  labs(title = "Quaterly Economic Growth (2011-2023)",
       x = "Year (Quarter)",
       y = "Economic Growth (%)") +
  theme_minimal()

## 5.3. Scatter plot hubungan antara Inflasi dan Pertumbuhan Ekonomi
plot_scatter <- ggplot(data, aes(x = Inflasi, y = PertumbuhanEkonomi)) +
  geom_point(aes(color = InflationGroup), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "The Relationship Between Inflation and Economic Growth",
       x = "Inflation (%)",
       y = "Economic Growth (%)") +
  theme_minimal()

# Tampilkan plot
print(plot_inflasi)
print(plot_pertumbuhan)
print(plot_scatter)

# --- 6. UJI T DUA SAMPEL ---
# Tujuan uji t:
# Mengetahui apakah perbedaan tingkat inflasi (kelompok 'Rendah' vs 'Tinggi')
# berpengaruh pada rata-rata pertumbuhan ekonomi.
# Hipotesis:
#   H0: Rata-rata pertumbuhan ekonomi pada kelompok inflasi rendah = rata-rata pada kelompok inflasi tinggi.
#   H1: Rata-rata pertumbuhan ekonomi pada kelompok inflasi rendah ≠ rata-rata pada kelompok inflasi tinggi.
t_test_result <- t.test(PertumbuhanEkonomi ~ InflationGroup, data = data, conf.level = 0.95)
print(t_test_result)

# --- 7. ANALISIS REGRESI SEDERHANA ---
# Selain uji t, kita juga dapat melihat pengaruh inflasi terhadap pertumbuhan ekonomi
# dengan membangun model regresi sederhana.
reg_model <- lm(PertumbuhanEkonomi ~ Inflasi, data = data)
summary(reg_model)

# --- 8. UJI KORELASI
# Uji korelasi antara inflasi dan pertumbuhan ekonomi
correlation_result <- cor.test(data$Inflasi, data$PertumbuhanEkonomi)
print(correlation_result)

# --- 9. DIAGNOSTIK MODEL REGRESI ---
# Melihat plot diagnostik (residual plot, QQ plot, dsb)
par(mfrow = c(2,2))
plot(reg_model)
par(mfrow = c(1,1))

# Alternatif: Plot residual vs fitted menggunakan ggplot2
ggplot(data, aes(x = reg_model$fitted.values, y = reg_model$residuals)) +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Residual vs Fitted Values",
       x = "Nilai Terprediksi",
       y = "Residual") +
  theme_minimal()

