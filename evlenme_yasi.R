# Gerekli paketlerin y??klenmesi
install.packages("tidyverse")
install.packages("forecast")
install.packages("ggplot2")
install.packages("rmarkdown")
library(readxl)
library(tidyverse)
library(forecast)
library(ggplot2)
library(rmarkdown)
library(readr)
# Dosya yolunu belirtin
evlenme_yasi <- read_excel("C://Users//halil//Desktop//midterm//evlenme_yasi.xlsx")
View(evlenme_yasi)

# ??lk birka?? sat??r?? kontrol et
head(evlenme_yasi)

# Veri ??er??evesi yap??s??n?? kontrol et
str(evlenme_yasi)

# Tekrar g??rselle??tirmeyi deneyelim
ggplot(evlenme_yasi, aes(x = YIL)) +
  geom_line(aes(y = ERKEK_ORTALAMA_EVLENME_YASI, color = "Erkek"), size = 1.2) +
  geom_line(aes(y = KADIN_ORTALAMA_EVLENME_YASI, color = "Kad??n"), size = 1.2) +
  labs(title = "Kad??n ve Erkek Ortalama Evlenme Ya??lar?? (Y??llara G??re)",
       x = "Y??l", y = "Ortalama Evlenme Ya????") +
  scale_color_manual(values = c("Erkek" = "blue", "Kad??n" = "red")) +
  theme_minimal()

# Y??llara g??re toplam evlenme say??s??n?? g??rselle??tirelim
ggplot(evlenme_yasi, aes(x = YIL, y = EVLENME_SAYISI)) +
  geom_line(color = "green", size = 1.2) +
  labs(title = "Y??llara G??re Toplam Evlenme Say??s??",
       x = "Y??l", y = "Toplam Evlenme Say??s??") +
  theme_minimal()





# Veriyi manuel giriyoruz
evlenme_yasi <- tibble::tibble(
  YIL = 2001:2024,
  KADIN_ORTALAMA_EVLENME_YASI = c(
    23.4, 24, 24, 24.1, 24.1, 24.2, 24.3, 24.5, 24.8, 24.9, 25.2, 25.4,
    25.5, 25.6, 25.8, 26.1, 26.2, 26.5, 26.9, 27.1, 27.7, 27.7, 28.2, 28.3
  )
)

kad??n_ts <- ts(evlenme_yasi$KADIN_ORTALAMA_EVLENME_YASI,
               start = min(evlenme_yasi$YIL),
               frequency = 1)

# ARIMA modeliyle tahminleme
kad??n_model <- auto.arima(kad??n_ts)
kad??n_forecast <- forecast(kad??n_model, h = 5)

# Tahmin sonu??lar??n?? g??rselle??tirme
autoplot(kad??n_forecast) +
  labs(title = "Kad??n Ortalama Evlenme Ya???? Tahmini (2025-2029)",
       x = "Y??l", y = "Ya??") +
  theme_minimal()





















# Erkek evlenme ya???? tahmini
erkek_ts <- ts(evlenme_yasi$ERKEK_ORTALAMA_EVLENME_YASI, start = min(evlenme_yasi$YIL), frequency = 1)
erkek_model <- auto.arima(erkek_ts)
erkek_forecast <- forecast(erkek_model, h = 5)
autoplot(erkek_forecast) + labs(title = "Erkek Ortalama Evlenme Ya???? Tahmini (2025-2029)", x = "Y??l", y = "Ya??")

# Toplam evlenme say??s?? tahmini
sayi_ts <- ts(evlenme_yasi$EVLENME_SAYISI, start = min(evlenme_yasi$YIL), frequency = 1)
sayi_model <- auto.arima(sayi_ts)
sayi_forecast <- forecast(sayi_model, h = 5)
autoplot(sayi_forecast) + labs(title = "Toplam Evlenme Say??s?? Tahmini (2025-2029)", x = "Y??l", y = "Ki??i Say??s??")
