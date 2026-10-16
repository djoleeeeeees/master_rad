
## ---------------------------------------------------------------------------
## ANALIZA

## wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
## setwd(wd)

## ---------------------------------------------------------------------------
## 1) Paketi

paketi <- c("dplyr", "tidyr", "forcats", "readxl","sf", "spdep", "mapview", "leafpop", "ggplot2", "viridis", "RColorBrewer", "ggcorrplot", "ranger", "xgboost", "Metrics", "leaflet" ,"htmlwidgets", "webshot", "patchwork")
novi <- paketi[!(paketi %in% installed.packages()[,"Package"])]
if(length(novi)) install.packages(novi)
invisible(lapply(paketi, library, character.only = TRUE))

if(!dir.exists("slike")) dir.create("slike")
if(!dir.exists("exceli")) dir.create("exceli")
if(!dir.exists("web")) dir.create("web")

set.seed(123) ## fiksiranje vrednosti
CRS_UTM <- 32634   # EPSG:32634 = WGS84 / UTM zone 34N (Beograd)

beograd_granice <- st_read("beograd_granice.shp")

## ---------------------------------------------------------------------------
## 2. Učitavanje

stanovi_ag <- read_excel("stanovi_filtrirano_ag.xlsx")
stanovi_ag <- as.data.frame(stanovi_ag)

## ---------------------------------------------------------------------------
## 3. Deskriptivna analiza

summary(stanovi_ag[, c("Cena","Cena_m2","Kvadratura","Broj_soba")])

   # 3.1 Distribucija cene po m2
p_hist <- ggplot(stanovi_ag, aes(Cena_m2)) +
  geom_histogram(bins = 60, fill = "#2c7fb8") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribucija cene stanova", x = "Cena po m²(€)", y = "Broj oglašenih stanova") +
  theme_minimal()
p_hist
ggsave("slike/hist_cena.png", p_hist, width = 8, height = 5, dpi = 150)

   # 3.2 Cena po m2 po opštinama boxplot po median
p_box <- stanovi_ag %>%
  mutate(Opština = fct_reorder(Opština, Cena_m2, median)) %>%
  ggplot(aes(Opština, Cena_m2)) +
  geom_boxplot(outlier.alpha = 0.2, fill = "#7fcdbb") +
  coord_flip() +
  labs(title = "Cena po m² po opštinama", x = NULL, y = "Cena po m² (€)") +
  theme_minimal()
p_box
ggsave("slike/box_opstine.png", p_box, width = 8, height = 6, dpi = 150)

   # 3.3 Medijana cene/m2 po opštini
tab_opstine <- stanovi_ag %>%
  group_by(Opština) %>%
  summarise(n = n(), med_cena_m2 = median(Cena_m2), .groups = "drop") %>%
  arrange(desc(med_cena_m2))
print(tab_opstine, n = Inf)

## ---------------------------------------------------------------------------

## 4. Prostorna analiza

   # 4.1 Konverzija u sf objekat
sf_df <- st_as_sf(stanovi_ag, coords = c("Y","X"), crs = 32634)

   # 4.2 Karta prostorne raspodele cene/m2
p_karta <- ggplot() +
  geom_sf(data = sf_df, aes(color = Cena_m2), size = 1, alpha = 0.8) +
  geom_sf(data = beograd_granice, fill = NA, color = "red", size = 0.3) +
  scale_color_viridis(
    option = "plasma", 
    trans = "log10",
    name = "Cena (€) / m²",
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(title = "Prostorna raspodela cene stanova po m²") +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()
p_karta
ggsave("slike/karta_cena_m2.png", plot = p_karta, width = 9, height = 7, dpi = 150)

# 4.2.1. Karta prostorne raspodele cene/m2 UVEĆANO
centar_cenam2 <- st_bbox(c(
  xmin = 442000, ymin = 4950000, 
  xmax = 468000, ymax = 4968000
), crs = 32634)
uvecano_cenam2 <- st_crop(sf_df, centar_cenam2)
uvecano_cenam2 <- ggplot() +
  geom_sf(data = uvecano_cenam2, aes(color = Cena_m2), size = 1, alpha = 0.8) +
  geom_sf(data = beograd_granice, fill = NA, color = "red", size = 0.3) +
  scale_color_viridis(
    option = "plasma", 
    trans = "log10",
    name = "Cena (€) / m²",
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  coord_sf(
    xlim = c(442000, 468000), 
    ylim = c(4950000, 4968000), 
    expand = FALSE, 
    datum = 32634
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()
uvecano_cenam2
ggsave("slike/uvecano_cenam2.png", plot = uvecano_cenam2, width = 9, height = 7, dpi = 150)

   # 4.3 Globalni Moran's I – prostorna autokorelacija cene
coords  <- st_coordinates(sf_df)
knn8    <- knearneigh(coords, k = 8) # 8 najbližih suseda
nb      <- knn2nb(knn8)
listw   <- nb2listw(nb, style = "W")

moran_cena <- moran.test(sf_df$Cena_m2, listw, zero.policy = TRUE)
moran_cena

   # 4.4 Interaktivna karta
sf_df_wgs <- sf_df %>%
  mutate(
    Kategorija_cene = cut(
      Cena_m2, 
      breaks = c(-Inf, 2000, 3000, 4000, 5000, Inf),
      labels = c("До 2000 €", "2001 - 3000 €", "3001 - 4000 €", "4001 - 5000 €", "Preko 5000 €")
    )
  ) %>%
  st_transform(crs = 4326)

custom_popup_karta <- paste0(
  "<div style='line-height: 2;'>",
  "<b>Šifra oglasa: </b>", sf_df_wgs$Šifra_oglasa, "<br>",
  "<b>Opština: </b>", sf_df_wgs$Opština, "<br>",
  "<b>Naselje: </b>", sf_df_wgs$Naselje, "<br>",
  "<b>Ulica: </b>", sf_df_wgs$Ulica, "<br>",
  "<b>Cena: </b>", format(round(sf_df_wgs$Cena), big.mark = "."), " €<br>",
  "<b>Kvadratura: </b>", sf_df_wgs$Kvadratura, " m²<br>",
  "<b>Cena po m²: </b>", format(round(sf_df_wgs$Cena_m2), big.mark = "."), " €",
  "</div>"
)

interakt_karta <- mapview(
  sf_df_wgs, 
  zcol = "Kategorija_cene",
  layer.name = "Cena po m² (€)",
  col.regions = brewer.pal(5, "YlOrRd"),
  alpha.regions = 0.8,
  cex = 4,
  popup = custom_popup_karta,
  map.types = c("OpenTopoMap","Esri.WorldImagery","OpenStreetMap")
)
interakt_karta
mapshot(interakt_karta, file = normalizePath("web/interakt_karta.html", mustWork = FALSE))
saveWidget(interakt_karta@map, "web/interakt_karta.html", selfcontained = TRUE)

    # 4.5. Dijagrami rasejanja
BLUE <- "#2c7fb8"
RED  <- "#b2182b"

stanovi_ag <- stanovi_ag %>%
  mutate(
    km_centar = rast_centar_ag / 1000,
    km_muzeji_pozorista = rast_muzeji_pozorista_ag / 1000,
    km_vrtici = rast_vrtici_ag / 1000,
    km_skole = rast_skole_ag / 1000,
    km_verski_objekti = rast_verski_objekti_ag / 1000,
    km_stajalista = rast_stajalista_ag / 1000,
    km_parkovi  = rast_parkovi_ag / 1000,
    km_sume = rast_sume_ag / 1000,
    km_vode = rast_vode_ag / 1000
  )

trend_centar <- stanovi_ag %>%
  mutate(pojas = cut(km_centar, breaks = seq(min(km_centar, na.rm = TRUE), max(km_centar, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_centar, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p1 <- ggplot(stanovi_ag, aes(km_centar, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_centar, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od centra",
       x = "Udaljenost od centra (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p1
ggsave("slike/01_rastojanje_centar.png", plot = p1, width = 9, height = 5.5, dpi = 140)

trend_muzeji_pozorista <- stanovi_ag %>%
  mutate(pojas = cut(km_muzeji_pozorista, breaks = seq(min(km_muzeji_pozorista, na.rm = TRUE), max(km_muzeji_pozorista, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_muzeji_pozorista, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p2 <- ggplot(stanovi_ag, aes(km_muzeji_pozorista, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_muzeji_pozorista, aes(x, y), color = RED, linewidth = 1.3)+
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od ustanova kulture",
       x = "Udaljenost od muzeja ili pozorišta (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p2
ggsave("slike/02_rastojanje_muzeji_pozorista.png", plot = p2, width = 9, height = 5.5, dpi = 140)

trend_vrtici <- stanovi_ag %>%
  mutate(pojas = cut(km_vrtici, breaks = seq(min(km_vrtici, na.rm = TRUE), max(km_vrtici, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_vrtici, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p3 <- ggplot(stanovi_ag, aes(km_vrtici, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_vrtici, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od vrtića",
       x = "Udaljenost od vrtića (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p3
ggsave("slike/03_rastojanje_vrtici.png", plot = p3, width = 9, height = 5.5, dpi = 140)

trend_skole <- stanovi_ag %>%
  mutate(pojas = cut(km_skole, breaks = seq(min(km_skole, na.rm = TRUE), max(km_skole, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_skole, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p4 <- ggplot(stanovi_ag, aes(km_skole, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_skole, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od škola",
       x = "Udaljenost od škola (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p4
ggsave("slike/04_rastojanje_skole.png", plot = p4, width = 9, height = 5.5, dpi = 140)

trend_verski_objekti <- stanovi_ag %>%
  mutate(pojas = cut(km_verski_objekti, breaks = seq(min(km_verski_objekti, na.rm = TRUE), max(km_verski_objekti, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_verski_objekti, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p5 <- ggplot(stanovi_ag, aes(km_verski_objekti, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_verski_objekti, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od verskih objekata",
       x = "Udaljenost od verskih objekata (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p5
ggsave("slike/05_rastojanje_verski_objekti.png", plot = p5, width = 9, height = 5.5, dpi = 140)

trend_stajalista <- stanovi_ag %>%
  mutate(pojas = cut(km_stajalista, breaks = seq(min(km_stajalista, na.rm = TRUE), max(km_stajalista, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_stajalista, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p6 <- ggplot(stanovi_ag, aes(km_stajalista, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_stajalista, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od stajališta",
       x = "Udaljenost od stajališta (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p6
ggsave("slike/06_rastojanje_stajalista.png", plot = p6, width = 9, height = 5.5, dpi = 140)

trend_parkovi <- stanovi_ag %>%
  mutate(pojas = cut(km_parkovi, breaks = seq(min(km_parkovi, na.rm = TRUE), max(km_parkovi, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_parkovi, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p7 <- ggplot(stanovi_ag, aes(km_parkovi, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_parkovi, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od parkova",
       x = "Udaljenost od parkova (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p7
ggsave("slike/07_rastojanje_parkovi.png", plot = p7, width = 9, height = 5.5, dpi = 140)

trend_sume <- stanovi_ag %>%
  mutate(pojas = cut(km_sume, breaks = seq(min(km_sume, na.rm = TRUE), max(km_sume, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_sume, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p8 <- ggplot(stanovi_ag, aes(km_sume, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_sume, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od šuma",
       x = "Udaljenost od šuma (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p8
ggsave("slike/08_rastojanje_sume.png", plot = p8, width = 9, height = 5.5, dpi = 140)

trend_vode <- stanovi_ag %>%
  mutate(pojas = cut(km_vode, breaks = seq(min(km_vode, na.rm = TRUE), max(km_vode, na.rm = TRUE), length.out = 26))) %>%
  group_by(pojas) %>%
  summarise(x = mean(km_vode, na.rm = TRUE), y = median(Cena_m2, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 5)

p9 <- ggplot(stanovi_ag, aes(km_vode, Cena_m2)) +
  geom_point(size = 1.5, alpha = 0.2, color = BLUE) +
  geom_line(data = trend_vode, aes(x, y), color = RED, linewidth = 1.3) +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Cena po m² u odnosu na udaljenost od vode",
       x = "Udaljenost od vodenih površina (km)", y = "Cena po m²") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank())
p9
ggsave("slike/09_rastojanje_vode.png", plot = p9, width = 9, height = 5.5, dpi = 140)

# svih 9 dijagrama
p_svi <- 
  (p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p7 | p8 | p9)
p_svi
ggsave(
  "slike/svi_dijag.png",
  plot = p_svi,
  width = 14,
  height = 10,
  dpi = 300
)
   # 4.6. Spearmanova korelacija ( CENA M2 VS RASTOJANJA X 9)

vars_korelacija <- stanovi_ag %>%
  select(
    Cena_m2,
    km_centar,
    km_muzeji_pozorista,
    km_parkovi,
    km_skole,
    km_stajalista,
    km_sume,
    km_verski_objekti,
    km_vode,
    km_vrtici
  )

cor_matrix <- cor(vars_korelacija, use = "complete.obs", method = "spearman")

sp_corr <- ggcorrplot(
  cor_matrix,
  hc.order = FALSE,
  type = "full",
  lab = TRUE,
  lab_size = 3.2,
  method = "square",
  outline.col = "white",
  colors = c("#4575b4", "#f7f7f7", "#d73027"),
  title = "Spirmanova korelacija - Cena po m² i lokacijskе karakteristike",
  ggtheme = theme_minimal()
) +
  labs(fill = "ρ") +
  scale_y_discrete(limits = rev(colnames(cor_matrix))) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold")
  )

print(sp_corr)

ggsave("slike/sp_corr.png", plot = sp_corr, width = 9.5, height = 7.5, dpi = 150)

## ---------------------------------------------------------------------------
## 5. Kreiranje modela

# 5.1 Definisanje prediktora
prom_stan <- c("Kvadratura","Broj_soba","Tip_objekta","Stanje_objekta", 
               "Grejanje","Sprat","Ukupna_spratnost",
               "Uknjižen","Nije_poslednji_sprat","Nije_stan_u_kući",
               "Salonski","Duplex","Penthouse","Pod_hipotekom","Energetski_pasoš",
               "Potkrovlje", "Terasa","Francuski_balkon","Lođa",
               "Klima","Lift","Podrum","Topla_voda","Telefon","KATV","Internet",
               "Interfon","Video_nadzor","Kamin","Garaža","Parking","Sa_baštom"
) # karakteristike stana

prom_lok <- c("Y","X", "Opština", "Naselje","Ulica", grep("^rast_.*_ag$", names(stanovi_ag), value = TRUE))

prom_stan <- intersect(prom_stan, names(stanovi_ag))
prom_lok  <- intersect(prom_lok,  names(stanovi_ag))
prom_stan
prom_lok

# 5.2. Priprema radnog skupa podataka
podaci <- stanovi_ag
podaci$y_cena <- podaci$Cena_m2

# 5.3. Fja za Ranger
cv_ranger <- function(prediktori, folds){  
  form <- as.formula(
    paste("y_cena ~", paste(sprintf("%s", prediktori), collapse = " + "))
  )
  pred <- numeric(nrow(podaci))  
  for(f in sort(unique(folds))){  
    tr <- which(folds != f);  
    te <- which(folds == f)  
    m <- ranger(form, data = podaci[tr, ],  
                num.trees = 500, min.node.size = 3, importance = "impurity", seed = 123)  
    pred[te] <- predict(m, podaci[te, ])$predictions  
  }  
  pred
}

# 5.4. Fja za metrike
metrike <- function(y_true, pred_val){
  r2   <- 1 - sum((y_true - pred_val)^2) / sum((y_true - mean(y_true))^2)
  data.frame(
    r2       = round(r2, 3),
    rmse = round(rmse(y_true, pred_val)),
    mae  = round(mae(y_true, pred_val))
  )
}

set.seed(123)

# 5.5. Slučajna kros validacija
n <- nrow(podaci)
fold_rnd <- sample(rep(1:5, length.out = n))

pred_stan_rnd <- cv_ranger(prom_stan, fold_rnd)
pred_lok_rnd <- cv_ranger(prom_lok, fold_rnd)
pred_all_rnd <- cv_ranger(c(prom_stan, prom_lok), fold_rnd)

rez_rnd <- rbind(
  cbind(Model = "Samo stan", metrike(podaci$y_cena, pred_stan_rnd)),
  cbind(Model = "Samo lokacija", metrike(podaci$y_cena, pred_lok_rnd)),
  cbind(Model = "Stan + lokacija", metrike(podaci$y_cena, pred_all_rnd))
)

print(rez_rnd)

# 5.6. Prostorna unakrsna validacija

bx <- cut(podaci$Y, quantile(podaci$Y, seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
by <- cut(podaci$X, quantile(podaci$X, seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
blok <- interaction(bx, by, drop = TRUE)

set.seed(123)
mapa_blok <- setNames(sample(rep(1:5, length.out = nlevels(blok))), levels(blok))
fold_sp   <- mapa_blok[as.character(blok)]

pred_stan_sp <- cv_ranger(prom_stan, fold_sp)
pred_lok_sp <- cv_ranger(prom_lok, fold_sp)
pred_all_sp <- cv_ranger(c(prom_stan, prom_lok), fold_sp)

rez_sp <- rbind(
  cbind(Model = "Samo stan", metrike(podaci$y_cena, pred_stan_sp)),
  cbind(Model = "Samo lokacija", metrike(podaci$y_cena, pred_lok_sp)),
  cbind(Model = "Stan + lokacija", metrike(podaci$y_cena, pred_all_sp))
)
print(rez_sp)

# 5.7. XGBoost algoritam Prostorne CV

X_stan <- model.matrix(~ . - 1, data = podaci[, prom_stan], na.action = pass)
X_lok  <- model.matrix(~ . - 1, data = podaci[, prom_lok],  na.action = pass)
X_all  <- model.matrix(~ . - 1, data = podaci[, c(prom_stan, prom_lok)], na.action = pass)

y <- podaci$y_cena

cv_xgboost <- function(X, y, folds) {
  pred <- numeric(nrow(X))
  params <- list(
    max_depth = 6,
    learning_rate = 0.05,
    subsample = 0.8,
    objective = "reg:squarederror",
    seed = 123
  )
  for(f in sort(unique(folds))) {
    tr <- which(folds != f)
    te <- which(folds == f)
    dtr <- xgb.DMatrix(data = X[tr, ], label = y[tr])
    dte <- xgb.DMatrix(data = X[te, ], label = y[te])
    m_xgb <- xgb.train(
      params = params,
      data = dtr,
      nrounds = 400,
      verbose = 0
    )
    pred[te] <- predict(m_xgb, dte)
  }
  pred
}

pred_stan_sp_xgb <- cv_xgboost(X_stan, y, fold_sp)
pred_lok_sp_xgb  <- cv_xgboost(X_lok,  y, fold_sp)
pred_all_sp_xgb  <- cv_xgboost(X_all,  y, fold_sp)

rez_xgb_sp <- rbind(
  cbind(Model = "Samo stan",     metrike(podaci$y_cena, pred_stan_sp_xgb)),
  cbind(Model = "Samo lokacija", metrike(podaci$y_cena, pred_lok_sp_xgb)),
  cbind(Model = "Stan + lokacija", metrike(podaci$y_cena, pred_all_sp_xgb))
)

print(rez_xgb_sp)

## 5.8. Rezultati Prostorne i slučajne cv (RF i XGBoost)

sve_performanse <- rbind(
  data.frame(Validacija = "Slučajna CV (RF)", rez_rnd),
  data.frame(Validacija = "Prostorna CV (RF)", rez_sp),
  data.frame(Validacija = "Prostorna CV (XGBoost)", rez_xgb_sp)
)

write.csv(sve_performanse, "exceli/sve_performanse_modela.csv", row.names = FALSE)

## 5.10. Vizuelizacija - Predviđene vs. Stvarne cene (Sve spojeno)

df_mreza <- rbind(
  data.frame(
    Stvarna_cena = podaci$y_cena,
    Predviđena_cena = pred_all_rnd,
    Model = "RF - Slučajna CV"
  ),
  data.frame(
    Stvarna_cena = podaci$y_cena,
    Predviđena_cena = pred_all_sp,
    Model = "RF - Prostorna CV"
  ),
  data.frame(
    Stvarna_cena = podaci$y_cena,
    Predviđena_cena = pred_all_sp_xgb,
    Model = "XGBoost - Prostorna CV"
  )
)

df_mreza$Model <- factor(
  df_mreza$Model, 
  levels = c("RF - Slučajna CV", "RF - Prostorna CV", "XGBoost - Prostorna CV")
)

# spojeno
p_pred_vs_stvarno_mreza <- ggplot(df_mreza, aes(x = Stvarna_cena, y = Predviđena_cena, color = Model)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, color = "#a50026", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c(
    "RF - Slučajna CV" = "#2c7fb8",
    "RF - Prostorna CV" = "#d95f02",
    "XGBoost - Prostorna CV" = "#7570b3"
  )) +
  facet_wrap(~ Model, nrow = 1) +
  labs(
    title = "Predviđene vs. stvarne cene po m² (kombinovani modeli)",
    x = "Stvarna cena po m² (€)",
    y = "Predviđena cena po m² (€)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.4),
    panel.spacing = unit(1.2, "lines")
  )

print(p_pred_vs_stvarno_mreza)
ggsave("slike/pred_vs_stvarno_mreza.png", plot = p_pred_vs_stvarno_mreza, width = 12, height = 4.5, dpi = 300)

# 5.11. Poređenje R2, RMSE, MAE
sve_performanse <- sve_performanse %>%
  mutate(
    Validacija = factor(Validacija, levels = c("Slučajna CV (RF)", "Prostorna CV (RF)", "Prostorna CV (XGBoost)")),
    Model = factor(Model, levels = c("Samo stan", "Samo lokacija", "Stan + lokacija"))
  )

boja <- c(
  "Slučajna CV (RF)"       = "#2c7fb8",
  "Prostorna CV (RF)"      = "#d95f02",
  "Prostorna CV (XGBoost)" = "#7570b3"
)

p_r2 <- ggplot(sve_performanse, aes(x = Model, y = r2, fill = Validacija)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85), width = 0.7) +
  geom_text(aes(label = r2), position = position_dodge(width = 0.85), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = boja) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Poređenje R²", x = "Model", y = "R²", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    legend.position = "top",
    legend.justification = "center",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold")
  )

p_mae <- ggplot(sve_performanse, aes(x = Model, y = mae, fill = Validacija)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85), width = 0.7) +
  geom_text(aes(label = mae), position = position_dodge(width = 0.85), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = boja) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Poređenje MAE", x = "Model", y = "MAE (€/m²)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    legend.position = "top",
    legend.justification = "center",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold")
  )

p_rmse <- ggplot(sve_performanse, aes(x = Model, y = rmse, fill = Validacija)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85), width = 0.7) +
  geom_text(aes(label = rmse), position = position_dodge(width = 0.85), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = boja) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Poređenje RMSE", x = "Model", y = "RMSE (€/m²)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    legend.position = "top",
    legend.justification = "center",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold")
  )
p_rmse
ggsave("slike/grafikon_r2.png", plot = p_r2, width = 8, height = 6, dpi = 300)
ggsave("slike/grafikon_mae.png", plot = p_mae, width = 8, height = 6, dpi = 300)
ggsave("slike/grafikon_rmse.png", plot = p_rmse, width = 8, height = 6, dpi = 300)

# Vizuelizacija preklopa za prostornu  validaciju
viz_fold <- factor(paste("Preklop", fold_sp), 
                   levels = paste("Preklop", 1:5))
p_cv <- ggplot(sf_df) +
  geom_sf(aes(color = viz_fold), size = 0.8, alpha = 0.7) +
  geom_sf(data = beograd_granice, fill = NA, color = "black", size = 0.4) +
  scale_color_brewer(palette = "Set1", name = "Prostorni preklop") +
  labs(
    title = "Prostorna unakrsna validacija",
    subtitle = "Raspodela lokacija nekretnina po prostornim preklopima",
    x = NULL,
    y = NULL
  ) +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p_cv)
ggsave("slike/prostorni_foldovi.png", plot = p_cv, width = 8, height = 6, dpi = 300)

## ---------------------------------------------------------------------------
## 6. Značaj promenljivih (Feature Importance) - Zadatak 6

form_all <- as.formula(paste("Cena_m2 ~", paste(sprintf("`%s`", c(prom_stan, prom_lok)), collapse = " + ")))

model_pun <- ranger(form_all, data = podaci, num.trees = 500, min.node.size = 3, importance = "permutation")

imp <- sort(model_pun$variable.importance, decreasing = TRUE)
imp_df <- data.frame(Promenljiva = names(imp), Znacaj = as.numeric(imp))
imp_df$Znacaj_rel <- round(100 * imp_df$Znacaj / sum(imp_df$Znacaj), 1)

print(head(imp_df, 15))
write.csv(imp_df, "exceli/znacaj_promenljivih.csv", row.names = FALSE)

p_imp <- ggplot(head(imp_df, 15), aes(reorder(Promenljiva, Znacaj_rel), Znacaj_rel)) +
  geom_col(fill = "#2b5c8f", width = 0.65) +
  geom_text(aes(label = paste0(Znacaj_rel, "%")), hjust = -0.1, size = 3.2, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Relativni značaj (%)") +
  theme_minimal()
p_imp
ggsave("slike/znacaj_promenljivih.png", plot = p_imp, width = 8, height = 5.5, dpi = 300)

## ---------------------------------------------------------------------------
## 7. Globalni moran's I indeks (RF i XGBoost - Prostorna CV)

rez_rnd_stan <- podaci$Cena_m2 - pred_stan_rnd
rez_rnd_lok  <- podaci$Cena_m2 - pred_lok_rnd
rez_rnd_pun  <- podaci$Cena_m2 - pred_all_rnd

rez_rf_stan <- podaci$Cena_m2 - pred_stan_sp
rez_rf_lok  <- podaci$Cena_m2 - pred_lok_sp
rez_rf_pun  <- podaci$Cena_m2 - pred_all_sp

rez_xgb_stan <- podaci$Cena_m2 - pred_stan_sp_xgb
rez_xgb_lok  <- podaci$Cena_m2 - pred_lok_sp_xgb
rez_xgb_pun  <- podaci$Cena_m2 - pred_all_sp_xgb

moran_rnd_stan <- moran.test(rez_rnd_stan, listw, zero.policy = TRUE)
moran_rnd_lok  <- moran.test(rez_rnd_lok,  listw, zero.policy = TRUE)
moran_rnd_pun  <- moran.test(rez_rnd_pun,  listw, zero.policy = TRUE)

moran_rf_stan <- moran.test(rez_rf_stan, listw, zero.policy = TRUE)
moran_rf_lok  <- moran.test(rez_rf_lok,  listw, zero.policy = TRUE)
moran_rf_pun  <- moran.test(rez_rf_pun,  listw, zero.policy = TRUE)

moran_xgb_stan <- moran.test(rez_xgb_stan, listw, zero.policy = TRUE)
moran_xgb_lok  <- moran.test(rez_xgb_lok,  listw, zero.policy = TRUE)
moran_xgb_pun  <- moran.test(rez_xgb_pun,  listw, zero.policy = TRUE)

morans_globalni <- data.frame(
  Model = c("Samo stan", "Samo lokacija", "Stan + lokacija"),
  `RF_Rnd_I` = round(c(moran_rnd_stan$estimate[1], moran_rnd_lok$estimate[1], moran_rnd_pun$estimate[1]), 3),
  `RF_Sp_I` = round(c(moran_rf_stan$estimate[1], moran_rf_lok$estimate[1], moran_rf_pun$estimate[1]), 3),
  `XGB_Sp_I`= round(c(moran_xgb_stan$estimate[1], moran_xgb_lok$estimate[1], moran_xgb_pun$estimate[1]), 3)

)

print(morans_globalni)
print(morans_globalni)
write.csv(morans_globalni, "exceli/morans_globalni.csv", row.names = FALSE)

# Vizuelizacija
sf_df$rezidual_rf_rnd <- podaci$Cena_m2 - pred_all_rnd
sf_df$rezidual_rf_sp  <- podaci$Cena_m2 - pred_all_sp
sf_df$rezidual_xgb_sp <- podaci$Cena_m2 - pred_all_sp_xgb

p1_mi <- ggplot(sf_df) +
  geom_sf(data = beograd_granice, fill = NA, color = "grey30", size = 0.3) +
  geom_sf(aes(color = rezidual_rf_rnd), size = 0.6, alpha = 0.8) +
  scale_color_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
    limits = c(-1500, 1500), 
    oob = scales::squish,
    name = "Rezidual (EUR)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"), 
      barheight = unit(0.6, "cm"), 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  labs(title = "RF - Slučajna CV") +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p1_mi

p2_mi <- ggplot(sf_df) +
  geom_sf(data = beograd_granice, fill = NA, color = "grey30", size = 0.3) +
  geom_sf(aes(color = rezidual_rf_sp), size = 0.6, alpha = 0.8) +
  scale_color_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
    limits = c(-1500, 1500), 
    oob = scales::squish,
    name = "Rezidual (EUR)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"), 
      barheight = unit(0.6, "cm"), 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  labs(title = "RF - Prostorna CV") +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p2_mi

p3_mi <- ggplot(sf_df) +
  geom_sf(data = beograd_granice, fill = NA, color = "grey30", size = 0.3) +
  geom_sf(aes(color = rezidual_xgb_sp), size = 0.6, alpha = 0.8) +
  scale_color_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
    limits = c(-1500, 1500), 
    oob = scales::squish,
    name = "Rezidual (EUR)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"), 
      barheight = unit(0.6, "cm"), 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  labs(title = "XGBoost - Prostorna CV") +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p3_mi

ggsave("slike/p1_mi.png", plot = p1_mi, width = 11, height = 7, dpi = 300)
ggsave("slike/p2_mi.png", plot = p2_mi, width = 11, height = 7, dpi = 300)
ggsave("slike/p3_mi.png", plot = p3_mi, width = 11, height = 7, dpi = 300)

# Vizuelizacija UVEĆANO
centar_cenam2 <- st_bbox(c(
  xmin = 442000, ymin = 4950000,  
  xmax = 468000, ymax = 4968000
), crs = 32634)
uvecano_cenam2 <- st_crop(sf_df, centar_cenam2)
uvecano_granice <- st_crop(beograd_granice, centar_cenam2)
p1_mi_uvecano <- ggplot() +
  geom_sf(data = uvecano_cenam2, aes(color = rezidual_rf_rnd), size = 1, alpha = 0.8) +
  geom_sf(data = uvecano_granice, fill = NA, color = "grey30", size = 0.3) +
  scale_color_gradient2(
    low = "#2166ac", mid = "grey90", high = "#b2182b", midpoint = 0,
    limits = c(-1500, 1500),
    oob = scales::squish,
    name = "Rezidual (EUR)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"), 
      barheight = unit(0.6, "cm"), 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  coord_sf(
    xlim = c(442000, 468000),  
    ylim = c(4950000, 4968000),  
    expand = FALSE,  
    datum = 32634
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p1_mi_uvecano
p2_mi_uvecano <- ggplot() +
  geom_sf(data = uvecano_cenam2, aes(color = rezidual_rf_sp), size = 1, alpha = 0.8) +
  geom_sf(data = uvecano_granice, fill = NA, color = "grey30", size = 0.3) +
  scale_color_gradient2(
    low = "#2166ac", mid = "grey90", high = "#b2182b", midpoint = 0,
    limits = c(-1500, 1500),
    oob = scales::squish,
    name = "Rezidual (EUR)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"), 
      barheight = unit(0.6, "cm"), 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  coord_sf(
    xlim = c(442000, 468000),  
    ylim = c(4950000, 4968000),  
    expand = FALSE,  
    datum = 32634
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p2_mi_uvecano
p3_mi_uvecano <- ggplot() +
  geom_sf(data = uvecano_cenam2, aes(color = rezidual_xgb_sp), size = 1, alpha = 0.8) +
  geom_sf(data = uvecano_granice, fill = NA, color = "grey30", size = 0.3) +
  scale_color_gradient2(
    low = "#2166ac", mid = "grey90", high = "#b2182b", midpoint = 0,
    limits = c(-1500, 1500),
    oob = scales::squish,
    name = "Rezidual (EUR)",
    guide = guide_colorbar(
      barwidth = unit(6, "cm"), 
      barheight = unit(0.6, "cm"), 
      title.position = "top", 
      title.hjust = 0.5
    )
  ) +
  coord_sf(
    xlim = c(442000, 468000),  
    ylim = c(4950000, 4968000),  
    expand = FALSE,  
    datum = 32634
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )
p3_mi_uvecano
ggsave("slike/p1_mi_uvecano.png", plot = p1_mi_uvecano, width = 8, height = 6, dpi = 300)
ggsave("slike/p2_mi_uvecano.png", plot = p2_mi_uvecano, width = 8, height = 6, dpi = 300)
ggsave("slike/p3_mi_uvecano.png", plot = p3_mi_uvecano, width = 8, height = 6, dpi = 300)

## ---------------------------------------------------------------------------
## 8. Lokacijski premijum (Nad RF modelom)

prom_stan <- c("Kvadratura","Broj_soba","Tip_objekta","Stanje_objekta", 
               "Grejanje","Sprat","Ukupna_spratnost",
               "Uknjižen","Nije_poslednji_sprat","Nije_stan_u_kući",
               "Salonski","Duplex","Penthouse","Pod_hipotekom","Energetski_pasoš",
               "Potkrovlje", "Terasa","Francuski_balkon","Lođa",
               "Klima","Lift","Podrum","Topla_voda","Telefon","KATV","Internet",
               "Interfon","Video_nadzor","Kamin","Garaža","Parking","Sa_baštom"
) # karaketristike stana (32)

prom_lok <- c("Y","X", "Opština", "Naselje","Ulica", grep("^rast_.*_ag$", names(stanovi_ag), value = TRUE)) # 14
prom_comb <- c(prom_stan, prom_lok) # 48

form_comb <- as.formula(paste("Cena_m2 ~", paste(sprintf("`%s`", prom_comb), collapse = " + ")))

rf_kombinovani <- ranger(
  formula    = form_comb,
  data       = stanovi_ag,
  num.trees  = 500,
  importance = "permutation",
  seed       = 123
)
rf_kombinovani # nad celim skupom podataka

moda <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

med_kvadratura <- median(stanovi_ag$Kvadratura, na.rm = TRUE)
med_broj_soba <- median(stanovi_ag$Broj_soba, na.rm = TRUE)
med_sprat <- median(stanovi_ag$Sprat, na.rm = TRUE)
med_ukupna_spratnost <- median(stanovi_ag$Ukupna_spratnost, na.rm = TRUE)

mod_tip_objekta <- moda(stanovi_ag$Tip_objekta)
mod_stanje_objekta <- moda(stanovi_ag$Stanje_objekta)
mod_grejanje <- moda(stanovi_ag$Grejanje)
mod_uknjizen <- moda(stanovi_ag$Uknjižen)
mod_nije_poslednji <- moda(stanovi_ag$Nije_poslednji_sprat)
mod_nije_u_kuci <- moda(stanovi_ag$Nije_stan_u_kući)
mod_salonski <- moda(stanovi_ag$Salonski)
mod_duplex <- moda(stanovi_ag$Duplex)
mod_penthouse <- moda(stanovi_ag$Penthouse)
mod_hipoteka <- moda(stanovi_ag$Pod_hipotekom)
mod_energetski <- moda(stanovi_ag$Energetski_pasoš)
mod_potkrovlje <- moda(stanovi_ag$Potkrovlje)
mod_terasa <- moda(stanovi_ag$Terasa)
mod_balkon <- moda(stanovi_ag$Francuski_balkon)
mod_lodja <- moda(stanovi_ag$Lođa)
mod_klima <- moda(stanovi_ag$Klima)
mod_lift <- moda(stanovi_ag$Lift)
mod_podrum <- moda(stanovi_ag$Podrum)
mod_topla_voda <- moda(stanovi_ag$Topla_voda)
mod_telefon <- moda(stanovi_ag$Telefon)
mod_katv <- moda(stanovi_ag$KATV)
mod_internet <- moda(stanovi_ag$Internet)
mod_interfon <- moda(stanovi_ag$Interfon)
mod_video <- moda(stanovi_ag$Video_nadzor)
mod_kamin <- moda(stanovi_ag$Kamin)
mod_garaza <- moda(stanovi_ag$Garaža)
mod_parking <- moda(stanovi_ag$Parking)
mod_basta <- moda(stanovi_ag$Sa_baštom)

jed_lokacije <- stanovi_ag %>%
  distinct(Y, X, .keep_all = TRUE) %>%
  select(all_of(prom_lok))

fikt_stan <- jed_lokacije %>%
  mutate(
    Kvadratura = med_kvadratura,
    Broj_soba = med_broj_soba,
    Sprat = med_sprat,
    Ukupna_spratnost = med_ukupna_spratnost,
    Tip_objekta = mod_tip_objekta,
    Stanje_objekta = mod_stanje_objekta,
    Grejanje = mod_grejanje,
    Uknjižen = mod_uknjizen,
    Nije_poslednji_sprat = mod_nije_poslednji,
    Nije_stan_u_kući = mod_nije_u_kuci,
    Salonski = mod_salonski,
    Duplex = mod_duplex,
    Penthouse = mod_penthouse,
    Pod_hipotekom = mod_hipoteka,
    Energetski_pasoš = mod_energetski,
    Potkrovlje = mod_potkrovlje,
    Terasa = mod_terasa,
    Francuski_balkon = mod_balkon,
    Lođa = mod_lodja,
    Klima = mod_klima,
    Lift = mod_lift,
    Podrum = mod_podrum,
    Topla_voda = mod_topla_voda,
    Telefon = mod_telefon,
    KATV = mod_katv,
    Internet = mod_internet,
    Interfon = mod_interfon,
    Video_nadzor = mod_video,
    Kamin = mod_kamin,
    Garaža = mod_garaza,
    Parking = mod_parking,
    Sa_baštom = mod_basta
  )

fikt_stan$proc_cenam2 <- predict(rf_kombinovani, data = fikt_stan)$predictions
cena_med <- median(fikt_stan$proc_cenam2, na.rm = TRUE)
cena_med
fikt_stan <- fikt_stan %>%
  mutate(
    premijum_eur = proc_cenam2 - cena_med,
    premijum_proc = ((proc_cenam2 - cena_med) / cena_med) * 100
  )

sf_fikt <- st_as_sf(fikt_stan, coords = c("Y", "X"), crs = 32634, remove = FALSE)

# vizuelizacija karata
karta_cene_prem <- ggplot() +
  geom_sf(data = sf_fikt, aes(color = proc_cenam2), size = 1, alpha = 0.85) +
  geom_sf(data = beograd_granice, fill = NA, color = "red", size = 0.3) +
  scale_color_viridis_c(
    option = "plasma", 
    name = "Procenjena cena po m²",
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Procenjena cena referentnog stana po lokacijama",
  ) +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()
karta_cene_prem

karta_lok_prem <- ggplot() +
  geom_sf(data = sf_fikt, aes(color = premijum_proc), size = 1, alpha = 0.85) +
  geom_sf(data = beograd_granice, fill = NA, color = "red", size = 0.3) +
  scale_color_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a9850",
    midpoint = 0, name = "Premijum (%)"
  ) +
  labs(
    title = "Lokacijski premijum referentnog stana (Promena u %)"
  ) +
  coord_sf(datum = 32634) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()
karta_lok_prem
ggsave("slike/karta_cene_prem.png", plot = karta_cene_prem, width = 8, height = 6, dpi = 300)
ggsave("slike/karta_lok_prem.png", plot = karta_lok_prem, width = 8, height = 6, dpi = 300)

range(sf_fikt$premijum_proc, na.rm = TRUE)
sf_fikt[which.min(sf_fikt$premijum_proc), ]
sf_fikt[which.max(sf_fikt$premijum_proc), ]

# Vizuelizacija UVEĆANO
centar_cenam2 <- st_bbox(c(
  xmin = 442000, ymin = 4950000,  
  xmax = 468000, ymax = 4968000
), crs = 32634)
uvecano_cene_prem <- st_crop(sf_fikt, centar_cenam2)
karta_cene_prem_uvecano <- ggplot() +
  geom_sf(data = uvecano_cene_prem, aes(color = proc_cenam2), size = 1, alpha = 0.8) +
  geom_sf(data = beograd_granice, fill = NA, color = "red", size = 0.3) +
  scale_color_viridis_c(
    option = "plasma", 
    name = "Procenjena cena po m²",
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  coord_sf(
    xlim = c(442000, 468000),  
    ylim = c(4950000, 4968000),  
    expand = FALSE,  
    datum = 32634
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
karta_cene_prem_uvecano

uvecano_lok_prem <- st_crop(sf_fikt, centar_cenam2)
karta_lok_prem_uvecano <- ggplot() +
  geom_sf(data = uvecano_lok_prem, aes(color = premijum_proc), size = 1, alpha = 0.8) +
  geom_sf(data = beograd_granice, fill = NA, color = "red", size = 0.3) +
  scale_color_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a9850",
    midpoint = 0, name = "Premijum (%)"
  ) +
  coord_sf(
    xlim = c(442000, 468000),  
    ylim = c(4950000, 4968000),  
    expand = FALSE,  
    datum = 32634
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
karta_lok_prem_uvecano

ggsave("slike/karta_cene_prem_uvecano.png", plot = karta_cene_prem_uvecano, width = 9, height = 7, dpi = 150)
ggsave("slike/karta_lok_prem_uvecano.png", plot = karta_lok_prem_uvecano, width = 9, height = 7, dpi = 150)

# Premijum po opštinama
prem_opstine <- fikt_stan %>%
  group_by(Opština) %>%
  summarise(
    Broj_lokacija         = n(),
    Prosečna_Cena_fikt    = round(mean(proc_cenam2)),
    Prosečni_premijum_proc = round(mean(premijum_proc), 2),
    .groups               = "drop"
  ) %>%
  arrange(desc(Prosečni_premijum_proc))
prem_opstine
write.csv(prem_opstine, "exceli/premijum_po_opstinama.csv", row.names = FALSE)

# interaktivna karta
sg_wgs_prem <- st_transform(sf_fikt, 4326)
custom_popup <- paste0(
  "<div style='line-height: 2;'>",
  "<b>Opština: </b>", sg_wgs_prem$Opština, "<br>",
  "<b>Naselje: </b>", sg_wgs_prem$Naselje, "<br>",
  "<b>Ulica: </b>", sg_wgs_prem$Ulica, "<br>",
  "<b>Procenjena cena: </b>", format(round(sg_wgs_prem$proc_cenam2), big.mark = "."), " €<br>",
  "<b>Premijum: </b>", round(sg_wgs_prem$premijum_proc, 2), " %"
)
interakt_prem <- mapview(
  sg_wgs_prem,
  zcol = "premijum_proc",
  layer.name = "Premijum (%)",
  col.regions = colorRampPalette(c("#d73027", "#ffffbf", "#006837"))(100),
  cex = 4,            
  alpha.regions = 0.8,                        
  popup = custom_popup,                        
  map.types = c("OpenTopoMap", "Esri.WorldImagery", "OpenStreetMap")
)
interakt_prem
mapshot(interakt_prem, file = normalizePath("web/interakt_prem.html", mustWork = FALSE))
saveWidget(interakt_prem@map, "web/interakt_prem.html", selfcontained = TRUE)

# Dijag rasejanja
dg8_centar <- ggplot(fikt_stan, aes(x = rast_centar_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od centra na lokacijski premijum",
    x = "Udaljenost od centra (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_centar
ggsave("slike/dg8_centar.png", plot = dg8_centar, width = 9, height = 5.5, dpi = 140)

dg8_muzeji_pozorista <- ggplot(fikt_stan, aes(x = rast_muzeji_pozorista_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od ustanova kulture na lokacijski premijum",
    x = "Udaljenost od ustanova kulture (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_muzeji_pozorista
ggsave("slike/dg8_muzeji_pozorista.png", plot = dg8_muzeji_pozorista, width = 9, height = 5.5, dpi = 140)

dg8_vrtici <- ggplot(fikt_stan, aes(x = rast_vrtici_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od vrtića na lokacijski premijum",
    x = "Udaljenost od vrtića (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_vrtici
ggsave("slike/dg8_vrtici.png", plot = dg8_vrtici, width = 9, height = 5.5, dpi = 140)

dg8_skole <- ggplot(fikt_stan, aes(x = rast_skole_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od škola na lokacijski premijum",
    x = "Udaljenost od škola (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_skole
ggsave("slike/dg8_skole.png", plot = dg8_skole, width = 9, height = 5.5, dpi = 140)

dg8_verski_objekti <- ggplot(fikt_stan, aes(x = rast_verski_objekti_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od verskih objekata na lokacijski premijum",
    x = "Udaljenost od verskih objekata (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_verski_objekti
ggsave("slike/dg8_verski_objekti.png", plot = dg8_verski_objekti, width = 9, height = 5.5, dpi = 140)

dg8_stajalista <- ggplot(fikt_stan, aes(x = rast_stajalista_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od stajališta na lokacijski premijum",
    x = "Udaljenost od stajališta (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_stajalista
ggsave("slike/dg8_stajalista.png", plot = dg8_stajalista, width = 9, height = 5.5, dpi = 140)

dg8_parkovi <- ggplot(fikt_stan, aes(x = rast_parkovi_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od parkova na lokacijski premijum",
    x = "Udaljenost od parkova (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_parkovi
ggsave("slike/dg8_parkovi.png", plot = dg8_parkovi, width = 9, height = 5.5, dpi = 140)

dg8_sume <- ggplot(fikt_stan, aes(x = rast_sume_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od šuma na lokacijski premijum",
    x = "Udaljenost od šuma (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_sume
ggsave("slike/dg8_sume.png", plot = dg8_sume, width = 9, height = 5.5, dpi = 140)

dg8_vode <- ggplot(fikt_stan, aes(x = rast_vode_ag / 1000, y = premijum_proc)) +
  geom_point(color = "#2b8cbe", alpha = 0.5, size = 1) +
  geom_smooth(method = "loess", span = 1.2, color = "#bd0026", fill = "#fcae91", linewidth = 1, se = TRUE) +
  labs(
    title = "Uticaj udaljenosti od vode na lokacijski premijum",
    x = "Udaljenost od vode (km)",
    y = "Lokacijski premijum (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
dg8_vode
ggsave("slike/dg8_vode.png", plot = dg8_vode, width = 9, height = 5.5, dpi = 140)

# svih 9 dijagrama
dg8_svi <- 
  (dg8_centar | dg8_muzeji_pozorista | dg8_vrtici) /
  (dg8_skole | dg8_verski_objekti | dg8_stajalista) /
  (dg8_parkovi | dg8_sume | dg8_vode)
dg8_svi
ggsave(
  "slike/lok_prem_rastojanja.png",
  plot = dg8_svi,
  width = 15,
  height = 13,
  dpi = 300
)

## ---------------------------------------------------------------------------

