library(httr)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)
library(highcharter)
library(janitor)

# Step 1: Download the Data
response <- httr::GET("https://weather-api-tau-six.vercel.app/weather/JawaBarat")
json_data <- httr::content(response, "text")
parsed_data <- jsonlite::fromJSON(json_data, flatten = TRUE)

View(parsed_data)

weather_data_raw  <- parsed_data$data$forecast$area %>%
    unnest(parameter, names_sep = "_") %>%
    unnest(parameter_timerange, names_sep = "_") %>% 
    unnest(parameter_timerange_value, names_sep = "_") %>% 
    rename(
        stasiun_pengukuran = description,
        provinsi = domain,
        lon = longitude, lat = latitude,
        lokasi = name,
        weather_variable = parameter_description,
        timebound = parameter_timerange_type,
        datetime = parameter_timerange_datetime,
        value = parameter_timerange_value_text,
        unit = parameter_timerange_value_unit
    ) %>% 
    mutate(
        tanggal_waktu = ymd_hm(datetime, tz = "Asia/Jakarta"),
        tanggal = date(tanggal_waktu),
        kabkot = map_chr(lokasi, ~unlist(strsplit(.x, ","))[2])
        ) %>% 
    select(-lokasi)


View(weather_data_raw)

weather_hourly <- weather_data_raw %>%
    filter(
        timebound == "hourly",
        unit %in% c("%","C","deg","KPH")
        ) %>% 
    select(
        id, stasiun_pengukuran, kabkot, provinsi,
        lon, lat,tanggal,
        tanggal_waktu,
        weather_variable,
        value
    ) %>% 
    pivot_wider(names_from = weather_variable, values_from = value) %>% 
    clean_names()

View(weather_hourly)


weather_daily <- weather_data_raw %>%
    filter(
        timebound == "daily",
        unit %in% c("%","C","deg","KPH")
        ) %>% 
    select(
        id, stasiun_pengukuran, kabkot, provinsi,
        lon, lat,tanggal,
        tanggal_waktu,
        weather_variable,
        value
    ) %>% 
    pivot_wider(names_from = weather_variable, values_from = value) %>% 
    clean_names()

View(weather_daily)

# wrangling
kabkot_terpanas  <- weather_daily %>% 
    filter(tanggal == min(tanggal)) %>% 
    arrange(desc(max_temperature)) %>% 
    mutate(max_temperature = as.numeric(max_temperature)) %>% 
    head(8)

kabkot_terlembab  <- weather_daily %>% 
    filter(tanggal == min(tanggal)) %>% 
    arrange(desc(max_humidity)) %>% 
    mutate(max_humidity = as.numeric(max_humidity)) %>% 
    head(8)

kabkot_berangin  <- weather_hourly %>% 
    filter(tanggal == min(tanggal)) %>% 
    mutate(wind_speed = as.numeric(wind_speed)) %>% 
    reframe(
        max_wind_speed = max(wind_speed,na.rm = T),
        .by = kabkot
    ) %>% 
    ungroup() %>% 
    arrange(desc(max_wind_speed)) %>% 
    head(8)

suhu_harian <- weather_hourly %>% 
    mutate(
      temperature = as.numeric(temperature),
      humidity = as.numeric(humidity),
      ) %>% 
      reframe(
        mean_temp = mean(temperature, na.rm = T),
        .by = c(kabkot, tanggal)
      ) %>% 
    left_join(
        weather_hourly %>% 
        mutate(temperature= as.numeric(temperature)) %>% 
        reframe(med_temp = median(temperature, na.rm = T),.by = tanggal),
        join_by(tanggal)
    ) %>% 
    mutate(tanggal = ymd(tanggal))

View(suhu_harian)

suhu_harian_bdg  <- 
  suhu_harian %>% 
  filter(kabkot == "Kota Bandung") %>% 
  left_join(
    weather_daily  %>% select(tanggal, kabkot, max_temperature, min_temperature),
    join_by(tanggal, kabkot)
  ) %>% 
  mutate(
    across(ends_with("temperature"),~as.numeric(.x))
  )

View(suhu_harian_bdg)
# viz
cols_1 <- c("#005f73","#0a9396","#94d2bd","#e9d8a6")

kabkot_terpanas$clrs  <- highcharter::colorize(kabkot_terpanas$max_temperature, cols_1)
tt1_x  <- c("Suhu (celcius): ")
tt1_y  <- sprintf("{point.%s:.2f}",c("max_temperature"))
tt1  <- tooltip_table(tt1_x,tt1_y)

kabkot_terpanas_bar <- hchart(
  kabkot_terpanas,
  "bar",
  hcaes(
    kabkot, 
    max_temperature,
    color = clrs)
  )|> 
   hc_xAxis(
      title = list(text = "Suhu"),
    gridLineWidth = 0
    ) |>
  hc_yAxis(
    title = list(text = "Kabupaten/Kota"),
    gridLineWidth = 0
    )  |>
  hc_title(
    text = "10 Kabupaten/Kota Jawa Barat Terpanas Hari Ini"
    ) |> 
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = tt1
    )

kabkot_terpanas_bar

kabkot_berangin$clrs  <- highcharter::colorize(kabkot_berangin$max_wind_speed, cols_1)
tt2_x  <- c("Kecepatan Angin (km/jam): ")
tt2_y  <- sprintf("{point.%s:.2f}",c("max_wind_speed"))
tt2  <- tooltip_table(tt2_x,tt2_y)

kabkot_berangin_bar <- hchart(
  kabkot_berangin,
  "bar",
  hcaes(
    kabkot, 
    max_wind_speed,
    color = clrs)
  )|> 
   hc_xAxis(
      title = list(text = "Kecepatan Angin (km/jam)"),
    gridLineWidth = 0
    ) |>
  hc_yAxis(
    title = list(text = "Kabupaten/Kota"),
    gridLineWidth = 0
    )  |>
  hc_title(
    text = "10 Kabupaten/Kota Jawa Barat Paling Berangin Hari Ini"
    ) |> 
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = tt2
    )

kabkot_berangin_bar

tt3_x <- c("Min", "Mean", "Max")
tt3_y <- sprintf("{point.%s}°", c("min_temperature", "mean_temp", "max_temperature"))

tt3 <- tooltip_table(tt3_x, tt3_y)

  hc_yAxis(
    max = 30,
    min = -10,
    labels = list(format = "{value} C"),
    showFirstLabel = FALSE
  ) |>
  hc_xAxis(
    title = list(text = ""), 
    gridLineWidth = 0.5,
    labels = list(format = "{value: %b}")
  )
hchart(
  suhu_harian_bdg,
  type = "columnrange",
  hcaes(
    x = tanggal, 
    low = min_temperature, 
    high = max_temperature,
    color = mean_temp
    )
  )   %>% 
    hc_yAxis(
    max = 35,
    min = 15,
    labels = list(format = "{value} C"),
    showFirstLabel = FALSE
  ) |>
  hc_xAxis(
    title = list(text = ""), 
    gridLineWidth = 0.5,
    labels = list(format = "{value: %e-%b}")
  ) %>% 
    hc_tooltip(
    useHTML = TRUE,
    pointFormat = tt3,
    headerFormat = as.character(tags$small("{point.x:%d %B, %Y}"))
  )

tes  <- suhu_harian %>% 
    mutate(wind_speed = as.numeric(wind_speed)) %>% 
    ggplot() +
    geom_point(
      aes(
        x = tanggal_waktu,
        y = temperature
      )
    ) +
  geom_line(
    aes(
      y = med_temp,
      x = tanggal_waktu
    
    )
  )

tes
