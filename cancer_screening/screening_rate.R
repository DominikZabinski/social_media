# libraries ----
library(data.table)
library(openxlsx)
library(ggplot2)
library(showtext)
library(cowplot)
library(sf)
library(rmapshaper)
library(RColorBrewer)
# functions ----
#' Title
#'
#' @param download_link 
#' @param main_page 
#' @param save_dir 
#'
#' @return
#' @export
#'
#' @examples
download_file_from <- function(download_link, main_page, save_dir) {
  filename <- basename(download_link)
  dest_filename <- paste0(save_dir, filename)
  if (!file.exists(dest_filename)) {
    message(sprintf("%s plik %s", Sys.time(), filename)) # just to know how we are doing
    download.file(url = paste0(main_page, download_link), destfile = dest_filename, mode = "wb")
    # there is a small chance that we get block from a server so it is better to behave nicely and 
    # put some time between each download  
    Sys.sleep(1) 
  } else {
    message(sprintf("Skipping %s", filename))
  }
}

#' Title
#'
#' @param data_object 
#' @param screening_year 
#' @param screening_month 
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(data_object, screening_year, screening_month) {
  # Prepare new names for columns. The problem is there is multiline header in each file and the names of some columns 
  # are 
  # a) split among multiple line 
  # b) have time stamp, related to the specific time period that this file is related to
  new_colnames <- c()
  # from which row data starts
  last_header_row <- max(which(is.na(data_object[[1]])))
  for (i in c(1:length(data_object))) {
    iter_col_data <- trimws(data_object[[i]][1:last_header_row])
    if (is.na(iter_col_data[1])) {
      suffix <- screening_year - as.numeric(iter_col_data[last_header_row])
      new_colname <- paste0(previous_values, " t-", suffix, collapse = "") 
    } else {
      new_colname <- paste0(iter_col_data[!is.na(iter_col_data)], collapse = " ")
      previous_values <- iter_col_data[1:(last_header_row - 1)]
      previous_values <- paste0(previous_values[!is.na(previous_values)], collapse = " ")
    }
    new_colnames <- c(new_colnames, new_colname)
  }
  
  # there is an empty character in some files, the reason why rbindlist with use.names = T does not work
  new_colnames <- gsub(pattern = "\u00a0", replacement = " ", x = new_colnames)
  setnames(data_object, names(data_object), new_colnames)
  
  # put info of year and month of the screening data
  data_object[, c("screening_year", "screening_month") := list(screening_year, screening_month)]
  
  # change some column type for numeric
  for (column in names(data_object)[substr(names(data_object), 1, 1) == "L"])
    suppressWarnings(data_object[[column]] <- as.numeric(gsub(pattern = "[^0-9.,]", replacement = "", x = data_object[[column]])))
  
  # only rows which have voidship (Poland has 16 of those) data or summary data (since not all the values could be
  # assigned to one of the vodiships)
  data_object[`OW NFZ` %in% c(1:16) | `OW NFZ` == "RAZEM"][]
}

#' Title
#'
#' @param xmin 
#' @param xmax 
#' @param y_sequence 
#'
#' @return
#' @export
#'
#' @examples
add_grid_lines <- function(xmin, xmax, y_sequence, base_size) {
  percent_labels <- 100 * y_sequence
  percent_labels[length(percent_labels)] <- paste0(percent_labels[length(percent_labels)], "%")
  percent_labels[-length(percent_labels)] <- paste0(percent_labels[-length(percent_labels)], "")
  list(
    lapply(X = y_sequence,
           FUN = function(i) {
             geom_segment(mapping = aes(x = xmin, xend = xmax, y = i, yend = i), linewidth = .1, color = "grey")
           }),
    lapply(X = 1:length(percent_labels), 
           FUN = function(i) {
             geom_text(mapping = aes(x = xmin, y = y_sequence[i], label = percent_labels[i]), 
                       vjust = -1, hjust = 0,
                       size = base_size / ggplot2::.pt, family = getOption("other_text_family"))
           })
  )
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
convert_period_id_to_date <- function(x) {
  yy <- floor(x/13)
  mm <- x - 13 * yy
  yy[is.na(yy)] <- 2022
  mm[is.na(mm)] <- 1
  format(as.Date(sprintf("%s-%02d-01", yy, mm)), "%Y-%m")
}

#' Title
#'
#' @param plot_to_add_labels_to 
#' @param list_of_labels 
#'
#' @return
#' @export
#'
#' @examples
add_labels <- function(plot_to_add_labels_to, list_of_labels) {
  for (i in 1:length(list_of_labels)) {
    single_list <- list_of_labels[[i]]
    if (!"x" %in% names(single_list)) single_list[["x"]] <- 1
    if (!"y" %in% names(single_list)) single_list[["y"]] <- 1
    if (!"color" %in% names(single_list)) single_list[["color"]] <- "black"
    if (!"size" %in% names(single_list)) single_list[["size"]] <- base_size * 1.5
    if (!"hjust" %in% names(single_list)) single_list[["hjust"]] <- 1
    if (!"vjust" %in% names(single_list)) single_list[["vjust"]] <- 1
    if (!"fontface" %in% names(single_list)) single_list[["fontface"]] <- "plain"
    
    plot_to_add_labels_to <- plot_to_add_labels_to + 
      draw_label(label = single_list$l, size = single_list$size, x = single_list$x, y = single_list$y, 
                 hjust = single_list$hjust, vjust = single_list$vjust, 
                 lineheight = .25, color = single_list$color, fontfamily = getOption("ann_text_family"), fontface = single_list$fontface)
  }
  return(plot_to_add_labels_to)
}

# from https://stackoverflow.com/questions/50973713/ggplot2-creating-themed-title-subtitle-with-cowplot
draw_label_theme <- function(label, theme = NULL, element = "text", ...) {
  if (is.null(theme)) {
    theme <- ggplot2::theme_get()
  }
  if (!element %in% names(theme)) {
    stop("Element must be a valid ggplot theme element name")
  }
  
  elements <- ggplot2::calc_element(element, theme)
  
  cowplot::draw_label(label, 
                      fontfamily = elements$family,
                      fontface = elements$face,
                      colour = elements$color,
                      size = elements$size, 
                      lineheight = elements$lineheight,
                      ...
  )
}

# from https://stackoverflow.com/a/54557456
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

#' Title
#'
#' @param shape_data 
#' @param shape_id_colname 
#' @param layout_data 
#' @param layout_cluster_id_colname 
#' @param layout_shape_id_colname 
#' @param final_geometry_colname 
#'
#' @return
#' @export
#'
#' @examples
merge_polygons <- function(shape_data, shape_id_colname = "JPT_KOD_JE",
                           layout_data, layout_cluster_id_colname = "klaster", layout_shape_id_colname = "id",
                           final_geometry_colname = "g") {
  layout_data <- unique(subset(x = layout_data, select = c(layout_cluster_id_colname, layout_shape_id_colname)))
  
  l = lapply(
    X = sort(unique(layout_data[[layout_cluster_id_colname]])),
    FUN = function(i) {
      this_cluster_shapes <- layout_data[which(layout_data[[layout_cluster_id_colname]] == i), ][[layout_shape_id_colname]]
      aa <- which(shape_data[[shape_id_colname]] %in% this_cluster_shapes)
      kk <- shape_data[aa, ] %>%
        st_as_sf() %>%
        st_make_valid() %>%
        st_union()
      kk_d <- st_sf(layout_cluster_id_colname = i, g = kk)
      return(kk_d)
    }
  )
  
  res <- do.call(what = rbind, args = l)
  names(res) <- c(layout_cluster_id_colname, final_geometry_colname)
  return(res)
}
# settings ----
options(
  ann_text_family = "quicksand",
  other_text_family = "montserrat"
)

font_add_google(name = "Montserrat", family = "montserrat", regular.wt = 400)
font_add_google(name = "Quicksand", family = "quicksand", regular.wt = 400)
showtext_auto()

base_size <- 30
title_size <- 2.25 * base_size
capt_size <- .85 * base_size
lege_size <- 2 * base_size
axtext_size <- 0.8 * base_size
sett_chart <- theme(
  line = element_line(colour = "grey30"),
  text = element_text(family = getOption("other_text_family"), size = base_size), 
  axis.title = element_text(size = 1.25 * axtext_size),
  axis.text = element_text(size = axtext_size),
  axis.ticks = element_blank(), 
  legend.background = element_blank(), 
  legend.key = element_blank(), 
  legend.text = element_text(size = lege_size, lineheight = 0),
  panel.background = element_rect(fill = "transparent", color = NA),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = "transparent", color = NA),
  plot.title = element_text(family = getOption("ann_text_family"), size = title_size, face = "bold"),
  plot.caption = element_text(family = getOption("ann_text_family"), size = capt_size, hjust = 0, lineheight = 0.25),
  strip.background = element_blank(), 
  strip.text = element_text(size = axtext_size, margin = margin(t = -10), color = "grey30")
)
# data acquisition ----
# data is in form of .xlsx files
# we'll get all download links from site, save it and read one-by-one to retrieve usefull information
data_url <- "http://www.nfz.gov.pl/dla-pacjenta/programy-profilaktyczne/dane-o-realizacji-programow/"
webpage_content <- readLines(con = data_url)
# we look for two phrases: 'xlsx' and 'download'. next, we get download links from 'href' fields
download_links <- webpage_content[regexpr(pattern = "xlsx", text = webpage_content) > 1]
download_links <- download_links[regexpr(pattern = "download", text = download_links) > 1]
download_links <- substr(download_links, regexpr("href=", download_links) + nchar('href="/'), regexpr("xlsx", download_links) + 3)

# create directory to store downloaded files and start process
main_page <- "http://www.nfz.gov.pl/"
save_dir <- "data_in/"
dir.create(path = save_dir, showWarnings = F, recursive = T)
for (download_link in download_links) download_file_from(download_link, main_page, save_dir)

# data wrangling ----
# there is two types of screening data: breast cancer (mammografia) and cervical cancer (cytologia). we are interested in files containing data for each month and those files have specific name standard
# we'll do same thing for each screening: get all previously downloaded, read them into R, change data format and column names and store them in one object
process_directory <- "data_out/"
dir.create(path = process_directory, showWarnings = F, recursive = T)

# one has to fix file names due to changes in the naming convention
datatable_all_files <- data.table(filename = list.files(path = save_dir))
datatable_all_files[, screening_type := ifelse(filename %like% "mammog", "mammografia", "cytologia")]
exceptions_m <- c("obj._populacji_1_marca_2016_r.__m.xlsx", "objecie_populacji_programem_raka_piersi.xlsx")
datatable_all_files[filename %in% exceptions_m, screening_type := "mammografia"]
datatable_all_files[, file_date := regexpr(pattern = "(\\.\\d{2}\\.\\d{4})", text = filename) + 1]
datatable_all_files[, file_date := substr(filename, file_date, file_date + 6)]
datatable_all_files[, file_month := substr(file_date, 1, 2)]
table(datatable_all_files$file_month)

datatable_all_files[, file_year := substr(file_date, 4, 7)]
table(datatable_all_files$file_year)

dcast(datatable_all_files, file_year ~ file_month)

# fixing file dates
datatable_all_files[file_year == "2916", file_year := "2016"]
datatable_all_files[file_year == "eci", c("file_year", "file_month") := list("2021", "08")]
datatable_all_files[filename %like% "obj._populacji_1_marca_2016_r", c("file_year", "file_month") := list("2016", "03")]

dcast(datatable_all_files, file_year ~ file_month)

datatable_all_files[, file_year := as.numeric(file_year)]
datatable_all_files[, file_month := as.numeric(file_month)]
datatable_all_files <- datatable_all_files[file_year > 2016 | (file_year == 2016 & file_month > 1)]

table(datatable_all_files$screening_type)
dcast(datatable_all_files[screening_type == "cytologia"], file_year ~ file_month, fun.aggregate = length)
dcast(datatable_all_files[screening_type == "mammografia"], file_year ~ file_month, fun.aggregate = length)

for (iter_type in c("mammografia", "cytologia")) {
  files_to_process <- datatable_all_files[screening_type == iter_type]
  screening_data <- rbindlist(
    l = lapply(
      X = files_to_process$filename, 
      FUN = function(x){
        message(sprintf("%s %s", Sys.time(), x))
        data_object <- data.table(read.xlsx(paste0(save_dir, x), startRow = 1))
        data_file_details <- files_to_process[filename == x]
        return(process_data(data_object, data_file_details$file_year, data_file_details$file_month))
      }
    ), 
    use.names = T, fill = T
  )
  
  # there is a bug in the name of one voideship
  screening_data[`OW NFZ` == 5, `Nazwa województwa` := "ŁÓDZKIE"]
  
  # number of women excluded from screening
  screening_data$excluded <- ifelse(is.na(screening_data$`Liczba osób wyłączonych - ogółem`), 0, screening_data$`Liczba osób wyłączonych - ogółem`)
  
  # save data
  saveRDS(object = screening_data, file = sprintf("%s%s_polaczone.rds", process_directory, iter_type))
}
# data analysis - mammography ----
data_screening_m <- readRDS(file = "data_out/mammografia_polaczone.rds")
dict_ids <- unique(data_screening_m[,.(`ID gminy`, `ID powiatu`, `OW NFZ`, `Nazwa gminy`, `Nazwa powiatu`, `Nazwa województwa`)])
setnames(data_screening_m, which(names(data_screening_m) == "Liczba osób kwalifikujących się"), "number_qualified")

data_screening_m <- data_screening_m[,.(number_qualified = sum(number_qualified), number_excluded = sum(excluded)), by = .(`ID powiatu`, screening_year, screening_month)]
data_screening_m[, excluded_rate := number_excluded / number_qualified]
data_screening_m[, period_id := screening_year * 13 + screening_month]

data_ribbon <- data_screening_m[,.(minimum_rate = min(excluded_rate), maximum_rate = max(excluded_rate)), by = .(period_id)]
max_county_id <- data_screening_m[period_id == max(period_id)][excluded_rate == max(excluded_rate)]$`ID powiatu`
min_county_id <- data_screening_m[period_id == max(period_id)][excluded_rate == min(excluded_rate)]$`ID powiatu`

data_gridlines <- data.table(mini = min(data_screening_m$period_id), maxi = max(data_screening_m$period_id))
# data analysis - cytology ----
data_screening_c <- readRDS(file = "data_out/cytologia_polaczone.rds")
setnames(data_screening_c, which(names(data_screening_c) == "Liczba osób kwalifikujących się"), "number_qualified")

data_screening_c <- data_screening_c[,.(number_qualified = sum(number_qualified), number_excluded = sum(excluded)), by = .(`ID powiatu`, screening_year, screening_month)]
data_screening_c[, excluded_rate := number_excluded / number_qualified]
data_screening_c[, period_id := screening_year * 13 + screening_month]

data_ribbon_c <- data_screening_c[,.(minimum_rate = min(excluded_rate), maximum_rate = max(excluded_rate)), by = .(period_id)]
max_county_id_c <- data_screening_c[period_id == max(period_id)][excluded_rate == max(excluded_rate)]$`ID powiatu`
min_county_id_c <- data_screening_c[period_id == max(period_id)][excluded_rate == min(excluded_rate)]$`ID powiatu`

data_gridlines_c <- data.table(mini = min(data_screening_c$period_id), maxi = max(data_screening_c$period_id))

# simplifying shapefiles ----
# download the file 'Obreby' from  https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/ 
# because after 22.12.2022 something change in Wojewodztwa/Powiaty/Gminy files that the sea areas are included in the shp
shp_obreb <- st_read("Obreby/")
shp_obreb$ID_POW <- substr(shp_obreb$JPT_KOD_JE, 1, 4)
cluster_pow <- shp_obreb[, c("ID_POW", "JPT_KOD_JE")]
shp_powiat <- merge_polygons(shape_data = shp_obreb, shape_id_colname = "JPT_KOD_JE", layout_data = cluster_pow, layout_cluster_id_colname = "ID_POW", layout_shape_id_colname = "JPT_KOD_JE")
# simplified shape
shp_simplified <- ms_simplify(input = shp_powiat, keep = .0125/3, keep_shapes = T, weighting = 1)
# mammography plots ----
lineskip <- 0.017
mammo <- ggplot() + 
  # adding artificial gridlines
  add_grid_lines(data_gridlines$mini - 5, data_gridlines$maxi + 5, seq(0, .7, .15), axtext_size) +
  geom_text(mapping = aes(x = data_gridlines$mini - 5, y = .65, label = "Stopień objęcia populacji - program profilaktyki raka piersi"),
            size = 1.25 * axtext_size / ggplot2::.pt, family = getOption("other_text_family"), hjust = 0) +
  geom_ribbon(data = data_ribbon, aes(x = period_id, ymin = minimum_rate, ymax = maximum_rate), fill = "#FFB4BCCA") + 
  geom_line(data = data_screening_m, aes(x = period_id, y = excluded_rate, group = `ID powiatu`), alpha = .2, color = "white") +
  geom_line(data = data_screening_m[is.na(`ID powiatu`)], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "white", linewidth = 3.5) +
  geom_line(data = data_screening_m[is.na(`ID powiatu`)], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "#F5A6BC", linewidth = 2) +
  # highlight the county with maximum rate
  geom_line(data = data_screening_m[`ID powiatu` == max_county_id], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "white", linewidth = 3.5) +
  geom_line(data = data_screening_m[`ID powiatu` == max_county_id], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "#F386A5", linewidth = 2) +
  # highlight the county with minimum rate
  geom_line(data = data_screening_m[`ID powiatu` == min_county_id], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "white", linewidth = 3.5) +
  geom_line(data = data_screening_m[`ID powiatu` == min_county_id], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "#FFA7B3", linewidth = 2) +
  # dodatki tekstowe
  geom_segment(data = data.frame(x = 26250, xs = data_screening_m[excluded_rate == min(excluded_rate)]$period_id, 
                                 y = min(data_screening_m$excluded_rate), ys = min(data_screening_m$excluded_rate)), aes(x = x, y = y, xend = xs, yend = ys), linetype = "dashed") +
  geom_text(mapping = aes(x = 26250, y = .125 - 0:2 * lineskip, 
                          label = c("Po wprowadzeniu nowych zasad", "najniższy stopień objęcia populacji", "jest w powiecie gorlickim - 14,8%")),
            size = axtext_size / ggplot2::.pt, family = getOption("other_text_family"), hjust = 0, vjust = 1) +
  geom_segment(data = data.frame(x = 26260, xs = 26260, 
                                 y = .56, ys = .6), aes(x = x, y = y, xend = xs, yend = ys), linetype = "dashed") +
  geom_text(mapping = aes(x = 26261, y = .62 - 0:2 * lineskip, 
                          label = c("Jednym z liderów jest powiat", "zielonogórski. Stopień objęcia", "populacji przez długi okres wynosił > 50%.")),
            size = axtext_size / ggplot2::.pt, family = getOption("other_text_family"), hjust = 0, vjust = 1) +
  # scales
  scale_y_continuous(limits = function(x) c(0, max(x) * 1.01), expand = c(0, 0)) + 
  scale_x_continuous(labels = convert_period_id_to_date, expand = c(0, 1), n.breaks = 5) +
  sett_chart +
  theme(panel.grid = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(), plot.background = element_rect(fill = NULL, color = NULL)) +
  labs(y = NULL, x = NULL)
mammo

map_mam <- ggplot(data = merge(shp_simplified, data_screening_m[period_id == max(period_id),.(ID_POW = sprintf("%04d", as.numeric(`ID powiatu`)), excluded_rate)], by = "ID_POW"), aes(fill = excluded_rate)) +
  geom_sf(color = "grey90") +
  scale_fill_steps2(low = "#386A5F", mid = "white", high = "#F386A5",
                    midpoint = data_screening_m[period_id == max(period_id) & is.na(`ID powiatu`)]$excluded_rate, 
                    breaks = pretty(range(data_screening_m[period_id == max(period_id)]$excluded_rate)),
                    labels = scales::percent) +
  sett_chart +
  theme_void() +
  theme(legend.direction = "horizontal", legend.position = "bottom", 
        legend.text = element_text(size = lege_size / ggplot2::.pt)) +
  labs(fill = NULL, title = "Program profilaktyki raka piersi (2024-06-01)") +
  theme(title = element_text(size = base_size, color = "#F386A5", face = "bold"), legend.spacing.y = unit(0, "npc")) +
  guides(fill = guide_colorbar(barheight = unit(.025, "npc"), barwidth = unit(.5, "npc")))

map_mam
# cytology plots ----
cyto <- ggplot() + 
  # adding artificial gridlines
  add_grid_lines(data_gridlines_c$mini - 5, data_gridlines_c$maxi + 5, seq(0, .45, .05), axtext_size) +
  geom_text(mapping = aes(x = data_gridlines_c$mini - 5, y = .5, label = "Stopień objęcia populacji - program profilaktyki raka szyjki macicy"),
            size = 1.25 * axtext_size / ggplot2::.pt, family = getOption("other_text_family"), hjust = 0) +
  geom_ribbon(data = data_ribbon_c, aes(x = period_id, ymin = minimum_rate, ymax = maximum_rate), fill = "#b2d8d8") + 
  geom_line(data = data_screening_c, aes(x = period_id, y = excluded_rate, group = `ID powiatu`), alpha = .2, color = "white") +
  geom_line(data = data_screening_c[is.na(`ID powiatu`)], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "white", linewidth = 3.5) +
  geom_line(data = data_screening_c[is.na(`ID powiatu`)], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "#008080", linewidth = 2) +
  # highlight the county with maximum rate
  geom_line(data = data_screening_c[`ID powiatu` == max_county_id_c], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "white", linewidth = 3.5) +
  geom_line(data = data_screening_c[`ID powiatu` == max_county_id_c], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "#004c4c", linewidth = 2) +
  # highlight the county with minimum rate
  geom_line(data = data_screening_c[`ID powiatu` == min_county_id_c], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "white", linewidth = 3.5) +
  geom_line(data = data_screening_c[`ID powiatu` == min_county_id_c], aes(x = period_id, y = excluded_rate, group = `ID powiatu`), color = "#66b2b2", linewidth = 2) +
  # dodatki tekstowe
  # geom_segment(data = data.frame(x = 26270, xs = data_screening_c[excluded_rate == min(excluded_rate)]$period_id, 
  #                                y = min(data_screening_c$excluded_rate), ys = min(data_screening_c$excluded_rate)), aes(x = x, y = y, xend = xs, yend = ys), linetype = "dashed") +
  geom_text(mapping = aes(x = 26210, y = .0275 - 0:1 * lineskip, 
                          label = c("Jednym z powiatów z najniższym stopniem objęcia populacji", "jest powiat bieszczadzki. Od lat nie przekracza on 5% (obecnie 2,7%).")),
            size = axtext_size / ggplot2::.pt, family = getOption("other_text_family"), hjust = 0, vjust = 1) +
  geom_segment(data = data.frame(x = 26278, xs = 26278, 
                                 y = .40, ys = data_screening_c[`ID powiatu` == max_county_id_c][excluded_rate == min(excluded_rate)]$excluded_rate), aes(x = x, y = y, xend = xs, yend = ys), linetype = "dashed") +
  geom_text(mapping = aes(x = 26250, y = .45 - 0:2 * lineskip, 
                          label = c("Liderem profilaktycznej cytologii w ramach NFZ", "jest powiat m. Siedlce. Pomimo trendu spadkowego, od maja 2021", "widać wyraźny wzrost. Obecnie stopień objęcia wynosi 37,4%.")),
            size = axtext_size / ggplot2::.pt, family = getOption("other_text_family"), hjust = 0, vjust = 1) +
  # scales
  scale_y_continuous(limits = function(x) c(0, max(x) * 1.01), expand = c(0, 0)) + 
  scale_x_continuous(labels = convert_period_id_to_date, expand = c(0, 1), n.breaks = 6) +
  sett_chart +
  theme(panel.grid = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(), plot.background = element_rect(fill = NULL, color = NULL)) +
  labs(y = NULL, x = NULL)
cyto

map_cyto <- ggplot(data = merge(shp_simplified, data_screening_c[period_id == max(period_id),.(ID_POW = sprintf("%04d", as.numeric(`ID powiatu`)), excluded_rate)], by = "ID_POW"), aes(fill = excluded_rate)) +
  geom_sf(color = "grey90") +
  scale_fill_steps2(low = "#FF8000", mid = "white", high = "#008080",
                    midpoint = data_screening_c[period_id == max(period_id) & is.na(`ID powiatu`)]$excluded_rate, 
                    breaks = pretty(range(data_screening_c[period_id == max(period_id)]$excluded_rate)),
                    labels = scales::percent) +
  sett_chart +
  theme_void() +
  theme(legend.direction = "horizontal", legend.position = "bottom", 
        legend.text = element_text(size = lege_size / ggplot2::.pt)) +
  labs(fill = NULL, title = "Program profilaktyki raka szyjki macicy (2024-06-01)") +
  theme(title = element_text(size = base_size, color = "#008080", face = "bold"), legend.spacing.y = unit(0, "npc")) +
  guides(fill = guide_colorbar(barheight = unit(.025, "npc"), barwidth = unit(.5, "npc")))

map_cyto
# combining ----
blank_sp <- ggdraw() + sett_chart
rh <- c(0.20, 1.25, 1.15, 1.15, 1.25, 0.2)
szer <- 800 * 1 / 96

combined_plots <- plot_grid(
  ggdraw() + sett_chart +
    draw_label_theme(
      label = "Profilaktyko, quo vadis?", 
      theme = sett_chart, element = "plot.title", x = 0.05, hjust = 0, vjust = 0.5
    ), 
  plot_grid(mammo, blank_sp, nrow = 1, rel_widths = c(6, 3)), 
  plot_grid(blank_sp, map_mam, nrow = 1, rel_widths = c(4, 5)), 
  plot_grid(blank_sp, map_cyto, nrow = 1, rel_widths = c(4, 5)), 
  plot_grid(cyto, blank_sp, nrow = 1, rel_widths = c(6, 3)), 
  ggdraw() + sett_chart +
    draw_label_theme(
      label = "Dane: http://www.nfz.gov.pl/dla-pacjenta/programy-profilaktyczne/dane-o-realizacji-programow/\nViz: @DominikZabinski",
      theme = sett_chart, element = "plot.caption", x = 0.95, hjust = 1, vjust = 0.5
    ), 
  ncol = 1, rel_heights = rh
)
marg <- .035
teksty_mammo <- list(
  list(l = "Nowotwór piersi jest jednym z najczęściej\nwystępujących nowotworów i jedną\nz najczęstszych przyczyn zgonów\nwśród kobiet. Nowotwór szyjki macicy\njest trzecim najczęstszym\nnowotworem u kobiet.\n\nKluczem do walki z nimi jest\njak najszybsze wykrycie.", x = 1 - marg, y = .95),
  list(l = "Na stronie NFZ dostępne są\ndane dot. stopnia objęcia\npopulacji programem profilaktyki\nraka piersi oraz szyjki macicy.\n\nOd lutego 2016 publikowane są\nmiesięczne raporty, które pozwalają \n obserwować jak stopień objęcia\npopulacji zmieniał się w czasie.", x = 1 - marg, y = .85),
  list(l = "Na przestrzeni lat wyraźnie widoczny jest trend\nspadkowy. W niewielu powiatach stopień objęcia\npopulacji przekraczał 50% (wg Eurostat\nw krajach skandynawskich przekracza on 80%).\nNależy wspomnieć, że wg OECD wartość dla\ncałego kraju wynosi 54%, a wg danych NFZ 38%.", x = marg, y = .7, hjust = 0),
  list(l = "Analiza ostatnich danych wyraźnie wskazuje\nna zróżnicowanie przestrzenne. Powyżej wartości\nogólnopolskiej plasują się powiaty zachodnie\ni północne. Najniższe objęcie populacji to domena\npołudniowych regionów.", x = marg, y = .63, hjust = 0),
  list(l = "Od 01.11.2023 profilaktyczną mammografię\nna NFZ mogą wykonać kobiety w wieku \n45 – 74 lata. Do tej pory bezpłatna mammografia\nbyła dostępna dla kobiet w wieku 50 – 69 lat.\nZ tego powodu nie można porównywać wprost\nwyników dla skrajnych okresów.", x = marg, y = .57, hjust = 0),
  list(x = marg, hjust = 0, y = .495,
       l = "Znacznie gorzej przedstawia się sytuacja\nw programie profilaktyki raka szyjki macicy.\nWskaźnik objęcia populacji systematycznie spada,\nchoć są regiony (np. Siedlce), w których od kilku lat\nobserwowana jest poprawa."),
  list(x = marg, hjust = 0, y = .425,
       l = "W większości powiatów wskaźnik objęcia populacji\nnie przekracza 15%, zaś w skali kraju - 11%.\nLiczby te nie odpowiadają jednak danym\nprezentowanym przez OECD - 73%. "),
  list(x = marg, hjust = 0, y = .375, 
       l = "Należy mieć na uwadze, że od 01.11.2023 program\nobjął kobiety od 25. do 64. roku życia (wcześniej\nbył dostępny dla kobiet w wieku 25-59 lat)."),
  list(l = "Docelowy poziom objęcia\nprogramami raka piersi oraz\nszyjki macicy, wyznaczony\nprzez UE, to 90% w każdym\nz tych programów.\n\nA jaki jest nasz cel?", x = 1 - marg, y = .2)
)

combined_plots_labs <- add_labels(combined_plots, teksty_mammo)

ratio <- 4 * 9 / 16

combined_grad_labs <- ggdraw() + 
  annotation_custom(grob = make_gradient(deg = 150, n = 500, cols = c("#FFB4BCCA", rep("#FFFFFF", 3))), xmin = -Inf, xmax = Inf, ymin = .5, ymax = Inf) + 
  annotation_custom(grob = make_gradient(deg = -150, n = 500, cols = c("#008080", rep("#FFFFFF", 3))), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = .5) + 
  draw_plot(combined_plots_labs)

ggsave(plot = combined_grad_labs , filename = "prevention_care.png", width = szer, height = ratio * szer, units = "in", dpi = 720/2, bg = "white")
