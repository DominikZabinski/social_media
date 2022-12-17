# sessionInfo()
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Polish_Poland.1250  LC_CTYPE=Polish_Poland.1250    LC_MONETARY=Polish_Poland.1250 LC_NUMERIC=C                  
# [5] LC_TIME=Polish_Poland.1250    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] cowplot_1.1.1     dplyr_1.0.5       ggbump_0.1.0      showtext_0.9-2    showtextdb_3.0    sysfonts_0.8.3    ggplot2_3.3.5    
# [8] data.table_1.14.0 openxlsx_4.2.3   
# 
# loaded via a namespace (and not attached):
#   [1] zip_2.1.1        Rcpp_1.0.7       pillar_1.6.0     compiler_4.0.3   tools_4.0.3      jsonlite_1.7.2   lifecycle_1.0.0 
# [8] tibble_3.1.0     gtable_0.3.0     pkgconfig_2.0.3  rlang_1.0.2      DBI_1.1.1        cli_3.2.0        curl_4.3        
# [15] withr_2.4.1      generics_0.1.3   vctrs_0.4.1      grid_4.0.3       tidyselect_1.1.0 glue_1.6.2       R6_2.5.0        
# [22] fansi_0.4.2      purrr_0.3.4      magrittr_2.0.1   scales_1.1.1     ellipsis_0.3.2   assertthat_0.2.1 colorspace_2.0-0
# [29] utf8_1.2.1       stringi_1.5.3    munsell_0.5.0    crayon_1.4.1    

# libraries ----
library(openxlsx)
library(data.table)
library(ggplot2)
library(showtext)
library(ggbump)
library(dplyr) # because ggbump::geom_bump needs dplyr::arrange
library(cowplot)

# functions ----
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

add_labels <- function(pp, yy)
{
  for (i in 1:length(yy))
  {
    tt <- yy[[i]]
    if (!"x" %in% names(tt)) tt[["x"]] <- 1
    if (!"y" %in% names(tt)) tt[["y"]] <- 1
    if (!"color" %in% names(tt)) tt[["color"]] <- "black"
    if (!"size" %in% names(tt)) tt[["size"]] <- base_size * 1.5
    if (!"hjust" %in% names(tt)) tt[["hjust"]] <- 1
    if (!"vjust" %in% names(tt)) tt[["vjust"]] <- 1
    if (!"fontface" %in% names(tt)) tt[["fontface"]] <- "plain"
    
    pp <- pp + draw_label(label = tt$l, size = tt$size, x = tt$x, y = tt$y, hjust = tt$hjust, vjust = tt$vjust, lineheight = .25, color = tt$color, fontfamily = getOption("annTextFamily"), fontface = tt$fontface)
  }
  return(pp)
}

# ustawienia / settings ----
options(
  annTextFamily = "quicksand",
  otherTextFamily = "montserrat"
)

font_add_google(name = "Montserrat", family = "montserrat", regular.wt = 400) # need package 'curl for this
font_add_google(name = "Quicksand", family = "quicksand", regular.wt = 400)
showtext_auto()

# data ----
# comes from https://stat.gov.pl/obszary-tematyczne/ludnosc/statystyka-przyczyn-zgonow/zgony-wedlug-przyczyn-okreslanych-jako-garbage-codes,3,3.html
file <- "garbage_codes/garbage_codes_lista_rozszerzona_2000_2005_2010-2019.xlsx"

dataAll <- rbindlist(
  l = lapply(
    X = 2010:2019,
    FUN = function(x)
    {
      sName <- sprintf("%s_lista rozszerzona", x)
      message(x)
      d <- data.table(read.xlsx(xlsxFile = file, sheet = sName, startRow = ifelse(x < 2013, 4, 6)))
      setnames(d, "X3", "Polska")
      d <- melt(data = d, id.vars = c("Kod.ICD", "Opis"), variable.factor = F, value.factor = F)
      d[, Rok := x]
      d
    }
  )
)

dataChart <- dataAll[Kod.ICD == "w % do zgonów ogółem"]
dataChart[, value := as.numeric(value)]

# lvls
head(dataAll)
dataAll[, ICD := trimws(Kod.ICD)]
# based on Iburg KM, Mikkelsen L, Adair T, Lopez AD (2020) Are cause of death data fit for purpose? Evidence from 20 countries at different levels of socio-economic development.
garbageLvls <- data.table(read.table(file = "garbage_codes/garbage_lvls.txt", sep = ";", stringsAsFactors = F))

dataIcd <- dataAll[!ICD %in% c("Garbage codes", "w % do zgonów ogółem", "w tym:", "Zgony OGÓŁEM")]
dataIcd[, value := as.numeric(value)]
dataIcd[is.na(value), value := 0]
# sprawdzenie czy garbage codes ogolem vs lista daje to samo
ag <- dataIcd[,.(value = sum(value)), by = .(Rok, variable)]
f <- merge(ag, dataAll[ICD == "Garbage codes",.(Rok, variable, r = value)], by = c("Rok", "variable"))
f[r != value]

garbageLvlsSplitted <- rbindlist(
  l = lapply(
    X = 1:4,
    FUN = function(lvl){
      splitted <- trimws(unlist(strsplit(x = garbageLvls[V1 == lvl]$V2, split = ",")))
      icdVector <- unlist(lapply(X = splitted, FUN = function(x){
        g <- trimws(unlist(strsplit(x = x, split = "-")))
        if (length(g) == 1)
        {
          return(g)  
        } else
        {
          if (nchar(g[1]) == 3)
          {
            return(g)
          } else
          {
            base <- substr(g[1], 1, 3)
            start <- substr(g[1], 5, 5)
            end <- substr(g[2], 5, 5)
            paste0(base, ".", c(as.numeric(start):as.numeric(end)))
          }
          
        }
      }))
      data.table(lvl = lvl, icd = icdVector)
    }
  )
)

garbageLvlsSplitted[, base := substr(icd, 1, 3)]
nrow(garbageLvlsSplitted[,.(length(unique(lvl))), by = .(base)][V1 > 1])

table(garbageLvlsSplitted[,.(length(unique(lvl)), paste0(sort(unique(lvl)), collapse = "|")), by = .(base)][V1 > 1]$V2)

# can we separate this
merge(garbageLvlsSplitted[,.(length(unique(lvl)), paste0(sort(unique(lvl)), collapse = "|")), by = .(base)][V1 > 1],
      dataIcd[,.(sum(value)), by = .(base = substr(ICD, 1, 3))], by = "base")

problematic <- merge(garbageLvlsSplitted[,.(length(unique(lvl)), paste0(sort(unique(lvl)), collapse = "|")), by = .(base)][V1 > 1],
                     dataIcd[,.(sum(value)), by = .(base = substr(ICD, 1, 3))], by = "base")

dataIcd[substr(ICD,1,3) %in% problematic$base & variable == "Polska",.(sum(value)), by = .(ICD)]
garbageLvlsSplitted[icd %in% dataIcd[substr(ICD,1,3) %in% problematic$base & variable == "Polska",.(sum(value)), by = .(ICD)]$ICD]

dataIcdLvls <- merge(dataIcd[,.(Rok, variable, value, ICD)], garbageLvlsSplitted[,.(ICD = icd, lvl)], all.x = T)
dataIcdLvls[variable == "Polska" & is.na(lvl),.(sum(value)), by = .(Rok)]
sort(unique(dataIcdLvls[is.na(lvl)]$ICD))
garbageLvlsSplitted[icd %like% "I51"]

# imputing data
dataIcdLvls[ICD %in% c("N18", "R95", "Y15"), lvl := 1]
dcast(dataIcdLvls[is.na(lvl)][variable == "Polska"], ICD ~ Rok, value.var = "value")

# i'm not a medic
dataIcdLvls <- dataIcdLvls[!is.na(lvl)]
dataIcdLvls <- dataIcdLvls[,.(sum(value)), by = .(Rok, variable, lvl)]
dataIcdLvls <- merge(dataIcdLvls, dataAll[ICD == 'Zgony OGÓŁEM',.(Rok, variable, ogol = value)], by = c("Rok", "variable"))
dataIcdLvls[, ods := V1 / as.numeric(ogol)]

dd <- dcast(data = dataIcdLvls, variable + lvl ~ Rok, value.var = "ods")
dd[, v := `2019` > `2010`]

# visualization / grafika ----
chosen <- c("Lubelskie", "Polska", "Opolskie", "Pomorskie")
colorsC <- c("#CD7672", "#56C596", "#534666", "#138086")
backgCol <- "#f0be77"
names(colorsC) <- chosen

base_size <- 30
title_size <- 2.25 * base_size
capt_size <- .85 * base_size
lege_size <- 1.5 * base_size
axtext_size <- 1.2 * base_size
an_size <- axtext_size / ggplot2::.pt

sett_chart <- theme(
  line = element_line(colour = "grey30"),
  text = element_text(family = getOption("otherTextFamily"), size = base_size), 
  axis.text = element_text(size = axtext_size),
  axis.ticks = element_blank(), 
  legend.background = element_blank(), 
  legend.key = element_blank(),
  legend.text = element_text(size = lege_size),
  panel.background = element_blank(), 
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(color = "grey90"),
  plot.background = element_rect(colour = backgCol, fill = backgCol),
  plot.title = element_text(family = getOption("annTextFamily"), size = title_size, face = "bold"),
  plot.caption = element_text(family = getOption("annTextFamily"), size = capt_size, hjust = 0, lineheight = 0.25),
  strip.background = element_blank(), 
  strip.text = element_text(size = axtext_size, margin = margin(t = -10), color = "grey30")
)

segs <- data.table(
  x = 2010-.5, y = c(0,20,40,60), 
  l = c("0", "20", "40", "60% wszystkich zgonów"), 
  xend = c(2019, 2019, 2019, 2016), 
  ll = c("solid", "dashed", "dashed", "dashed"),
  id = 1:4
)

gora <- ggplot() + 
  geom_segment(data = segs[y != 0], mapping = aes(x = x, y = y, yend = y, xend = xend, group = id), linetype = "dashed", color = "grey90", size = .9) +
  geom_segment(data = segs[y == 0], mapping = aes(x = x, y = y, yend = y, xend = xend, group = id), linetype = "solid", color = "grey90", size = .9) +
  geom_text(data = segs, mapping = aes(x = x, y = y, label = l), hjust = 0, vjust = -0.5, size = an_size, family = getOption("otherTextFamily"), color = "grey30") +
  ggbump::geom_bump(data = dataChart[!variable %in% chosen], mapping = aes(x = Rok, y = value, group = variable), color = "gray90", size = 1, alpha = .75) + 
  lapply(
    X = chosen,
    FUN = function(i) {
      ggbump::geom_bump(data = dataChart[variable == i], mapping = aes(x = Rok, y = value, group = variable), color = colorsC[i], size = 1.51)
    }
  ) +
  sett_chart + 
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank()) +
  scale_y_continuous(limits = function(x) c(0, max(x) * 1.05)) + 
  scale_x_continuous(breaks = 2010:2019, expand = c(0, .25)) +
  labs(x = NULL,  y = NULL)

takNie <- colorsC[c(3, 4)]
names(takNie) <- c(TRUE, FALSE)

lvls_status <- c(
  `1` = "Poziom I",
  `2` = "II",
  `3` = "III",
  `4` = "IV"
)

dd[, variable2 := factor(variable, levels = dd[lvl == 1][order(`2019`)]$variable)]

dol <- ggplot() +
  geom_segment(data = dd, mapping = aes(x = `2010`, xend = `2019`, y = variable2, yend = variable2, color = v), size = 1) +
  geom_point(data = dd, mapping = aes(x = `2019`, y = variable2, color = v), size = 2.5) +
  facet_wrap(~lvl, scales = "free_x", labeller = labeller(lvl = lvls_status), nrow = 1) + 
  scale_x_continuous(
    limits = function(x) c(ifelse(min(x) < .01, -max(x) / 10, 0), max(x) * 1.05),
    labels = function(x) c(format(100 * x[-(length(x))], decimal.mark = ",", drop0trailing = T), paste0(format(100*x[length(x)]), "%")), 
    expand = c(0, 0.005), 
    breaks = function(x){j <- floor(100 * max(x)) / 100; c(0, round(j/2, ifelse(round(100 * j, 0)%%2 == 0, 2, 3)), round(j, 2))}
  ) + 
  labs(y = NULL, x = NULL, color = NULL) + 
  scale_color_manual(values = takNie) +
  sett_chart +
  theme(
    legend.position = "none", panel.grid.major.x = element_line(colour = "grey90"), panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(x = .05, units = "npc"),
    plot.margin = margin(l = -50, r = 15, b = -25)
  )

title <- ggdraw() + sett_chart +
  draw_label_theme(
    label = "Garbage (codes) in - garbage (actions) out", 
    theme = sett_chart, element = "plot.title", x = 0.05, hjust = 0, vjust = 0.5
  )

caption <- ggdraw() + sett_chart +
  draw_label_theme(
    label = "Dane: https://stat.gov.pl/obszary-tematyczne/ludnosc/statystyka-przyczyn-zgonow/zgony-wedlug-przyczyn-okreslanych-jako-garbage-codes,3,3.html\nInspiracja: Iburg KM, Mikkelsen L, Adair T, Lopez AD (2020) Are cause of death data fit for purpose? Evidence from 20 countries at different levels of socio-economic development.\nViz: @DominikZabinski",
    theme = sett_chart, element = "plot.caption", x = 0.05, hjust = 0, vjust = 0.5
  )

blank_sp <- ggdraw() + sett_chart
rh <- c(0.20, 1.25, 1.15, 0.2)

aa <- plot_grid(
  title, 
  plot_grid(gora, blank_sp, nrow = 1, rel_widths = c(6, 3)), 
  dol, caption, 
  ncol = 1, rel_heights = rh
)

marg <- .035
stepT <- .02
startT <- .735
tt2 <- c(
  "Polska" = "W skali kraju udział wahał się \nmiędzy 29% (2019) a 32% (2015).", 
  "Lubelskie" = "Najczęściej używa się ich\nw lubelskim: w całym okresie\nudział wahał między 37% a 42%.",
  "Opolskie" = "Największy postęp odnotowano\nw opolskim: z 55% w 2016 udział\ngarbage codes spadł do 10%.", 
  "Pomorskie" = "Najrzadziej garbage codes\nstosuje się w woj. pomorskim:\nudział spadł z 27% do 5%." 
)
oo <- unlist(lapply(X = tt2, FUN = function(i) length(unlist(strsplit(x = i, split = "\n", fixed = T)))))
kol <- c("Polska", "Lubelskie", "Opolskie", "Pomorskie") 
dop <- lapply(
  X = kol, 
  FUN = function(i)
  {
    k <- which(kol == i)
    ile <- sum(oo[1:k]) - oo[i]
    list(
      l = tt2[i],
      x = 1 - marg, y = startT - ile * stepT,
      color = colorsC[i], fontface = "bold"
    )
  }
)

teksty <- list(
  list(l = "Garbage codes - określenie tych kodów ICD-10,\nktórych używanie uniemożliwia precyzyjne\nokreślenie przyczyny zgonu, a przy dużej skali\nplanowania w zakresie zdrowia publicznego.", x = 1 - marg, y = .9),
  list(l = "Używanie garbage codes różni się\nnie tylko między województwami,\nale również w czasie.", x = 1- marg, y = .81),
  list(l = "Garbage codes można podzielić\nze względu na to jak wielki wpływ\nma ich używanie na działania\ni planowanie w zakresie\nzdrowia publicznego.\nWyróżnia się 4 poziomy\ngarbage codes. \nPoziom I to największy potencjał,\nkryjący najwięcej informacji.\nPoziom IV to kody, których\ndokładność nie wpływa\nznacznie na plany dot.\nzdrowia publicznego.", x = marg, y = .45, hjust = 0),
  list(l = "W latach 2010-2019", x = marg, y = .179, hjust = 0),
  list(l = "w 9 województwach udział\nkodów poziomu I zwiększył się,", x = marg, y = .159, hjust = 0, color = takNie["TRUE"], fontface = "bold"),
  list(l = "zaś w 10 województwach udział\nkodów poziomu II spadł.", x = marg, y = .119, hjust = 0, color = takNie["FALSE"], fontface = "bold")
)

teksty <- append(x = teksty, values = dop)

aa <- add_labels(aa, teksty)

szer <- 800 * 1 / 96

ggsave(plot = aa, filename = "garbage_codes/2010-2019_garbage_combo.png", width = szer, height = 2 * szer * 9 / 16, units = "in", dpi = 720/2)
file.show("garbage_codes/2010-2019_garbage_combo.png")
