# inspiration: https://twitter.com/_rospearce/status/1410903833442717698/photo/1
# biblioteki / libraries ----
library(data.table)
library(ggplot2)
library(showtext)
library(stringr)
library(ggimage)
library(patchwork)
font_add_google(name = "Montserrat", family = "montserrat", regular.wt = 400) # need package 'curl for this
font_add_google(name = "Quicksand", family = "quicksand", regular.wt = 400)
showtext_auto()

# ustawienia / settings ----
options(
    annTextSize = 14,
    linesTextSize = 20,
    annTextFamily = "quicksand",
    lineSizeHighlighted = 1.75,
    otherTextFamily = "montserrat",
    scaleIt = 15,
    sizeDotted = .5,
    maxDataY = 170
)

# funkcje / functions ----
f_labl <- function(x, roman = F)
{
    r <- floor(x / 12)
    m <- x - r * 12 + 1
    if (roman == T)
    {
        paste0(as.roman(m), " ", r)
    } else
    {
        format(as.Date(sprintf("%s-%02d-01", r, m)), "%Y %B")
    }
}

rob_ramke <- function(ff, ii)
{
    danePlot <- copy(ff[Produkty.rolne == ii])
    danePlot[, val2 := Wartosc]
    danePlotSl <- unique(danePlot[,.(Rok, Miesiące)])
    danePlotSl[, m := factor(x = Miesiące, levels = c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień"))]
    danePlotSl[, n := Rok * 12 + as.numeric(m) - 1]
    
    danePlot <- merge(danePlot, danePlotSl, by = c("Rok", "Miesiące"))
    danePlot[, type := Nazwa]
    
    # data for background
    dR <- rbindlist(
        l = lapply(
            X = unique(danePlot$n), 
            FUN = function(x) 
                data.table(n = x, ymin = min(danePlot[n == x]$val2), ymax = max(danePlot[n == x]$val2))
        )
    )
    
    # wzgledna i bezwzgledna roznica - kiedy min i max
    dR[, rel := ymax / ymin - 1]
    dR[, abs := ymax - ymin]
    
    miniMaxiDiff <- list(
        smallestAbs = dR[abs == min(abs)],
        smallestRel = dR[rel == min(rel)],
        biggestAbs = dR[abs == max(abs)],
        biggestRel = dR[rel == max(rel)]
    )
    
    # najdluzszy okres bycia minimum i maksimum
    doOkresow <- copy(danePlot[,.(Nazwa, n, val2)])
    doOkresow[, valMin := min(val2), by = .(n)]
    doOkresow[, valMax := max(val2), by = .(n)]
    doOkresow[, typ := ""]
    doOkresow[val2 == valMax, typ := "top"]
    doOkresow[val2 == valMin, typ := "bottom"]
    doOkresow <- doOkresow[order(Nazwa, n)]
    doOkresow[, typRleid := rleid(typ), by = .(Nazwa)]
    doOkresowN <- doOkresow[typ != "",.(.N, typ = unique(typ), min(n), f_labl(min(n)), max(n), f_labl(max(n))), by = .(Nazwa, typRleid)]
    
    # najdluzsze serie
    doOkresowN[, maxStreak := max(N), by = .(typ)]
    
    return(list(
        danePlot = danePlot,
        dR = dR,
        diffs = miniMaxiDiff,
        longestStreak = doOkresowN[N == maxStreak],
        lastTops = doOkresowN[V5 == max(V5)]
    ))
}

#' pomocnicze linie siatki
#'
#' @param dt 
#' @param pp 
#' @param colorT 
#' @param colorL 
#'
#' @return
#' @export
#'
#' @examples
pomoc_siat <- function(dt, pp = seq(0, 30, 10), colorT = "grey70", colorL = "grey90")
{
    list(
        geom_text(data = data.table(x = min(dt$n) - 5, y = pp), aes(x = x, y = y, label = y), hjust = 0, vjust = -1, size = getOption("linesTextSize"), color = colorT, family = getOption("otherTextFamily")),
        geom_segment(data = data.table(x = min(dt$n) - 5, xend = max(dt$n) + 5, y = pp, yend = pp), aes(x = x, y = y, xend = xend, yend = yend), color = colorL)
    )
}

#' Title
#'
#' @param nf 
#' @param onTheRight 
#' @param txt 
#' @param posTxtTop 
#' @param up 
#' @param dR 
#' @param sD 
#' @param br 
#'
#' @return
#' @export
#'
#' @examples
put_diff <- function(nf = 24072, onTheRight = T, txt = c("Różnica na początku 2006", "wynosiła 21.8% i było to najmniej wzglednie i bezwzglednie (3.4zł)"), posTxtTop = 12.5, up = T, dR, sD = getOption("maxDataY"), br = 20)
{
    if (!is.null(br))
    {
        txt <- str_wrap(txt, width = br)
        txt <- unlist(strsplit(x = txt, split = "\n", fixed = T))
    }
    dTtxt <- data.table(n = nf + ifelse(onTheRight, 1, -1), t = txt)
    dTtxt[, y := seq(from = posTxtTop, by = -(sD / 30) * getOption("annTextSize") / getOption("scaleIt"), length.out = nrow(dTtxt))]
    
    y0 <- min(dTtxt$y)
    y1 <- dR[n == nf]$ymax
    
    if (up == F)
    {
        y0 <- dR[n == nf]$ymin
        y1 <- max(dTtxt$y)
    }
    
    list(
        geom_text(data = dTtxt, aes(x = n, y = y, label = t), hjust = ifelse(onTheRight, 0, 1), size = getOption("annTextSize"), family = getOption("annTextFamily")), 
        geom_segment(
            data = data.table(x = nf, xend = nf, y = y0, yend = y1), 
            aes(x = x, y = y, xend = xend, yend = yend), linetype = "dotted", size = getOption("sizeDotted")
        )    
    )
}

#' Title
#'
#' @param dR 
#' @param nonChartEnd 
#' @param txt 
#' @param nf 
#' @param nR 
#' @param type 
#' @param sD 
#' @param over 
#' @param br 
#'
#' @return
#' @export
#'
#' @examples
draw_series <- function(dR, nonChartEnd = 18, txt = c("Podlaskie od kwietnia 2012 do lipca 2016", "(4 lata 4 miesięce)"), nf = 24147, nR = 24198, type = "PODLASKIE", sD = getOption("maxDataY"), over = T, br = 20)
{
    if (!is.null(br))
    {
        txt <- str_wrap(txt, width = br)
        txt <- unlist(strsplit(x = txt, split = "\n", fixed = T))
    }
    lineSep <- (sD / 30) * getOption("annTextSize") / getOption("scaleIt")
    dTtxt <- data.table(n = nf + 1, t = txt)
    dTtxt[, y := seq(from = nonChartEnd, by = -lineSep, length.out = nrow(dTtxt))]
    
    dataR <- dR[data.table::between(n, nf, nR, incbounds = T),.(n, mini = ymax, maxi = nonChartEnd + lineSep/2)]
    if (over == F)
    {
        dataR <- dR[data.table::between(n, nf, nR, incbounds = T),.(n, mini = min(dTtxt$y) - lineSep, maxi = ymin)]
    }
    
    list(
        geom_ribbon(data = dataR, aes(x = n, ymin = mini, ymax = maxi), fill = kolory[type], alpha = .5),
        geom_text(data = dTtxt, aes(x = n, y = y, label = t), hjust = 0, size = getOption("annTextSize"), vjust = 1, family = getOption("annTextFamily"))
    )
}

#' Title
#'
#' @param daneSig 
#'
#' @return
#' @export
#'
#' @examples
highlighted_void <- function(daneSig)
{
    lapply(
        X = types,
        FUN = function(x)
            list(
                geom_path(data = daneSig[type == x][order(n)], aes(x = n, y = val2, group = type), color = "white", size = getOption("lineSizeHighlighted") * 1.5, lineend = "round"),
                geom_path(data = daneSig[type == x][order(n)], aes(x = n, y = val2, group = type, color = as.factor(type)), size = getOption("lineSizeHighlighted"), lineend = "round")   
            )
    )
}

# dane / data ----
# dane pochodzą z https://bdl.stat.gov.pl/ i jest to wskaźnik 2967: Przeciętne ceny skupu ważniejszych produktów rolnych (dane miesięczne) / data comes from https://bdl.stat.gov.pl/ and it is an indicator 2967: Average purchase prices of major agricultural products (monthly data)
dataFile <- "CENY_2967_CREL_20210704152741"
daneCeny <- data.table(read.csv2(unzip(zipfile = paste0("cow_milk_prices/", dataFile, ".zip"), file = paste0(dataFile, ".csv")), encoding = "UTF-8"))
daneCeny[1]
daneCeny[,.(.N), by = .(Produkty.rolne)]
setnames(daneCeny, c("Okresy"), c("Miesiące"))

# mleko krowie / cow milk ----
ramka_f <- rob_ramke(ff = daneCeny[!is.na(Wartosc)], ii = "mleko krowie za 1 hl")
ramka_f$danePlot
ramka_f$dR
ramka_f$diffs
ramka_f$longestStreak
ramka_f$lastTops

types <- c("PODLASKIE", "MAŁOPOLSKIE")
kolory <- c("#ac3e31", "#dbae58", "#20283e")[1:length(types)]
names(kolory) <- types
daneSig <- ramka_f$danePlot[type %in% types]

wykres <- ggplot() +
    pomoc_siat(dt = ramka_f$danePlot, pp = c(0, 50, 100, 150), colorL = "grey95") +
    # geom_ribbon jako tlo
    geom_ribbon(data = ramka_f$dR, aes(x = n, ymin = ymin, ymax = ymax), fill = "#fafafa") +
    
    # roznice
    put_diff(nf = 24145, onTheRight = T, up = T, dR = ramka_f$dR, posTxtTop = 95, txt = "Największa względna różnica między skrajnymi województwami wystąpiła w II 2012 - 26,95%.", br = 32) + 
    put_diff(nf = 24213, onTheRight = T, up = T, dR = ramka_f$dR, posTxtTop = 110, txt = "W X 2017 odnotowano największą bezwzględną różnicę: 28,26 zł.", br = 25) + 
    put_diff(nf = 24234, onTheRight = F, up = T, dR = ramka_f$dR, posTxtTop = 75, txt = c("VII 2019 był miesiącem, w którym", "zarówno względna jak i bezwzględna", "różnica była najmniejsza", "(odpowiednio 10,7% i 13,09 zł)."), br = NULL) +
    
    # niewyroznione wojewodztwa
    geom_path(data = ramka_f$danePlot[!type %in% types][order(type, n)], aes(x = n, y = val2, group = type), alpha = .05, size = getOption("lineSizeHighlighted") / 2) +
    
    # serie
    draw_series(dR = ramka_f$dR, nonChartEnd = 164, txt = "Najdłużej województwo utrzymywało najwyższą cenę od XII 2015 do IX 2017 (podlaskie).", nf = 24191, nR = 24212, type = "PODLASKIE", over = T, br = 26) + 
    draw_series(dR = ramka_f$dR, txt = c("W małopolsce od", "XII 2019 utrzymuje", "się najniższa cena", "w kraju. Jest to", "najdłuższa seria", "tego typu."), nonChartEnd = 115, nf = 24239, nR = 24256, type = "MAŁOPOLSKIE", over = F, br = NULL) +
    
    # wojewodztwa wyroznione
    highlighted_void(daneSig = daneSig) +
    lapply(
        X = types,
        FUN = function(x)
            geom_text(data = ramka_f$danePlot[type == x][n == max(n)], aes(x = n + 1, y = val2, label = type), color = kolory[x], size = getOption("annTextSize") + 4, hjust = 0, fontface = "bold")
    ) +
    scale_color_manual(values = kolory) +
    scale_y_continuous(limits = function(x) c(0, getOption("maxDataY")), expand = c(0, 0)) +
    scale_x_continuous(labels = function(x) f_labl(x, roman = F), expand = c(0, 0), limits = function(x) c(min(x), max(x) + 10), breaks = seq(from = min(ramka_f$danePlot$n) + 12, by = 24, length.out = 5)) +
    theme_minimal() + 
    theme(
        panel.grid = element_blank(), 
        legend.position = "none", 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = getOption("linesTextSize") * ggplot2::.pt, color = "grey70"),
        axis.ticks.x = element_line(color = "grey90", size = .5),
        axis.title = element_blank(),
        plot.title = element_text(family = getOption("otherTextFamily"), size = (getOption("linesTextSize") + 8) * ggplot2::.pt),
        plot.subtitle = element_text(family = getOption("otherTextFamily"), size = (getOption("linesTextSize") + 2) * ggplot2::.pt),
        plot.caption = element_text(family = getOption("otherTextFamily"), size = (getOption("linesTextSize") - 4) * ggplot2::.pt)
    ) +
    ggtitle(label = "Mleko krowie - cena za 1 hl [zł]", subtitle = "Dane miesięczne I 2012 - V 2021") + 
    labs(caption = "Dane: bdl.stat.gov.pl | Viz: @DominikZabinski")

ggsave(filename = "cow_milk_prices/ceny_mleko_krowie.png", height = 4, width = 7, dpi = 720, plot = wykres)
