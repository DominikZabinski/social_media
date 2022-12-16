# biblioteki / libraries ----
library(data.table) # przetwarzanie tabel                         / handling tables
library(rvest)      # przetwarzanie tresci ze stron internetowych / handling content from web
library(ggplot2)    # glowna biblioteka do wykresow               / main charting library
library(colorspace) # biblioteka do kolorow                       / handling colors
library(stringr)    # operacje na stringach                       / handling strings
library(ggtext)     # rozszerzenie ggplot2 dla stringow           / ggplot2 extension for strings

# funkcje / functions ----
trans_it <- function(x, a, b, c, d)
{
    (x - a) / (b - a) * (d - c) + c
}
# pobranie i przygotowanie danych / downloading and preparing data ----
# pobranie tresci strony i odfiltrowanie tabel / download webpage nad filter out tables
url <- "https://zoneapp.gunb.gov.pl/ranking/"

df <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)
saveRDS(object = df, file = "ceeb/data.rds")


allDataVoid <- rbindlist(
    l = lapply(
        X = 1:16,
        FUN = function(i)
        {
            dt <- as.data.table(df[[i + 5]])
            dt[, ow := i]
            return(dt[])
        }
    )
)

allDataVoid[, ods := `Liczba punktów adresowych z co najmniej 1 złożoną deklaracją` / `Liczba punktów adresowych`]

all2 <- copy(allDataVoid[,.(Gmina, mian = `Liczba punktów adresowych`, ods = `Liczba punktów adresowych z co najmniej 1 złożoną deklaracją` / `Liczba punktów adresowych`, ow)])

kolejnoscOw <- all2[,.(odsOw = weighted.mean(x = ods, w = mian)), by = .(ow)]
kolejnoscOw
all2[, ow2 := factor(x = ow, levels = kolejnoscOw[order(odsOw)]$ow)]

all2 <- all2[order(ow2, ods)]
all2[, id2 := 1:nrow(all2)]


all2[, al := (1 - mian/max(mian)) ^ 10]
all2[, al2 := trans_it(al, 0, 1, .3, 1)]
podsumOw <- all2[,.(
    idOw = weighted.mean(x = id2, w = mian), 
    mn = min(id2), mx = max(id2),
    mm = median(ods), sr = weighted.mean(x = ods, w = mian)), by = .(ow)]

sl <- list(
    'dolnośląskie', "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", "małopolskie", "mazowieckie", "opolskie",
    "podkarpackie", "podlaskie", "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"
)

sl2 <- data.table(s = 1:16, n = unlist(sl))
podsumOw$n <- sl2[match(podsumOw$ow, table = sl2$s)]$n
podsumOw[, ow2 := factor(x = ow, levels = kolejnoscOw[order(odsOw)]$ow)]
podsumOw <- podsumOw[order(ow2)]

kolorAAA <- "#64b441"
kolorSrednia <- lighten(kolorAAA, amount = .5)
kolorMediana <- darken(kolorAAA, amount = .5)
fff <- 3

podsumOw$ll <- unlist(
    lapply(
        X = 1:nrow(podsumOw), 
        FUN = function(i) str_interp(
            string = "<span style='font-size:${scrFS2}pt;'>${ow}</span><br><span style='font-size:${scrFS}pt;'>(<span style='color:${kS}'>${a}</span>; <span style='color:${kM}'>${b}</span>)</span>", 
            env = list(
                ow = podsumOw$n[i], 
                scrFS = fff,
                scrFS2 = fff + 2,
                a = scales::percent(podsumOw$sr[i]), 
                kS = kolorSrednia,
                b = scales::percent(podsumOw$mm[i]),
                kM = kolorMediana
            )
        )
    )
)

i <- 16

podsumOw$ll[i] <- str_interp(
    string = "<span style='font-size:${scrFS2}pt;'>${ow}</span><br><span style='font-size:${scrFS}pt;'>(<span style='color:${kS}'>średnia: ${a}</span>; <span style='color:${kM}'>mediana: ${b}</span>)</span>", 
    env = list(
        ow = podsumOw$n[i], 
        scrFS = fff,
        scrFS2 = fff + 2,
        a = scales::percent(podsumOw$sr[i]), 
        kS = kolorSrednia,
        b = scales::percent(podsumOw$mm[i]),
        kM = kolorMediana
    )
)

kolejnosc <- all2[order(-mian)]$id2
medPl <- all2[,.(m = median(ods))]$m
srPl <- all2[,.(m = weighted.mean(ods, w = mian))]$m

plotTitle <- "Jest lepiej, ale czy wystarczająco?"
subTitle <- str_interp(
    string = "ogólnopolski poziom wypełnienia CEEB to <span style='color:${sK}'>${sW}</span> przy medianie równej <span style='color:${mK}'>${mW}</span>", 
    env = list(mK = kolorMediana, mW = scales::percent(medPl), sK = kolorSrednia, sW = scales::percent(srPl))
)

kolor <- "#1e2c69"
kolor <- colorspace::adjust_transparency(kolor, alpha = .7)
pp <- ggplot() + 
    lapply(
        X = 1:nrow(podsumOw),
        FUN = function(i)
        {
            if (i %% 2 == 0) return(NULL)
            dd <- podsumOw[i]
            geom_rect(mapping = aes(xmin = 0, xmax = 1, ymin = dd$mn, ymax = dd$mx), fill = "white", color = "white")
        }
    ) +
    geom_vline(xintercept = medPl, color = kolorMediana) +
    geom_vline(xintercept = srPl, color = kolorSrednia) +
    geom_vline(xintercept = c(.25, .50, .75), color = 'grey70', size = .1) +
    lapply(
        X = kolejnosc,
        FUN = function(i)
        {
            kk <- colorspace::lighten(col = kolor, amount = 1 - all2[id2 == i]$al2)
            geom_point(data = all2[id2 == i], aes(x = ods, y = id2, size = mian), shape = 21, stroke = .1, fill = kk, color = 'grey99')
        }
    ) +
    geom_richtext(data = podsumOw, aes(x = 0, y = mx, label = ll), fill = NA, label.color = NA, hjust = 0, size = fff/.pt, lineheight = 1, vjust = .8) +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0.025), limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(expand = c(0, 100)) +
    scale_size_continuous(
        trans = "identity", limits = range(all2$mian), range = c(1, 15), 
        breaks = unname(quantile(all2$mian))
    ) +
    labs(
        x = NULL, y = NULL, 
        size = "Liczba punktów\nadresowych", 
        title = plotTitle,
        subtitle = subTitle,
        caption = "poziom wypełnienia bazy CEEB z dokładnością do gmin\nDane: https://zoneapp.gunb.gov.pl/ranking/, stan na 7 lipca 2022 11:03\nOpracowanie @DominikZabinski"
        ) + 
    theme(
        legend.position = "bottom", 
        panel.grid = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#f1f2f2", colour = "#f1f2f2"),
        plot.title = element_text(size = 8, lineheight = .5, margin = margin(b = 0, unit = "pt")),
        plot.subtitle = element_markdown(size = 6, lineheight = .5),
        plot.caption = element_text(size = 5, face = "italic", lineheight = 1),
        axis.text.x = element_text(size = 6),
        legend.title = element_text(size = 7, lineheight = .75),
        legend.text = element_text(size = 5, lineheight = .75)
    )

ggsave(plot = pp, filename = "ceeb/ceeb_220708.png", width = 4, height = 6, dpi = 1440, bg = "white")
