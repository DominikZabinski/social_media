# libraries ----
library(data.table)
library(ggplot2)
library(bezier)

# functions ----
transf <- function(x, a, b, c, d)
{
    (d - c) * (x - a) / (b - a) + c
}

# creates a symmetric arc
arc_me <- function(x0, y0, r, t, obs, a = min(obs), b = max(obs))
{
    t <- transf(obs, a, b, t[1], t[2])
    data.table(x = r * sin(t) + x0, y = r * cos(t) + y0)
}

crt_beziers <- function(obs, gr_set, arc_set)
{
    rbindlist(
        l = lapply(
            X = 1:nrow(obs),
            FUN = function(i)
            {
                ws <- gr_set[gr_set$id_s == obs$id_s[i]]
                t <- seq(0, 1, length=100)
                ii <- mean(c(obs$x[i], ws$x))
                hh <- mean(c(obs$y[i], arc_set$r + arc_set$y0))
                w1 <- .05
                ii2 <- w1 * obs$x[i] + (1 - w1) * ws$x
                jj2 <- w1 * obs$y[i] + (1 - w1) * (arc_set$r + arc_set$y0)
                jj <- matrix(
                    nrow = 4,
                    byrow = T,
                    data = c(
                        ws$x, 0, 
                        ii2, jj2,
                        ii, hh,
                        obs$x[i], obs$y[i]
                    )
                )
                bezier_points <- bezier(t=t, p=jj)
                gg <- data.table(bezier_points)
                gg[, id := i]
                gg[, ids := obs$id_s[i]]
                return(gg)
            }
        )
    )
}

# an ----
# width of arc
oo <- .45
# another arc settings
to_arc <- list(x0 = 0, y0 = 4, r = 3)
ff <- arc_me(x0 = to_arc$x0, y0 = to_arc$y0, r = to_arc$r, t = c(-pi * oo, pi * oo), obs = c(1, 2, 3, 6))
# declaring groups
subs <- data.table(id_s = c("A", "B"), y = 0, x = c(-0.5, 0.5))
# assigning observations to groups
ff[, id_s := sample(x = subs$id_s, size = nrow(ff), replace = T)]

# creating bezier curves for each observation
beziers <- rbindlist(
    l = lapply(
        X = 1:nrow(ff),
        FUN = function(i)
        {
            ws <- subs[id_s == ff$id_s[i]]
            t <- seq(0, 1, length=100)
            ii <- mean(c(ff$x[i], ws$x))
            hh <- mean(c(ff$y[i], to_arc$r + to_arc$y0))
            jj <- matrix(data = c(ws$x, 0, ii, hh, ff$x[i], ff$y[i]), nrow = 3, byrow = T)
            bezier_points <- bezier(t=t, p=jj)
            gg <- data.table(bezier_points)
            gg[, id := i]
            gg[, ids := ff$id_s[i]]
            return(gg)
        }
    )
)


ggplot() +
    geom_point(data = ff, aes(x = x, y = y), color = "red") +
    geom_line(data = beziers, aes(x = V1, y = V2, group = id, color = ids))

# rl example
bb <- data.table(read.csv2(file = unzip(zipfile = "bezier_tree/RYNE_2392_CREL_20230224230323.zip", files = "RYNE_2392_CREL_20230224230323.csv")))
bb[, KK := sprintf("%07d", as.numeric(Kod))]
bb[, w := substr(KK, 1, 2)]
bb <- bb[order(Wartosc)]

new_arc <- list(x0 = 0, y0 = 4, r = 5, j = .55)
arc_from_set <- arc_me(x0 = new_arc$x0, y0 = new_arc$y0, r = new_arc$r, t = new_arc$j * pi * c(-1, 1), obs = bb$Wartosc, a = 0)
arc_from_set$id_s <- bb$w

oo <- sort(unique(bb$w))
new_groups <- bb[,.(V1 = mean(Wartosc), Vmx = max(Wartosc)), by = .(id_s = w)]
new_groups[, y := 0]
toW <- 2.5
new_groups[, x := transf(x = V1, 0, max(V1), -toW, toW)]

bezier2 <- crt_beziers(obs = arc_from_set, gr_set = new_groups, arc_set = new_arc)

pall <- c(
    "#a27c64", # yellow + brown
    "#6a4e42",
    "#0d0a0c",
    "#f3ddb4", # yellow + white
    "#3e2c29",
    "#4a4444",
    "#463c44",
    "#838482",
    "#807c84"
)

highlight_me <- function(arc, line, toH, colors = list(linesH = "#463c44", linesNH = "#6a4e42", pointsH = "#463c44", pointsNH = "#6a4e42"))
{
    ggplot() + 
        geom_line(data = line[ids != toH], aes(x = V1, y = V2, group = id), size = .75, color = colors$linesNH, alpha = .1) + 
        geom_point(data = arc[id_s != toH], aes(x = x, y = y), color = colors$pointsNH, size = 2, alpha = .4) +
        geom_line(data = line[ids == toH], aes(x = V1, y = V2, group = id), size = .75, color = colors$linesH, alpha = .8) + 
        geom_point(data = arc[id_s == toH], aes(x = x, y = y), fill = colors$pointsH, size = 2, shape = 21, color = "#f3ddb4", alpha = .8) +
        theme_minimal()
}

# geom path text for the arc to describe values?
ll <- range(c(arc_from_set$x, bezier2$V1))
kk <- range(c(arc_from_set$y, bezier2$V2))
sk <- (ll[2] - ll[1]) / (kk[2] - kk[1])
lo <- arc_me(x0 = new_arc$x0, y0 = new_arc$y0, r = new_arc$r * 1.15, t = new_arc$j * pi * c(-1, 1), obs = c(0:100))

i <- "06"
pp <- highlight_me(arc = arc_from_set, line = bezier2, toH = i) + 
    # arc as scale
    geom_path(
        data = lo, 
        mapping = aes(x = x, y = y), color = "#838482", size = 1
    ) + 
    geom_point(data = lo[c(1, 101)], mapping = aes(x = x, y = y), color = "#838482", size = .35) +
    # line beneath
    geom_segment(aes(x = -(toW + .1), xend = toW + .1, y = -0.1, yend = -0.1), size = 1, color = "#f3ddb4", lineend = "round") + 
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = "white")) + 
    geom_text(data = data.table(lab = c(0, max(bb$Wartosc)), x = lo$x[c(1, 101)], y = lo$y[c(1, 101)]), mapping = aes(x = x, y = y, label = lab), color = "#807c84", size = 1.5, vjust = 1.5) + 
    geom_text(data = data.table(lab = c(0, round(max(new_groups$V1))), x = c(-(toW + .1), (toW + .1)), y = -.1), mapping = aes(x = x, y = y, label = lab), color = "#807c84", size =  1.5, vjust = 1.5)

nn <- sprintf("bezier_tree/%s_test.png", i)
ggsave(plot = pp, filename = nn, width = 4 * sk, height = 4, dpi = 720)
file.show(nn)

for (i in sprintf("%02d", 2 * 1:16))
{
    pp <- highlight_me(arc = arc_from_set, line = bezier2, toH = i) + 
        # arc as scale
        geom_path(
            data = arc_me(x0 = new_arc$x0, y0 = new_arc$y0, r = new_arc$r * 1.15, t = new_arc$j * pi * c(-1, 1), obs = c(0:100)), 
            mapping = aes(x = x, y = y), color = "#838482", size = 2
        ) + 
        # line beneath
        geom_segment(aes(x = -(toW + .1), xend = toW + .1, y = -0.1, yend = -0.1), size = 2, color = "#f3ddb4", lineend = "round") + 
        theme_void() +
        theme(panel.background = element_rect(fill = "#463c44"))
    nn <- sprintf("bezier_tree/%s_test.png", i)
    ggsave(plot = pp, filename = nn, width = 4 * sk, height = 4, dpi = 720)
    # file.show("bezier_tree/test.png")
}
