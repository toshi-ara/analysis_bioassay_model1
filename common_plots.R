library(ggplot2)
library(ggforce)

source("common_scripts.R", local = TRUE)


## use subscript in strip of figure
label_reaction <- c(
    expression(paste(TD[50], sep = "")),
    expression(paste(LD[50], sep = ""))
)

labeler <- function(title, judgement_time, d50, lwr, upr) {
    str <- sprintf(
        "expression(paste(%s[50], \" (%s): %.1f mg/kg [%.1f, %.1f]\"))",
        title, judgement_time, d50, lwr, upr
    )
    eval(parse(text = str))
}



########################################
# plot fit probit curve
########################################

##
## function
## input:
##   model: glm object by probit analysis
##   response_tbl: data points
##   d50, lwr, upr
##   title
##   judgement_time: string
##
plot_fit_probit_curve <- function(model, response_tbl,
                                  d50, lwr, upr,
                                  title = NULL, judgement_time) {
    log_d50 <- log(d50)

    log_dose <- response_tbl$logDose
    d <- (log_dose[2] - log_dose[1]) * 0.2
    xmin <- min(log_dose) - d
    xmax <- max(log_dose) + d

    yval <- seq(0, 1, by = 0.2)

    rate_min <- 0.001
    rate_max <- 0.999
    dat_ci <- get_ci(model, p = seq(rate_min, rate_max, length.out = 101))

    p <- ggplot(dat_ci) +
        geom_ribbon(aes(y = !!sym("prob"),
                        xmin = !!sym("log_lwr"),
                        xmax = !!sym("log_upr")),
                    alpha = 0.3) +
        geom_line(aes(!!sym("logDose"), !!sym("prob"))) +
        geom_segment(x = -10, y = 0.5,
                     xend = log_d50, yend = 0.5,
                     color = "gray30") +
        geom_segment(x = log_d50, y = 0.5,
                     xend = log_d50, yend = -10,
                     color = "gray30") +
        geom_point(data = response_tbl,
                   aes(!!sym("logDose"), !!sym("rate")),
                   size = 2) +
        scale_x_continuous(breaks = response_tbl$logDose,
                           labels = response_tbl$Dose,
                           minor_breaks = NULL) +
        coord_cartesian(xlim = c(xmin, xmax),
                        ylim = c(0, 1)) +
        scale_y_continuous(breaks = yval,
                           labels = yval * 100,
                           minor_breaks = NULL) +
        labs(x = "Dose (mg/kg)", y = "Response rate (%)") +
        ggtitle(ifelse(is.null(title),
                       "",
                       labeler(title, judgement_time, d50, lwr, upr))) +
        theme_bw() +
        theme(
            axis.text = element_text(size = 14, color = "black"),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 16, hjust = 0.5),
            panel.grid.minor = element_blank()
        )

    return(p)
}



########################################
# violin plot for TD50/LD50 at each time
########################################

plot_d50_time_violin <- function(dat, dat_summary, seed) {
    set.seed(seed)
    p <- ggplot(dat, aes(factor(!!sym("Time")), !!sym("Dose"))) +
        geom_violin() +
        geom_sina(size = 0.8, alpha = 0.5) +
        stat_summary(data = dat_summary,
                     aes(factor(!!sym("Time")), !!sym("geomean")),
                     fun = identity,
                     geom = "crossbar",
                     width = 0.8) +
        labs(x = "Judgement Time (min)", y = "Dose (mg/kg)") +
        facet_wrap(~ reaction, scales = "free_y", labeller=label_parsed) +
        theme_bw() +
        theme(
            axis.text = element_text(size = 14, color = "black"),
            axis.title = element_text(size = 16),
            panel.grid.major.x = element_blank(),
            strip.text = element_text(size = 18),
            strip.background = element_blank())

    return(p)
}



####################
## theme for Gompertz curve fitting
####################

theme_Gompertz <- list(
    labs(x = "Time (min)", y = "Cumulative event (%)",
         color = "Rate"),
    scale_x_continuous(breaks = seq(0, 10, by = 2), minor_breaks = NULL),
    scale_y_continuous(breaks = seq(0, 100, by = 20), minor_breaks = NULL),
    coord_cartesian(xlim = c(0, max_time), ylim = c(0, 100)),
    scale_color_brewer(palette = "Dark2"),
    theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
    )
)



####################
## theme for violin/sina plot
####################

theme_D50 <- list(
    labs(x = "Judgement Time (min)", y = "Dose (mg/kg)"),
    theme_bw(),
    theme(
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 18),
        strip.background = element_blank())
)

