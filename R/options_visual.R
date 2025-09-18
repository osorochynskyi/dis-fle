# Created by Oleksandr Sorochynskyi
# On 24/10/2022

#' Settings for visual outputs in documents
#'
#' Notes to self :
#'
#' * an A4 sheet is about 20 cm wide, so a 16cm plot looks reasonably good
#' * there is 2.54 cm in an inch
#'
fig_height <- 8 / 2.54
fig_width <- 16 / 2.54

options(
    vsc.dev.args = list(
        width = fig_width,
        height = fig_height,
        unit = "in",
        res = 300
    ),
    digits = 4,
    scipen = 5,
    OutDec = ","
)


knitr::opts_chunk$set(
    echo = FALSE,
    fig.width = fig_width,
    fig.height = fig_height
)

ggplot2::theme_set(ggplot2::theme_light(10))

# This removes the grey box around facet titles
# But i ultimately decided that having the most typical visul is easiest
# for comprehension
#  theme(
#      strip.background = element_rect(
#          color="white",
#          fill="white",
#      ),
#      strip.text.x = element_text(color = "black"),
#      strip.text.y = element_text(color = "black"),
# )



ggsave_pdf <- function(filename, plot = last_plot(), ...) {
    args <- list(...)
    if ("scale" %in% names(args)) {
        scale <- args[["scale"]]
    } else {
        scale <- 1
    }
    if (!"device" %in% names(args)) {
        args[["device"]] <- "pdf"
    }
    if (!"width" %in% names(args)) {
        args[["width"]] <- scale * fig_width
    }
    if (!"height" %in% names(args)) {
        args[["height"]] <- scale * fig_height
    }
    if (!"units" %in% names(args)) {
        args[["units"]] <- "in"
    }
    if (!"dpi" %in% names(args)) {
        args[["dpi"]] <- 300
    }
    args <- append(list(filename = filename, plot = plot), args)
    do.call(ggplot2::ggsave, args)
}

#' Save plots using tikz device
#'
#' Saves some typing and keeps the figure size consistant
#' @param p a ggplot object, plot to be saved
#' @param filename character string giving the name of the file for the plot
#' @param height,width (in cm)
#' @param aspect_ratio ratio of width over height
#' @param height_mult, width_mult scaler, e.g., if =3 the
#'    height(width) will be multipled by 3
#' @param hadjust, vadjust additional margin width to add to the plot,
#'    useful when legend takes up a lot of space
ggsave_tikz <- function(filename,
                        p = ggplot2::last_plot(),
                        height,
                        width,
                        format = c("tikz", "pdf"),
                        aspect_ratio = 2,
                        height_mult = 1,
                        width_mult = 1,
                        hadjust = 2,
                        vadjust = 1,
                        standalone = TRUE,
                        sanitize = TRUE,
                        ...) {
    if (missing(height) && missing(width)) {
        width <- fig_width
        height <- fig_height
    }
    if (missing(height)) height <- width / aspect_ratio
    if (missing(width)) width <- height * aspect_ratio

    format <- match.arg(format)

    # Apply adjustments
    height <- height * height_mult + vadjust
    width <- width * width_mult + hadjust

    # Convert to inches
    height <- height * 0.3937
    width <- width * 0.3937

    # Append a .tex to filename
    if (format == "tikz" && !grepl("\\.tex", filename)) {
        filename <- paste0(filename, ".tex")
    }
    if (format == "pdf" && !grepl("\\.pdf", filename)) {
        filename <- paste0(filename, ".pdf")
    }

    if (format == "tikz") {
        tikzDevice::tikz(
            filename = filename,
            height = height,
            width = width,
            standAlone = standalone,
            sanitize = sanitize,
            timestamp = FALSE,
            ...
        )
    } else if (format == "pdf") {
        cairo_pdf(
            file = filename,
            height = height,
            width = width,
            ...
        )
    }
    # p is either a ggplot, or a expression yielding a plot (e.g. { plot(x,y) })
    if ("gg" %in% class(p)) {
        print(p)
    } else {
        eval(p)
    }
    dev.off()

    if (format == "tikz") {
        cat(sprintf(
            "\\input{%s}\n\\caption{}\n\\label{fig:%s}\n",
            str_sub(filename, start = 8),
            str_match(filename, "output/figures/(.*).tex")[1, 2]
        ))
    } else if (format == "pdf") {
        cat(sprintf(
            "\\includegraphics{%s}\n\\caption{}\n\\label{fig:%s}\n",
            str_sub(filename, start = 8),
            str_match(filename, "output/figures/(.*).tex")[1, 2]
        ))
    }
    p
}
