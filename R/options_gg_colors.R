# Created by Oleksandr Sorochynskyi
# On 24/10/2022

default_discrete_palettes <- lapply(
    1:100,
    function(x) {
        if (x <= 5) {
            as.character(wes_palette("Zissou1", x, type = "discrete"))
        } else {
            as.character(wes_palette("Zissou1", x, type = "continuous"))
        }
    }
)

scale_wes_colors <- function(aes, num, ...) {
    fun <- sprintf("scale_%s_gradientn", aes)
    args <- list(...)
    args <- append(
        list(colors = wes_palette("Zissou1", num, type = "continuous")),
        args
    )
    do.call(fun, args)
}

gen_scale_wes_fun <- function(aes, num, ...) {
    function() scale_wes_colors(aes, num = num, ...)
}

options(
    ggplot2.discrete.colour = default_discrete_palettes,
    ggplot2.discrete.fill = default_discrete_palettes,
    ggplot2.continuous.colour = gen_scale_wes_fun("color", 3),
    ggplot2.continuous.fill = gen_scale_wes_fun("fill", 3),
    ggplot2.binned.colour = gen_scale_wes_fun("color", 3),
    ggplot2.binned.fill = gen_scale_wes_fun("fill", 3)
)
