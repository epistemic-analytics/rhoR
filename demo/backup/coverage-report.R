library(covr)
library(magrittr)

install.packages("rvest", repos = "https://cran.rstudio.com")
library(rvest)

args <- commandArgs(trailingOnly = TRUE)

perc <- sub(
    pattern = "rhoR coverage - ",
    replacement = "",
    x = html_text(html_nodes(read_html(
            paste0(
                "inst/coverage/",
                ifelse(
                    length(args) > 0,
                    paste0(args[1], "/"),
                    ""
                ),
                "index.html"
            )
    ), "h2"))
)
numb <- round(as.numeric(sub(
    pattern = "%",
    replacement = "",
    x = perc
)))

ranges <- data.frame(
    scale = c(90, 70, 50),
    color = c("green", "orange", "red"),
    stringsAsFactors = F
)
color <- ranges$color[[head(which(numb > ranges$scale), 1)]]

obj_name <- paste0(
                "coverage",
                ifelse(
                    length(args) > 0,
                    paste0("-", args[1]),
                    ""
                ),
                ".svg"
            )

aws.s3::put_object(
    file = httr::content(httr::GET(
        paste0("https://img.shields.io/badge/coverage-",
                    numb,
                    "%25-",
                    color,
                    ".svg")
    )),
    object = obj_name,
    bucket = "rhor.qe-libs.org",
    acl = "public-read",
    serialize = TRUE,
    headers = list("Content-Type" = "image/svg+xml")
)
