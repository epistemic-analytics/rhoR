library(covr)
library(magrittr)

install.packages("rvest", repos = "https://cran.rstudio.com")
library(rvest)

args <- commandArgs(trailingOnly = TRUE)

tmp_dir  <- file.path(tempdir(), "rhor")
print(paste0("Report directory: ", tmp_dir))
if (dir.create(tmp_dir, recursive = TRUE)) {
    tmp_file <- file.path(tmp_dir, paste0("index.html"))
    print(paste0("Report file: ", tmp_file))

    rep <- covr::report( x = covr::package_coverage(quiet = F), file = tmp_file)
    
    setwd(paste0(tmp_dir))
    print(list.files(tmp_dir))
    
    res = lapply(dir(".", full.names = F, recursive = T), function(f) { 
        aws.s3::put_object(
            file = f,
            object = paste0("coverage/", args[1], "/", f),
            bucket = "rhor.qe-libs.org", acl = "public-read"
        ) 
    })
    
    perc <- sub(
        pattern = "rhoR coverage - ",
        replacement = "",
        x = html_text(html_nodes(read_html(file.path(tmp_dir, "index.html")), "h2"))
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
}
