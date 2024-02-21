#' @keywords internal
hg_list <- function(data, hdtype, viz = NULL) {

  if (is.null(viz) | is.null(hdtype)) return()

  if (hdtype %in% c("CatNum")) {
    return(process_CatNum(data, viz))
  }

  if (hdtype %in% c("CatCatNum")) {
    return(process_CatCatNum(data, viz))
  }

  if (hdtype %in% c("CatNumNum")) {
    return(process_CatNumNum(data, viz))
  }

  if (hdtype %in% c("DatNum")) {
    return(process_DatNum(data, viz))
  }

  if (hdtype %in% c("CatDatNum")) {
    return(process_CatDatNum(data, viz))
  }

}


process_CatNum <- function(d, viz) {
  if (viz %in% c("bar", "column","pie", "donut")) {
    data <- purrr::pmap(.l = list(d[[1]], d[[2]], d[[3]], d[[4]]),
                        .f = function(name, y, label, color) {
                          list("name" = as.character(name),
                               "y" = as.numeric(y),
                               "label" = label,
                               "color" = color
                          )
                        })

    data <- list(data = data)
  }
  list(
    data = data,
    categories = purrr::map(as.character(unique(d[[1]])), function(z) z)
  )
}


process_CatCatNum <- function(d, viz) {
  if (viz %in% c("bar", "column")) {
    d$..labels <- as.character(d$..labels)
    axis_cat <- unique(d[[2]])
    if (all(grepl("^[0-9]+$", d[[2]]))) {
      axis_cat <- sort(unique(d[[2]]))
    }

    data_groups <- list(unique(d[[1]]),
                        split(d[complete.cases(
                          d[,c(setdiff(names(d), names(d)[1]))]),], d[[1]]))

    data <- list(
      categories = purrr::map(as.character(axis_cat), function(z) z),
      data = purrr::map(unique(d[[1]]), function(i) {
        d0 <- d |>
          dplyr::filter(!!sym(names(d)[1]) %in% i) #|>
        #dplyr::arrange(..index)
        label_info <- d0 %>% .$..labels %>% unlist()
        l0 <- list("name" = i,
                   "color" = unique(d0$..colors),
                   #"legendIndex" = unique(d0$..legendIndex),
                   "data" = purrr::map(seq_along(d0[[3]]), function(i){
                     list("label" =  label_info[i],
                          "y" = d0[[3]][i]
                     )
                   })
        )
      })
    )
  }


  data
}


process_CatNumNum <- function(d, viz) {
  if (viz %in% c("bar", "column")) {
    color <- unique(d$..colors)
    if (length(color) != 2) {
      color <- strsplit(color, split = "-") |> unlist()
    }
    if (length(unique(d[[1]])) > 1) {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = viz,
          yAxis = col - 2,
          data = d[[col]]
        )
      })
    } else {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = viz,
          yAxis = col - 2,
          data = list(d[[col]])
        )
      })

    }
    data <- list(
      title_axis = names(d)[2:3],
      categories = purrr::map(unique(d[[1]]), ~as.character(.x)),
      data = series
    )
  }
  data
}

process_DatNum <- function(d, viz) {

  if (viz %in% c("line")) {
    dl <- d |> select(y = 2, label = ..labels)
    data <- list(
      categories = unique(d[[1]]),
      data = list(data = purrr::transpose(dl),
                  color = d$..colors |> unique()
      )
    )
  }
  data
}


process_CatDatNum <- function(d, viz) {
  if (viz %in% c("line")) {
    var_cat <- names(d)[1]
    data_groups <- purrr::map(unique(d[[1]]), ~
                                d |> filter(!!sym(var_cat) %in% .x))
    names(data_groups) <- unique(d[[1]])
    data_groups <- list(unique(d[[1]]), data_groups)
    series <- purrr::pmap(.l = data_groups, function(name, d0) {
      names(d0)[3] <- "y"
      dl <- d0 |> transmute(y, label = ..labels, color = ..colors
      )
      list(data = purrr::transpose(dl), name = name,
           color = unique(dl$color)
      )
    })
    data <- list(
      categories = unique(d[[2]]),
      data = series
    )
  }
  data
}

