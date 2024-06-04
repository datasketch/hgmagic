#' @keywords internal
hg_list <- function(data, hdtype, viz = NULL) {

  if (is.null(viz) | is.null(hdtype)) return()

  if (hdtype %in% c("NumNum")) {
    return(process_NumNum(data, viz))
  }

  if (hdtype %in% c("CatCat")) {
    return(process_CatCat(data, viz))
  }

  if (hdtype %in% c("CatNum")) {
    return(process_CatNum(data, viz))
  }

  if (hdtype %in% c("CatCatNum")) {
    return(process_CatCatNum(data, viz))
  }

  if (hdtype %in% c("CatNumNum")) {
    return(process_CatNumNum(data, viz))
  }

  if (hdtype %in% c("CatNumNumNum")) {
    return(process_CatNumNumNum(data, viz))
  }

  if (hdtype %in% c("DatNum")) {
    return(process_DatNum(data, viz))
  }

  if (hdtype %in% c("CatDatNum")) {
    return(process_CatDatNum(data, viz))
  }

  if (hdtype %in% c("DatNumNum")) {
    return(process_DatNumNum(data, viz))
  }

  if (hdtype %in% c("CatImgNum")) {
    return(process_CatImgNum(data, viz))
  }

  if (hdtype %in% c("CatCatCatCatCatCatCat")) {
    return(process_CatCatCatCatCatCatCat(data, viz))
  }

}

#' Data processing for visualization
#'
#' This set of functions provides tools to process data for visualisation in different types of charts.
#'
#' @param d A data frame containing the data to process. The structure of the data frame varies depending on the processing function.
#' @param viz The desired type of visualisation. It can be "bar", "column", "pie", or "donut".
#' @return A list with the processed data for visualisation.
#' @examples
#' # Example usage of data processing functions
#' d <- data.frame(
#'   category = c("A", "A", "B", "B"),
#'   value = c(10, 20, 30, 40)
#' )
#' process_CatNum(d, "bar")
#' @export

#' @rdname process_functions
process_CatNum <- function(d, viz) {
  print("Process CatNum")
  if (viz %in% c("bar", "column","pie", "donut")) {
    data <- purrr::pmap(.l = list(d[[1]], d[[2]], d[[3]], d[[4]]),
                        .f = function(name, y, label, color) {
                          list("name" = as.character(name),
                               "y" = as.numeric(y),
                               "label" = label,
                               "color" = color
                          )
                        })
    data <- list(
      data = data,
      categories = purrr::map(as.character(unique(d[[1]])), function(z) z)
    )
  }
  if (viz == "treemap") {
    data <- purrr::pmap(
      list(d[[1]], d[[2]], d$..labels, d$..colors),
      function(name, value, label, color) {
        list(
          "name" = name,
          "value" = value,
          "label" = label,
          "color" = as.character(color)
        )
      }
    )
  }


  if (viz == "item") {
    data <- purrr::map(unique(d[[1]]), function(z){
      d0 <- d[d[[1]] %in% z,]
      list("name" = z,
           "y" = as.numeric(d0[[2]]),
           "color" = d0$..colors,
           marker = list(
             symbol= 'point'
           ))
    })
  }

  data

}

#' @rdname process_functions
process_CatCat <- function(d, viz) {
  if (viz == "sunburst") {
    col1 <- names(d)[1]
    col2 <- names(d)[2]
    d[[col2]] <- as.character(d[[col2]])
    data <- list(list(name = "Todos", id = "0.0", parent = ""))
    unique_categories <- unique(d[[col1]])

    category_data <- map(unique_categories, function(cat) {
      cat_id <- paste0("1.", which(unique_categories == cat))
      colors <- d |>
        filter(!!sym(col1) == cat) |>
        group_by(!!sym("..colors"))|>
        summarise(.groups = 'drop') |>
        pull()

      list(name = cat, id = cat_id, parent = "0.0", color = colors)
    })

    data <- append(data, category_data)

    subcategory_counts <- d |>
      filter(!is.na(.data[[col2]])) |>
      group_by(!!sym(col1), !!sym(col2), !!sym("..colors")) |>
      summarise(count = n(), .groups = 'drop') |>
      mutate(
        cat_id = paste0("1.",match(.data[[col1]],unique_categories)),
        subcat_id = pmap_chr(
          list(
            .data[[col1]],
            row_number()
          ),
          ~ paste0(
            "2.",
            match(..1, unique_categories), ".", ..2)
        )
      )

    max_count <- max(subcategory_counts$count)
    min_count <- min(subcategory_counts$count)

    subcategory_counts <- subcategory_counts |>
      mutate(opacity = calculate_opacity(count, min_count, max_count),
             colors = mapply(adjustcolor, ..colors, alpha.f = opacity))

    subcategory_data <- subcategory_counts |>
      pmap(function(...) {
        args <- list(...)
        list(
          name = args[[col2]],
          id = args[["subcat_id"]],
          parent = args[["cat_id"]],
          value = as.double(args[["count"]]),
          color = args[["colors"]]
        )
      }
      )

    data <- append(data, subcategory_data)

  }

  data
}

#' @rdname process_functions
process_NumNum <- function(d, viz) {
  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(x = 1, y = 2, color = ..colors)

    data <- purrr::pmap(d, function(x, y, color) {
      list(
        x = as.numeric(x),
        y = as.numeric(y),
        color = color
      )
    })
  }

  data
}

#' @rdname process_functions
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

  if (viz %in% "treemap") {
    var_cat <- names(d)[1]
    list_id <- purrr::map(unique(d[[1]]), function(i) {
      d0 <- d |> filter(!!sym(var_cat) %in% i)
      list(
        id = i,
        name = i,
        color = unique(d0$..colors)
      )
    })

    list_cats <- purrr::map(1:nrow(d), function(z) {
      nm <- ifelse(is.na(d[[2]][z]), "NA", d[[2]][z])
      list(
        name = nm,
        parent = d[[1]][z],
        value = d[[3]][z],
        label = d$..labels[z]#,
        #colorValue = d[[3]][z]
      )
    })
    data <- list(data = c(list_id, list_cats))
  }

  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |>
      rename(cat = 2, x = 1, y = 3) |>
      arrange(x)

    categories <- d$x |> unique()
    categories <- tibble(
      name = categories,
      index = 0:(length(categories) - 1)
    )

    d[, 1] <- categories$index[match(d[[1]], categories$name)]
    d <- d |> relocate(cat)

    categories$name <- ifelse(is.na(categories$name), "(NA)", categories$name)

    data <- process_CatNumNum(d, viz)
    data <- list(data = data, categories = categories$name)
  }

  if (viz == "sunburst") {
    col1 <- names(d)[1]
    col2 <- names(d)[2]
    d[[col2]] <- as.character(d[[col2]])

    data <- list(list(name = "Todos", id = "0.0", parent = ""))

    unique_categories <- unique(d[[col1]])

    category_data <- map(unique_categories, function(cat) {
      cat_id <- paste0("1.", which(unique_categories == cat))
      colors <- d |>
        filter(!!sym(col1) == cat) |>
        group_by(!!sym("..colors"))|>
        summarise(.groups = 'drop') |>
        pull()

      list(name = cat, id = cat_id, parent = "0.0", color = colors)
    })

    data <- append(data, category_data)

    subcategory_counts <- d |>
      filter(!is.na(.data[[col2]])) |>
      group_by(!!sym(col1), !!sym(col2), !!sym("..colors")) |>
      summarise(count = n(), .groups = 'drop') |>
      mutate(
        cat_id = paste0("1.",match(.data[[col1]],unique_categories)),
        subcat_id = pmap_chr(
          list(
            .data[[col1]],
            row_number()
          ),
          ~ paste0(
            "2.",
            match(..1, unique_categories), ".", ..2)
        )
      )

    subcategory_data <- subcategory_counts |>
      pmap(function(...) {
        args <- list(...)
        list(
          name = args[[col2]],
          id = args[["subcat_id"]],
          parent = args[["cat_id"]],
          value = as.double(args[["count"]]),
          color = args[["..colors"]]
        )
      }
      )

    data <- append(data, subcategory_data)
  }

  if (viz == "sankey"){

    if(any(sapply(d, is.numeric))){
      data <- d |>
        set_names('from', 'to', 'weight') |>
        mutate(weight = tidyr::replace_na(weight, 0))
    } else {
      data <- d |>
        set_names('from', 'to') |>
        group_by(from, to) |>
        summarize(weight = n())
    }

    if(all(sort(unique(data[[1]])) == sort(unique(data[[2]])))){
      data <- data |>
        mutate_at(vars(1), ~paste0(., "_1")) |>
        mutate_at(vars(2), ~paste0(., "_2"))
    }

    var_unions <- lapply(data[, c(1, 2)], function(col) {
      col <- data.frame(unique(col))
      col |> set_names("id")
    })

    nodes <- full_join(var_unions[[1]], var_unions[[2]], by = "id") |>
      colors_data(color_by = "id")

    nodes <- lapply(1:nrow(nodes), function(i) {
      as.list(nodes[i, ])
    })

    data <- list(data = data, nodes = nodes)

  }

  data
}

#' @rdname process_functions
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

  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(cat = 1, x = 2, y = 3, color = ..colors)
    categories <- unique(d$cat) |> sort(na.last = TRUE)

    data <- purrr::map(categories, function(z) {
      var_cat <- names(d)[1]

      if (is.na(z)) {
        d0 <- d |> filter(is.na(!!sym(var_cat)))
      } else {
        d0 <- d |> filter(!!sym(var_cat) == z)
      }

      d0 <- d0 |> select(x, y, color)
      name <- if(is.na(z)) "(NA)" else z

      list(
        name = name,
        type = viz,
        color = unique(d0$color),
        data = purrr::pmap(d0, function(x, y, color) {
          list(
            x = as.numeric(x),
            y = as.numeric(y),
            color = color
          )
        })
      )
    })
  }

  data
}

#' @rdname process_functions
process_CatNumNumNum <- function(d, viz) {
  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(cat = 1, x = 2, y = 3, z = 4, color = ..colors)
    categories <- unique(d$cat) |> sort(na.last = TRUE)

    data <- purrr::map(categories, function(c) {
      var_cat <- names(d)[1]

      if (is.na(c)) {
        d0 <- d |> filter(is.na(!!sym(var_cat)))
      } else {
        d0 <- d |> filter(!!sym(var_cat) == c)
      }

      d0 <- d0 |> select(x, y, z, color)
      name <- if(is.na(c)) "(NA)" else c

      list(
        name = name,
        type = "bubble",
        color = unique(d0$color),
        data = purrr::pmap(d0, function(x, y, z, color) {
          list(
            x = as.numeric(x),
            y = as.numeric(y),
            z = as.numeric(z),
            color = color
          )
        })
      )
    })
  }
}

#' @rdname process_functions
process_DatNum <- function(d, viz) {

  if (viz %in% c("line")) {
    dl <- d |> select(y = 2, label = ..labels)
    data <- list(
      categories = unique(d[[1]]),
      data =  purrr::transpose(dl),
      color = unique(d$..colors)[1]

    )
  }

  data
}

#' @rdname process_functions
process_CatDatNum <- function(d, viz) {
  if (viz %in% c("line")) {
    var_cat <- names(d)[1]
    data_groups <- purrr::map(unique(d[[1]]), ~
                                d |> filter(!!sym(var_cat) %in% .x))
    names(data_groups) <- unique(d[[1]])
    data_groups <- list(unique(d[[1]]), data_groups)
    series <- purrr::pmap(.l = data_groups, function(name, d0) {
      names(d0)[3] <- "y"
      dl <- d0 |>
        transmute(y,
                  label = ..labels,
                  color = ..colors)
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

#' @rdname process_functions
process_DatNumNum <- function(d, viz) {
  if (viz %in% c("line")) {
    color <- unique(d$..colors)
    if (length(color) != 2) {
      color <- strsplit(color, split = "-") |> unlist()
    }
    if (length(unique(d[[1]])) > 1) {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = "line",
          yAxis = col - 2,
          data = d[[col]]
        )
      })
    } else {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = "line",
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

#' @rdname process_functions
process_CatImgNum <- function(d, viz) {
  if (viz %in% "bar_icons") {
    d <- d |>
      rename(cat = 1, img = 2, y = 3) |>
      arrange(cat)

    data <- purrr::pmap(d, function(cat, img, y) {
      if (grepl("^http", img)) {
        color <- list(
          pattern = list(
            image = img,
            aspectRatio = 0.5
          )
        )
      } else {
        # intentos de cargar imagen local
        # img <- file(img) # As file
        # img <- paste0(getwd(), "/", img) # As path
        # img <- paste0("file://", getwd(), "/", img) # As URL

        color <- list(
          pattern = list(
            path = img,
            aspectRatio = 0.5
          )
        )
      }

      list(
        name = as.character(cat),
        y = as.numeric(y),
        color = color
      )
    })

    data <- list(data = data, categories = d$cat)
  }

  data
}

#' @rdname process_functions
process_CatCatCatCatCatCatCat <- function(d, viz) {

  if(viz == "parallel_coordinates") {
    d0 <- d |> select(-c(..colors))

    xAxis <- colnames(d0)

    yAxis <- 1:ncol(d0) |>
      purrr::map(function(i) {
        list(
          categories = unique(d0[[i]])
        )
      })

    d0 <-d0 |>
      mutate(across(where(is.character), ~ as.numeric(factor(.x))))

    data <- purrr::map(1:nrow(d0), function(i) {
      list(
        name = i,
        color = d$..colors[i],
        data = d0[i, ] |> as.numeric()
      )
    })

    data <- list(
      data = data,
      xAxis = xAxis,
      yAxis = yAxis
    )
  }

  data
}
