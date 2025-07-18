#' @keywords internal
hg_list <- function(data, hdtype, viz = NULL) {

  if (is.null(viz) | is.null(hdtype)) return()

  if (viz == "network_graph") {
    return(process_network_graph(data, viz))
  }

  if (hdtype %in% c("Num")) {
    return(process_Num(data, viz))
  }

  if (hdtype %in% c("NumNum")) {
    return(process_NumNum(data, viz))
  }

  if (viz == "line") {
    if (hdtype %in% c("DatNum", "CatNum")) {
    return(process_DatNum(data, viz))
    }
  } else {
    if (hdtype %in% c("CatNum")) {
      return(process_CatNum(data, viz))
    }
  }

  if (hdtype %in% c("CatCatNum")) {
    return(process_CatCatNum(data, viz))
  }

  if (hdtype %in% c("CatNumNum")) {
    return(process_CatNumNum(data, viz))
  }

  if (hdtype %in% c("CatCatCatNum")) {
    return(process_CatCatCatNum(data, viz))
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

  if (viz %in% c("parallel_coordinates")) {
    return(process_parallel_data_plot(data, viz))
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

  if (viz %in% c("bar", "column", "radial_bar", "pie", "donut")) {
    data <- purrr::pmap(
      list(d[[1]], d[[2]], d$..labels, d$..colors),
      function(name, y, label, color) {
        list(
          "name" = as.character(name),
          "y" = as.numeric(y),
          "label" = label,
          "color" = color
        )
      }
    )

    data <- list(
      data = data,
      categories = purrr::map(as.character(unique(d[[1]])), function(z) z)
    )
  }

  if (viz == "bubble") {
    data <- purrr::map(1:nrow(d), function(z){
      list("name" = d[[1]][z],
           "value" = d[[2]][z],
           "label" = d$..labels[z],
           "color" = as.character(d$..colors[z]))
    })
  }

  if (viz == "treemap") {
    data <- purrr::pmap(
      list(d[[1]], d[[2]], d$..labels, d$..colors),
      function(name, value, label, color) {
        list(
          "name" = name,
          "value" = value,
          "label" = label,
          "color" = as.character(color),
          "colorValue" = value
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
process_Num <- function(d, viz) {
  if (viz %in% "line") {
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)
    d0 <- d |> rename(y = 1, color = ..colors)

    data <- list(list(
      name = names(d)[1],
      color = unique(d0$color),
      data = purrr::imap(d0$y, function(y, i) list(
        x = i - 1,
        y = as.numeric(y),
        color = d0$color[i],
        label = d0$..labels[i]
      ))
    ))
  }

  data
}

#' @rdname process_functions
process_NumNum <- function(d, viz) {
  if (viz %in% "line") {
    data <- purrr::map(1:2, function(i) {
      col <- names(d)[i]

      list(
        name = col,
        yAxis = i - 1,
        color = unique(d[[paste0(col, "_color")]])[1],
        data = purrr::imap(d[[i]], function(y, j) {
          list(
            x = j - 1,
            y = as.numeric(y),
            label = d$..labels[j]
          )
        })
      )
    })
  }

  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(x = 1, y = 2, color = ..colors, label = ..labels)

    data <- purrr::pmap(d, function(x, y, color, label) {
      list(
        x = as.numeric(x),
        y = as.numeric(y),
        color = color,
        label = label
      )
    })
  }

  data
}

#' @rdname process_functions
process_CatCatNum <- function(d, viz) {

  if (viz %in% c("bar", "column", "radial_bar", "line")) {
    d$..labels <- as.character(d$..labels)


    if (viz == "line") {
      d <- d |> tidyr::drop_na(!!sym(names(d)[1]),!!sym(names(d)[2]))
    }
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
        label = d$..labels[z],
        colorValue = d[[3]][z]
      )
    })
    data <- list(data = c(list_id, list_cats))
  }

  if (viz %in% "heatmap") {
    x_categories <- unique(d[[1]])
    y_categories <- unique(d[[2]])

    x_matches <- purrr::set_names(0:(length(x_categories) - 1), x_categories)
    y_matches <- purrr::set_names(0:(length(y_categories) - 1), y_categories)

    data <- list(
      categories = list(x = unique(d[[1]]), y = unique(d[[2]])),
      data = purrr::map(seq_len(nrow(d)), function(i) {
        list(
          x = x_matches[[d[[1]][i]]],
          y = y_matches[[d[[2]][i]]],
          value = d[[3]][i],
          label = d$..labels[i]
        )
      })
    )
  }

  if (viz %in% c("bubble")) {
    var_cat <- names(d)[1]

    data <- purrr::map(unique(d[[1]]), function(z){
      d0 <- d |> filter(!!sym(var_cat) %in% z)
      color <- substr(unique(d0$..colors)[1], 1, 7)

      list(
        name = z,
        color = paste0(color, "80"),
        data = purrr::map(1:nrow(d0), function(i){
          list(
            name = d0[[2]][i],
            value = d0[[3]][i],
            label = d0$..labels[i],
            color = d0$..colors[i]
          )
        })
      )
    })

    data <- list(data = data)
  }

  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(cat = 2, x = 1, y = 3)

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

  if (viz %in% "sunburst") {
    d <- d |> arrange(desc(across(3, identity)))
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

    col3 <- names(subcategory_counts)[3]
    max_count <- max(subcategory_counts[[col3]])
    min_count <- min(subcategory_counts[[col3]])

    subcategory_counts <- subcategory_counts |>
      mutate(opacity = calculate_opacity(!!sym(col3), min_count, max_count),
             ..colors = mapply(adjustcolor, ..colors, alpha.f = opacity))

    subcategory_data <- subcategory_counts |>
      pmap(function(...) {
        args <- list(...)
        list(
          name = args[[col2]],
          id = args[["subcat_id"]],
          parent = args[["cat_id"]],
          value = as.double(args[[col3]]),
          color = args[["..colors"]],
          label = args[["..labels"]]
        )
      })

    data <- append(data, subcategory_data)
  }

  if (viz %in% c("sankey", "dependency_wheel")){
    data <- d |>
      set_names("from", "to", "weight", "label")

    nodes <- data |>
      distinct(from, to)
    nodes <- tibble(id = c(nodes$from, nodes$to), name = c(nodes$from, nodes$to))
    data <- list(data = data, nodes = nodes)

  }

  if (viz %in% "bar_negative_stack") {
    data <- purrr::imap(unique(d[[1]])[1:2], function(cat, i) {
      d0 <- d |> dplyr::filter(!!sym(names(d)[1]) %in% cat)
      if (i == 1) {
        d0 <- d0 |> dplyr::mutate(!!(names(d)[3]) := -1 * !!sym(names(d)[3]))
      }

      list(
        name = cat,
        color = unique(d0$..colors),
        data = purrr::map(1:nrow(d0), function(i) {
          list(
            y = d0[[3]][i],
            label = d0$..labels[i]
          )
        })
      )
    })

    data <- list(data = data, categories = unique(d[[2]]))
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
          data = purrr::imap(d[[col]], function(y, i) {
            list(
              label = d$..labels[i],
              y = y
            )
          })
        )
      })
    } else {
      series <- map(c(2,3), function(col) {
        list(
          name = names(d)[col],
          color = color[col-1],
          type = viz,
          yAxis = col - 2,
          data = purrr::imap(d[[col]], function(y, i) {
            list(
              label = d$..labels[i],
              y = y
            )
          })
        )
      })

    }
    data <- list(
      title_axis = names(d)[2:3],
      categories = purrr::map(unique(d[[1]]), ~as.character(.x)),
      data = series
    )
  }

  if (viz %in% "line") {
    series <- process_NumNum(d |> select(-1), viz)
    categories <- unique(d[[1]])

    data <- list(
      data = series,
      categories = categories
    )
  }

  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(cat = 1, x = 2, y = 3, color = ..colors, label = ..labels)
    categories <- unique(d$cat)

    data <- purrr::map(categories, function(z) {
      var_cat <- names(d)[1]

      if (is.na(z)) {
        d0 <- d |> filter(is.na(!!sym(var_cat)))
      } else {
        d0 <- d |> filter(!!sym(var_cat) == z)
      }

      d0 <- d0 |> select(x, y, color, label)
      name <- if(is.na(z)) "(NA)" else z

      list(
        name = name,
        type = viz,
        color = unique(d0$color),
        data = purrr::pmap(d0, function(x, y, color, label) {
          list(
            x = as.numeric(x),
            y = as.numeric(y),
            color = color,
            label = label
          )
        })
      )
    })
  }

  if (viz %in% "dumbbell") {

    data <- d |>
      relocate(..labels, .after = 3) |>
      mutate(
        temp_col2 = ifelse(d[[2]] > d[[3]], d[[3]], d[[2]]),
        temp_col3 = ifelse(d[[2]] > d[[3]], d[[2]], d[[3]])
      ) |>
      select(-2, -3) |>
      relocate(temp_col2, temp_col3, .after = 1) |>
      setNames(c("name", "low", "high", "label", "colors"))
  }

  data
}

#' @rdname process_functions
process_CatNumNumNum <- function(d, viz) {
  if (viz %in% "scatter") {
    if ("x" %in% names(d)) d <- d |> rename(x1 = x)
    if ("y" %in% names(d)) d <- d |> rename(y1 = y)

    d <- d |> rename(cat = 1, x = 2, y = 3, z = 4, color = ..colors, label = ..labels)
    categories <- unique(d$cat)

    data <- purrr::map(categories, function(c) {
      var_cat <- names(d)[1]

      if (is.na(c)) {
        d0 <- d |> filter(is.na(!!sym(var_cat)))
      } else {
        d0 <- d |> filter(!!sym(var_cat) == c)
      }

      d0 <- d0 |> select(x, y, z, color, label)
      name <- if(is.na(c)) "(NA)" else c

      list(
        name = name,
        type = "bubble",
        color = unique(d0$color),
        data = purrr::pmap(d0, function(x, y, z, color, label) {
          list(
            x = as.numeric(x),
            y = as.numeric(y),
            z = as.numeric(z),
            color = color,
            label = label
          )
        })
      )
    })
  }
}

#' @rdname process_functions
process_CatCatCatNum <- function(d, viz) {
  if (viz %in% "sankey"){

    if (any(apply(d, 2, function(x) any(duplicated(x))))){
      unique_values <- lapply(if (ncol(d) == 5) d[, -c(4, 5)] else d[, -4],
                              function(column) sort(unique(column)))

      equality_matrix <- sapply(unique_values,
                                function(x) sapply(unique_values,
                                                   function(y) all(x %in% y)))

      all_equal <- length(unique_values) == 1 ||
        all(rowSums(equality_matrix) == length(unique_values))

      if (all_equal) {
        d <- d |>
          ungroup() |>
          mutate(
            across(c(1), ~ paste0(., " 1")),
            across(c(2), ~ paste0(., " 2")),
            across(c(3), ~ paste0(., " 3"))
          )
      } else if (equality_matrix[1, 2] && equality_matrix[2, 1]) {
        d <- d |>
          ungroup() |>
          mutate(
            across(c(1), ~ paste0(., " 1")),
            across(c(2), ~ paste0(., " 2"))
          )
      } else if (equality_matrix[1, 3] && equality_matrix[3, 1]) {
        d <- d |>
          ungroup() |>
          mutate(
            across(c(1), ~ paste0(., " 1")),
            across(c(3), ~ paste0(., " 3"))
          )
      } else if (equality_matrix[2, 3] && equality_matrix[3, 2]) {
        d <- d |>
          ungroup() |>
          mutate(
            across(c(2), ~ paste0(., " 2")),
            across(c(3), ~ paste0(., " 3"))
          )
      }
    }

    df1 <- d |>
      select(1, 2, 4, 5) |>
      setNames(c("from", "to", "weight", "label"))

    df2 <- d |>
      ungroup() |>
      select(2, 3, 4, 5) |>
      setNames(c("from", "to", "weight", "label"))

    data <- bind_rows(df1, df2)

    var_unions <- lapply(data[, c(1, 2)], function(col) {
      col <- data.frame(unique(col))
      col |> set_names("id")
    })

    var_unions <- c(unique(data[[1]]), unique(data[[2]]))
    nodes <- data.frame(id = var_unions, name = var_unions)


    data <- list(data = data, nodes = nodes)

  }

  data
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
process_network_graph <- function(d, viz) {

  search_parent_color <- function(leaf, nodes, links) {
    parent <- links |> filter(to == leaf) |> pull(from) |> unique()

    if (parent[1] %in% nodes$id) {
      return(nodes$..colors[nodes$id == parent[1]])
    } else {
      search_parent_color(parent, nodes, links)
    }
  }

  d <- d |> ungroup()
  d0 <- d |> select(-c(ncol(d) - 1, ncol(d)))

  data <- purrr::map(1:(ncol(d0) - 1), function(i) {
    d |> select(from = i, to = i + 1, label = ..labels)
  })

  links <- data |>
    bind_rows() |>
    distinct()

  roots <- links |> filter(!from %in% to) |> pull(from) |> unique()
  parents <- links |> filter(from %in% roots) |> pull(to) |> unique()
  leaves <- links |> filter(!to %in% parents) |> pull(to) |> unique()

  nodes <- data.frame(id = c(roots, parents)) |>
    colors_data(color_by = "id")

  leaves <- purrr::map(leaves, function(leaf) {
    color <- search_parent_color(leaf, nodes, links)
    data.frame(id = leaf, ..colors = color)
  })

  links <- purrr::pmap(links, function(from, to, label) {
    list(from = from, to = to, label = label)
  })

  nodes <- nodes |>
    bind_rows(leaves) |>
    purrr::pmap(function(id, ..colors) {
      list(id = id, color = ..colors)
    })

  data <- list(data = links, nodes = nodes)
}

#' @rdname process_functions
process_parallel_data_plot <- function(d, viz) {

  if(viz %in% "parallel_coordinates") {
    d0 <- d |> select(-c(..colors, Conteo,..labels))

    xAxis <- colnames(d0)
    yAxis <- 1:ncol(d0) |>
      purrr::map(function(i) {
        list(
          categories = unique(d0[[i]])
        )
      })

    d0 <- d0 |>
      ungroup() |>
      mutate(across(where(is.character), ~ match(.x, unique(.x)) - 1))

    data <- purrr::map(1:nrow(d0), function(i) {
      list(
        name = i,
        color = d$..colors[i],
        data = d0[i, ] |> as.numeric(),
        label = d$..labels[i]
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
