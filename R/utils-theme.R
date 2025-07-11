#' @keywords internal
hgch_theme <- function(opts = NULL) {

  highcharter::hc_theme(
    useHTML = TRUE,
    #colors = opts$palette_colors,
    styledMode = TRUE,
    chart = list(
      #reflow = TRUE,
      #renderTo = 'container',
      backgroundColor = opts$background_color,
      marginBottom = opts$plot_margin_bottom,
      marginLeft = opts$plot_margin_left,
      marginRight = opts$plot_margin_right,
      marginTop = opts$plot_margin_top,

      plotBackgroundColor = opts$plot_background_color %||% opts$background_color,
      borderColor = opts$plot_border_color,
      borderWidth = opts$plot_border_width,
      style = list (
        #fontFamily = opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )),
    title = list(
      useHTML = TRUE,
      align = opts$title_align,
      style = list(
        fontFamily = opts$title_family,
        fontSize = paste0(opts$title_size, 'px'),
        color = opts$title_color %||% opts$text_color,
        fontWeight = opts$title_weight
      )
    ),
    subtitle = list(
      useHTML = TRUE,
      align = opts$subtitle_align,
      style = list(
        fontFamily = opts$title_family %||% opts$text_family,
        fontSize = paste0(opts$subtitle_size, 'px'),
        color = opts$subtitle_color %||% opts$text_color,
        fontWeight = opts$subtitle_weight

      )
    ),
    credits = list(
      useHTML = TRUE,
      href = opts$caption_link,
      margin = opts$caption_margin,
      #position = list(
      # align = opts$caption_align,
      #  x = ifelse(opts$caption_align == "right",-20, 20),
      # y = opts$y_credits
      #),
      style = list(
        fontFamily = opts$caption_family %||% opts$text_family,
        fontSize = paste0(opts$caption_size, 'px'),
        color = opts$caption_color
      )
    ),
    pane = list(
      startAngle = opts$pane_start_angle %||% 0,
      endAngle = opts$pane_end_angle %||% 360
    ),
    xAxis = list(
      visible = opts$grid_x_show %||% TRUE,
      gridLineWidth = opts$grid_x_width,
      lineColor = opts$axis_line_x_color %||% opts$axis_line_color, #color del eje x
      tickColor = opts$axis_ticks_color,#color de las divisiones del eje x
      gridLineColor = opts$grid_x_color %||% opts$grid_color,
      gridLineDashStyle = opts$grid_x_line_type %||% opts$grid_line_type,
      tickLength = opts$axis_tick_length,
      lineWidth = opts$axis_line_x_size %||% opts$axis_line_size,
      labels = list(
        #rotation = opts$axis_x_rotation,
        enabled = opts$axis_x_datalabel_show %||% TRUE,
        style = list(
          color = opts$title_axis_color %||% opts$text_color, #opts$font_color, #color nombre de las etiquetas
          fontFamily = opts$text_family,
          fontSize = paste0(opts$text_size, 'px')
        )),
      title = list(
        margin = opts$axis_margin_x,
        style = list(
          color = opts$title_axis_color,# REVISAR VALOR color del titulo del eje
          fontSize = paste0(opts$title_axis_size, 'px')
        )
      )#,
      # plotLines = list(
      #   list(value = opts$plotLine_value_x,
      #        color = 'black',
      #        dashStyle = 'shortdash'#,
      #        #width = 2,
      #        #zIndex = 5,
      #        # label = list(
      #        #   text = lineLabelsXY[2],
      #        #   style = list(
      #        #     color = 'black'
      #        #   )
      #        # )
      #   ))
    ),
    yAxis = list(
      visible = opts$grid_y_show,
      gridLineWidth = opts$grid_y_width,
      lineColor = opts$axis_line_y_color %||% opts$axis_line_color, #color del eje x
      tickColor = opts$axis_ticks_color,#color de las divisiones del eje x
      gridLineColor = opts$grid_y_color %||% opts$grid_color,
      gridLineDashStyle = opts$grid_y_line_type %||% opts$grid_line_type,
      tickLength = opts$axis_tick_length,
      lineWidth = opts$axis_line_y_size %||% opts$axis_line_size,
      #TODO: AGREGAR max = opts$y_max,
      #TODO: AGREGAR min = opts$y_min,
      labels = list(
        enabled = opts$axis_y_datalabel_show %||% TRUE,
        #rotation = opts$axis_y_rotation,
        style = list(
          color = opts$title_axis_color %||% opts$text_color, #opts$font_color, #color nombre de las etiquetas
          fontFamily = opts$text_family,
          fontSize = paste0(opts$text_size, 'px')
        )),
      title = list(
        margin = opts$axis_margin_y,
        style = list(
          color = opts$title_axis_color %||% opts$text_color,# color del titulo del eje
          fontSize = paste0(opts$title_axis_size, 'px')
        )
      )#,
      #   # plotLines = list(
      #   #   list(value = opts$plotLine_value_y,
      #   #        color = 'black',
      #   #        dashStyle = 'shortdash'#,
      #   #        #width = 2,
      #   #        #zIndex = 5,
      #   #        # label = list(
      #   #        #   text = lineLabelsXY[2],
      #   #        #   style = list(
      #   #        #     color = 'black'
      #   #        #   )
      #   #        # )
      #   #   ))
    ),
    plotOptions = list (
      pie = list(
        innerSize = opts$pie_inner_size,
        borderRadius = 8,
        # animation = list(
        #   duration = opts$animation_duration
        # ),
        #dataLabels = list(distance = ifelse(opts$inner_dataLabels,-100, 30)),
        showInLegend = opts$legend_show

      ),
      # #   packedbubble = list(
      # #     minSize = opts$bubble_min,
      # #     maxSize = opts$bubble_max,
      # #     animation = list(
      # #       duration = opts$animation_duration
      # #     ),
      # #     # zMin = 0,
      # #     # zMax = 1000,
      # #     layoutAlgorithm = list(
      # #       splitSeries = FALSE,
      # #       gravitationalConstant = 0.02
      # #     ),
      # #     marker= list(
      # #       fillOpacity = opts$bubble_opacity)
      # #   ),
      # #
      series = list(
        connectNulls = opts$line_connect_na,
        # colorByPoint = opts$color_by_point,
        # animation = list(
        #   duration = opts$animation_duration
        # ),
        dataLabels = list (
          enabled = opts$datalabel_show,
          className = "custom-data-label",
          format = opts$datalabel_template,#'{y} %',
          #format = paste0("",opts$format_sample_num)
          style = list(
            useHTML = TRUE,
            color = opts$datalabel_color %||% opts$text_color,
            fontFamily = opts$text_family,
            fontSize = paste0(opts$datalabel_size %||% 11, "px"),
            textOutline = ifelse(opts$datalabel_text_outline_show, "1px contrast", "none")
          ),
          inside = opts$datalabel_inside#,
          #format = opts$templatedataLabels %||% paste0(opts$cats, opts$format_dataLabels),
          #verticalAlign = opts$data_labels_align#'middle'
        ),
        cursor =  opts$cursor,
        events = list(
          click = JS(opts$click_function)
        ),
        marker = list(
          enabled = opts$line_marker_show,
          symbol = "circle",
          radius = opts$line_marker_size
        )
      )
    ),

    legend = list(
      backgroundColor = opts$legend_background,
      borderColor = opts$legend_border_color,
      borderWidth = opts$legend_border_width,
      #maxHeight = opts$legend_max_height,
      # title = list(
      #   text = opts$legend_title),
      # layout = opts$legend_layout,
      # align = opts$legend_align,
      # #y = y_legend,
      # verticalAlign = opts$legend_verticalAlign,
      itemMarginTop = opts$legend_margin_top,
      itemMarginBottom = opts$legend_margin_bottom,
      #reversed = opts$legend_reversed,
      itemStyle = list(
        fontFamily = opts$legend_text_family %||% opts$text_family,
        fontSize = paste0(opts$legend_text_size %||% opts$text_size, 'px'),
        color = opts$legend_text_color %||% opts$text_color
      )
    ),

    tooltip = list(
      useHTML = TRUE,
      backgroundColor = opts$tooltip_background,
      borderColor = opts$tooltip_border_color,
      borderRadius = opts$tooltip_border_radius,
      borderWidth =  opts$tooltip_border_width,

      style = list(
        width = paste0(opts$tooltip_width, "px"),
        whiteSpace = 'normal',
        color = opts$tooltip_text_color,
        #cursor = "default",
        fontFamily = opts$tooltip_text_family %||% opts$text_family,
        fontSize = paste0(opts$text_size, 'px')
      )
    )
  )
}
