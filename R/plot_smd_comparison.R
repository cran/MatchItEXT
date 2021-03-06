#' Draw a dot-line plot that compares the SMD change before and after matching
#'
#' This function accepts the data set from compute_smd() and draws a dot-line
#' plot to compare the SMD change before and after matching. This function
#' differs from plot(summary(data, standardize = T) in MatchIt package in the
#' following aspects: (1) it uses the original SMD values, rather than the
#' absolute SMD values; (2) overall SMD line is colored in blue; (3) increased
#' SMD line is colored in brick red; (4) decreased SMD line is colored in gray.
#' This function depends on ggplot2. If users are not satisfied with the plot,
#' it can be revised with ggplot2. Relevant data and codes are stored in the
#' returned list.
#'
#' @param smd_data a data frame derived from \code{\link{compute_smd}}
#' @keywords SMD plot
#' @seealso plot_ps_qq()
#' @return Return a list of relevant data, code, and plot
#' @import ggplot2 dplyr
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' m_out <- MatchIt::matchit(treat ~ re74 + re75 + age + educ + hispan +
#'  black, data = MatchIt::lalonde, method = "nearest")
#' opt_smd <- compute_smd(m_out, sd = "treatment")
#' plot_smd_data <- plot_smd(opt_smd)
plot_smd <- function(smd_data = NULL) {
  if (isTRUE(class(smd_data)[1] == "data.frame" &
             class(smd_data)[2] == "smd.data")) {
    class(smd_data) <- class(smd_data)[1]
    df <- smd_data %>%
      dplyr::select(.data$smd_before, .data$smd_after) %>%
      tibble::rownames_to_column("var") %>%
      dplyr::mutate(trend =
          ifelse(abs(.data$smd_after) < abs(.data$smd_before), "decrease",
            ifelse(abs(.data$smd_after) > abs(.data$smd_before),
                   "increase", "no change")))
    df_increase <- df %>%
      dplyr::filter(.data$trend == "increase")
    df <- df %>%
      tidyr::pivot_longer(!c(.data$var, .data$trend), names_to = "stage",
                          values_to = "smd")

    df$trend[df$var == "distance"] <- "overall"
    df$stage <- factor(df$stage, levels = c("smd_before", "smd_after"))
    df$trend <- factor(df$trend, levels =
                         c("overall", "decrease", "increase", "no change"))
    colors <- c("royalblue", "gray70", "firebrick", "springgreen")

    ylimits <- range(c(smd_data$smd_before, smd_data$smd_after))
    p_smd<- ggplot(data = df, aes(x = stage, y = .data$smd, group = var)) +
      labs(title = "Comparison of SMD Before and After Matching",
           x = NULL, y = "Standardized Mean Difference" ) +
      geom_hline(yintercept = c(-0.2, -0.1, 0, 0.1, 0.2), color = "gray90") +
      scale_y_continuous(breaks = round(seq(ylimits[1], ylimits[2], 0.1),
                                        digits = 1)) +
      scale_x_discrete(breaks=c("smd_before","smd_after"),
                       labels=c("All Data", "Matched Data")) +
      geom_line(aes(colour = .data$trend)) +
      geom_point(aes(colour = .data$trend)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_colour_manual(values=colors)
    p_smd_code <- quote(ggplot(data = df,
        aes(x = stage, y = .data$smd, group = var)) +
        labs(title = "Comparison of SMD Before and After Matching",
            x = NULL, y = "Standardized Mean Difference" ) +
        geom_hline(yintercept = c(-0.2, -0.1, 0, 0.1, 0.2), color = "gray90") +
        scale_y_continuous(breaks = round(seq(-0.3, 1, 0.1), digits = 1)) +
        scale_x_discrete(breaks=c("smd_before","smd_after"),
          labels=c("All Data", "Matched Data")) +
        geom_line(aes(colour = .data$trend)) +
        geom_point(aes(colour = .data$trend)) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_colour_manual(values=colors))
    results<- list(data = df, smd_increase = df_increase,
                   plot_code = p_smd_code, colors = colors, plot = p_smd)
    return(results)
  } else {
    stop("This is not a data set derieved from MatchItEXT::compute_smd().")
  }

}



#' Draw QQ plots of propensity score between groups before and after matching
#'
#' This function accepts a MatchIt object (i.e., the result of matchit function)
#' and draw side-by-side QQ plots of propensity score between groups before and
#' after matching, for the purpose of comparison. Note only the results of
#' Subclassification, Nearest Neighbor Matching, Optimal Matching, Full
#' Matching, and Genetic Matching are acceptable. The results of Exact Matching
#' is not applicable to this function.
#'
#' @param mi_obj A matchit object derived from MatchIt pacakge
#' @keywords QQ plot
#' @seealso plot_smd()
#' @return Return a list of relevant data, code, and QQ plot
#' @export
#' @import ggplot2
#' @importFrom grDevices devAskNewPage
#' @importFrom stats qqplot
#' @examples
#' # take lalonde data as an example
#'  m_out <- MatchIt::matchit(treat ~ re74 + re75 + age + educ + hispan +
#'     black, data = MatchIt::lalonde, method = "nearest")
#'  plot_ps_qq(m_out)
#'
plot_ps_qq <- function(mi_obj = NULL) {
  # draw qq plot of propensity score between treatment and control group before
  # matching
  # Create the quantile-quantile data table
  if (is(mi_obj, "matchit") == FALSE) {
    stop("The input is not a matchit object!")
  }
  qq_table_bf <- qqplot(x = mi_obj$distance[mi_obj$treat == 0], y = mi_obj$distance[mi_obj$treat == 1], plot.it=FALSE)
  qq_table_bf <- as.data.frame(qq_table_bf)
  # Set the x and y limits
  xylim <- range(c(qq_table_bf$x, qq_table_bf$y))
  # Generate qq plot
  qq_bf <- ggplot(qq_table_bf, aes( x = .data$x, y = .data$y)) + geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    coord_fixed(ratio = 1, xlim=xylim, ylim = xylim) +
    xlab("Control Group") + ylab("Treatment Group") +
    ggtitle("Before Matching") + theme(plot.title = element_text(hjust = 0.5))
  qq_bf_code <- quote(ggplot(qq_table_bf, aes( x = .data$x, y = .data$y)) +
                        geom_point() +
                        geom_abline(intercept = 0, slope = 1) +
                        coord_fixed(ratio = 1, xlim=xylim, ylim = xylim) +
                        xlab("Control Group") + ylab("Treatment Group") +
                        ggtitle("Before Matching") +
                        theme(plot.title = element_text(hjust = 0.5)))

  # draw qq plot of propensity score between treatment and control group after
  # matching
  # Create the quantile-quantile data table
  df_af <- data.frame(ps = mi_obj$distance, wt = mi_obj$weights,
                      treat = mi_obj$treat)
  df_af_tr <- df_af[which(df_af$wt != 0 & df_af$treat == 1),]
  df_af_ctl <- df_af[which(df_af$wt != 0 & df_af$treat == 0),]
  # create quantile sequence
  n <- min(nrow(df_af_tr), nrow(df_af_ctl))
  quantile_seq <- (1:n) / n - 0.5 / n
  # create weighted quantile vector for control and treatment group
  quan_af_tr <- Hmisc::wtd.quantile(df_af_tr$ps, weights = df_af_tr$wt,
                                    probs = quantile_seq)
  quan_af_ctl <- Hmisc::wtd.quantile(df_af_ctl$ps, weights = df_af_ctl$wt,
                                     probs = quantile_seq)
  qq_table_af <- data.frame(x = quan_af_ctl, y = quan_af_tr)
  qq_af <- ggplot(data = qq_table_af, aes( x= .data$x, y = .data$y)) +
    geom_point() + geom_abline( intercept=0, slope=1) +
    coord_fixed(ratio = 1, xlim=xylim, ylim = xylim) +
    xlab("Control Group") + ylab("Treatment Group") +
    ggtitle("After Matching") + theme(plot.title = element_text(hjust = 0.5))
  qq_af_code <- quote(ggplot(data = qq_table_af,
                             aes( x= .data$x, y = .data$y)) +
                        geom_point() + geom_abline( intercept=0, slope=1) +
                        coord_fixed(ratio = 1, xlim=xylim, ylim = xylim) +
                        xlab("Control Group") + ylab("Treatment Group") +
                        ggtitle("After Matching") +
                        theme(plot.title = element_text(hjust = 0.5)))

  # combine two qq plots in a canvas
  devAskNewPage(ask = FALSE)
  qq_combined <- ggpubr::ggarrange(qq_bf, qq_af, nrow = 1)
  qq_final <- ggpubr::annotate_figure(qq_combined, top =
    ggpubr::text_grob("QQ Plot for Propensity Score before and after matching",
      face = "bold", size = 14))
  results <- list(qq_table_bf = qq_table_bf, xylim = xylim, qq_bf_code =
                    qq_bf_code, qq_table_af = qq_table_af, qq_af_code =
                    qq_af_code, plot = qq_final)
  return(results)
}
