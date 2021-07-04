barplot_bygroup <- function (df, by_var, var, stat_fun = mean, order_by_stat = FALSE, 
    color = "red") 
{
    if (!is.data.frame(df)) 
        stop("df needs to be a dataframe")
    df <- as.data.frame(df)
    stat_var <- paste0("stat_", var)
    df <- data.frame(by_var = df[, by_var], var = df[, var])
    df <- df %>% dplyr::group_by(by_var) %>% dplyr::summarize(stat = stat_fun(var, 
        na.rm = TRUE))
    colnames(df) <- c(by_var, stat_var)
    df <- as.data.frame(df)
    df[, by_var] <- as.factor(df[, by_var])
    if (order_by_stat) 
        df[, by_var] <- stats::reorder(df[, by_var], -df[, stat_var])
    else df[, by_var] <- stats::reorder(df[, by_var], nrow(df):1)
    plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = by_var, 
        y = stat_var)) + ggplot2::coord_flip() + ggplot2::geom_col(fill = color)
    list(df = df, plot = plot)
}
