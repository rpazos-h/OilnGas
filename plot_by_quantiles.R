plot_by_quantiles <- function (df, ts_id, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), 
    var = utils::tail(colnames(df[sapply(df, is.numeric) & colnames(df) != 
        ts_id]), n = 1), points = TRUE) 
{
    if (!is.data.frame(df)) 
        stop("df needs to be a dataframe")
    df <- as.data.frame(df)
    if (!ts_id %in% colnames(df)) 
        stop("ts_id need to be in df")
    if (length(var) > 1) 
        stop("var needs to identify a single variable")
    if (!var %in% colnames(df)) 
        stop("var needs to be in df")
    df <- droplevels(df[stats::complete.cases(df[, c(ts_id, var)]), 
        c(ts_id, var)])
    df[, ts_id] <- try_convert_ts_id(df[, ts_id])
    q <- sapply(quantiles, function(x) {
        by(df[, which(!colnames(df) %in% ts_id)], df[, ts_id], 
            stats::quantile, na.rm = TRUE, x)
    })
    q <- data.frame(rownames(q), q)
    q[1] <- sort(unique(df[, ts_id]))
    colnames(q)[1] <- ts_id
    colnames(q)[2:(length(quantiles) + 1)] <- sprintf("q%02d", 
        quantiles * 100)
    gg <- tidyr::gather_(data = q, key_col = "quantile", 
        value_col = colnames(df)[!(ts_id == colnames(df))], gather_cols = colnames(q)[!(ts_id == 
            colnames(q))])
    gg$quantile <- factor(gg$quantile, levels = unique(gg$quantile))
    if (is.factor(df[, ts_id])) {
        plot <- ggplot2::ggplot(gg, ggplot2::aes_string(x = ts_id, 
            y = gg[, 3], color = "quantile", group = "quantile")) + 
            ggplot2::stat_summary(fun = sum, geom = "line") + 
            ggplot2::ylab(colnames(df[ncol(df)])) + ggplot2::scale_color_discrete(labels = quantiles)
    }
    else {
        plot <- ggplot2::ggplot(gg, ggplot2::aes_string(x = ts_id, 
            y = gg[, 3], color = "quantile")) + ggplot2::ylab(colnames(df[ncol(df)])) + 
            ggplot2::geom_line() + ggplot2::scale_color_discrete(labels = quantiles)
    }
    if (points) 
        plot <- plot + ggplot2::geom_point()
    list(df = gg, plot = plot)
}
