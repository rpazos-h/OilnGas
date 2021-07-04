plot_by_group <- function (df, ts_id, group_var, var, points = TRUE, error_bars = FALSE) 
{
    value <- se <- NULL
    if (!is.data.frame(df)) 
        stop("df needs to be a dataframe")
    df <- as.data.frame(df)
    if (!ts_id %in% colnames(df)) 
        stop("ts_id need to be in df")
    if (!group_var %in% colnames(df)) 
        stop("group_var need to be in df")
    if (!var %in% colnames(df)) 
        stop("var names need to be in df")
    df <- droplevels(df[stats::complete.cases(df[, c(ts_id, group_var, 
        var)]), c(ts_id, group_var, var)])
    df[, group_var] <- as.factor(df[, group_var])
    gf <- df %>% dplyr::group_by(!!rlang::sym(ts_id), !!rlang::sym(group_var)) %>% 
        dplyr::summarise(mean = mean(!!rlang::sym(var), na.rm = TRUE), 
            se = stats::sd(!!rlang::sym(var), na.rm = TRUE)/sqrt(length(which(!is.na(!!rlang::sym(var))))))
    gf <- as.data.frame(gf)
    if (error_bars) {
        if (is.factor(df[, ts_id])) {
            plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = ts_id, 
                color = group_var, group = group_var)) + ggplot2::stat_summary(fun = sum, 
                geom = "line", ggplot2::aes(y = mean)) + 
                ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - 
                  se, ymax = mean + se), width = 0.1) + ggplot2::xlab(ts_id) + 
                ggplot2::ylab(var)
        }
        else {
            plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = ts_id, 
                color = group_var, group = group_var)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - 
                se, ymax = mean + se), width = 0.1) + ggplot2::geom_line(ggplot2::aes(y = mean)) + 
                ggplot2::xlab(ts_id) + ggplot2::ylab(var)
        }
        if (points) 
            plot <- plot + ggplot2::geom_point(ggplot2::aes(y = mean))
    }
    else {
        plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = ts_id, 
            y = "mean", color = group_var, group = group_var)) + 
            ggplot2::geom_line() + ggplot2::xlab(ts_id) + ggplot2::ylab(var)
        if (points) 
            plot <- plot + ggplot2::geom_point()
    }
    list(df = gf, plot = plot)
}
