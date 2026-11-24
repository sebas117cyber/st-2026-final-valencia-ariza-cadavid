
# 3. Reconocimiento: diferenciacion, ACF y PACF (compatible con leer_serie)

identificar_modelo <- function(y, rezagos = 15, titulo = NULL,
                               etiqueta_y = NULL, fechas = NULL,
                               graficar = TRUE, tabla = TRUE, usar_kable = FALSE) {
  
  fuente <- ""
  frecuencia <- NULL
  
  # Detectar si 'y' es el tibble generado por leer_serie()
  if (is.data.frame(y) && "y" %in% names(y)) {
    z0 <- suppressWarnings(as.numeric(y$y))
    if (is.null(fechas) && "fecha" %in% names(y)) fechas <- y$fecha
    if (is.null(etiqueta_y)) {
      u_attr <- attr(y, "unidad")
      etiqueta_y <- if (!is.null(u_attr) && nzchar(u_attr)) u_attr else "Valor"
    }
    f_attr <- attr(y, "fuente")
    if (!is.null(f_attr) && nzchar(f_attr)) fuente <- f_attr
    frecuencia <- attr(y, "frecuencia")
  } else {
    z0 <- suppressWarnings(as.numeric(y))
    if (is.null(etiqueta_y)) etiqueta_y <- "Valor"
    if (is.ts(y)) {
      frecuencia <- frequency(y)
      if (is.null(fechas)) {
        tiempos <- as.numeric(time(y))
        anios <- floor(tiempos)
        frac <- tiempos - anios
        fechas <- as.Date(paste0(anios, "-01-01")) + round(frac * 365.25)
      }
    }
  }
  
  if (anyNA(z0)) stop("La serie contiene valores NA o no numéricos.")
  if (is.null(titulo)) titulo <- if (nzchar(fuente)) fuente else "Serie"
  n0 <- length(z0)
  
  # Función auxiliar interna para ACF/PACF (usa correlaciones_muestrales si existe)
  calc_corr <- function(serie, k_max) {
    if (exists("correlaciones_muestrales", mode = "function")) {
      return(correlaciones_muestrales(serie, k_max))
    }
    a <- as.numeric(stats::acf(serie, lag.max = k_max, plot = FALSE)$acf)[-1]
    pa <- as.numeric(stats::pacf(serie, lag.max = k_max, plot = FALSE)$pacf)
    list(K = length(a), acf = a, pacf = pa, banda = 1.96 / sqrt(length(serie)))
  }
  
  mostrar_tab <- function(df, tit) {
    if (exists("ver_tabla", mode = "function")) {
      ver_tabla(df, tit, usar_kable)
    } else {
      cat("\n", tit, "\n", sep = "")
      print(df, row.names = FALSE)
    }
  }
  
  crear_plot_serie <- function(valores, idx_x, tit, ylab_txt, xlab_txt, sub_fuente = "") {
    df_s <- data.frame(x = idx_x, y = valores)
    es_fecha <- inherits(idx_x, c("Date", "POSIXt"))
    
    df_s$tooltip <- if (es_fecha) {
      paste0("Fecha: ", format(df_s$x, "%Y-%m-%d"), "<br>", ylab_txt, ": ", round(df_s$y, 4))
    } else {
      paste0("t: ", df_s$x, "<br>", ylab_txt, ": ", round(df_s$y, 4))
    }
    
    g <- ggplot(df_s, aes(x = x, y = y, group = 1, text = tooltip)) +
      geom_line(color = "#1F1F1F", linewidth = 0.6) +
      labs(title = tit, x = xlab_txt, y = ylab_txt) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#111111"),
        axis.title = element_text(face = "bold", size = 11, color = "#111111"),
        axis.text = element_text(color = "#555555", size = 9.5),
        axis.text.x = if (es_fecha) element_text(angle = 45, hjust = 1) else element_text(),
        panel.grid.major = element_line(color = "#EAEAEA", linewidth = 0.4),
        panel.grid.minor = element_line(color = "#F5F5F5", linewidth = 0.25)
      )
    
    if (es_fecha) g <- g + scale_x_date(date_labels = "%Y")
    
    tit_html <- if (nzchar(sub_fuente) && sub_fuente != tit) {
      paste0("<b>", tit, "</b><br><span style='font-size:12px;color:#555555;'>Fuente: ", sub_fuente, "</span>")
    } else {
      paste0("<b>", tit, "</b>")
    }
    
    ggplotly(g, tooltip = "text") %>%
      layout(
        title = list(text = tit_html, x = 0.5, xanchor = "center"),
        hovermode = "x unified",
        margin = list(t = if (nzchar(sub_fuente) && sub_fuente != tit) 65 else 45)
      )
  }
  
  crear_plot_corr <- function(corr_obj, sufijo = "") {
    k_val <- min(corr_obj$K, length(corr_obj$acf), length(corr_obj$pacf))
    lags <- seq_len(k_val)
    acf_v <- corr_obj$acf[lags]
    pacf_v <- corr_obj$pacf[lags]
    
    df_c <- data.frame(
      Rezago = lags,
      ACF = acf_v,
      PACF = pacf_v,
      tt_acf = paste0("Rezago: ", lags, "<br>ACF: ", round(acf_v, 4), "<br>Banda 95%: ±", round(corr_obj$banda, 4)),
      tt_pacf = paste0("Rezago: ", lags, "<br>PACF: ", round(pacf_v, 4), "<br>Banda 95%: ±", round(corr_obj$banda, 4))
    )
    
    tema_corr <- theme_minimal(base_size = 11.5) +
      theme(
        axis.title = element_text(size = 10.5, color = "#111111"),
        axis.text = element_text(color = "#555555", size = 9),
        panel.grid.major = element_line(color = "#EAEAEA", linewidth = 0.4),
        panel.grid.minor = element_line(color = "#F5F5F5", linewidth = 0.25)
      )
    
    lim_y <- c(min(-0.25, min(c(df_c$ACF, df_c$PACF, -corr_obj$banda)) - 0.05), 1.05)
    
    g_acf <- ggplot(df_c, aes(x = Rezago, y = ACF)) +
      geom_hline(yintercept = 0, color = "#111111", linewidth = 0.55) +
      geom_hline(yintercept = c(corr_obj$banda, -corr_obj$banda), linetype = "dashed", color = "#222222", linewidth = 0.5) +
      geom_segment(aes(xend = Rezago, y = 0, yend = ACF), color = "#2B2B2B", linewidth = 0.65) +
      geom_point(aes(text = tt_acf), alpha = 0, size = 2) +
      scale_y_continuous(limits = lim_y, breaks = seq(-0.25, 1, by = 0.25)) +
      labs(x = "Rezago", y = "ACF") +
      tema_corr
    
    g_pacf <- ggplot(df_c, aes(x = Rezago, y = PACF)) +
      geom_hline(yintercept = 0, color = "#111111", linewidth = 0.55) +
      geom_hline(yintercept = c(corr_obj$banda, -corr_obj$banda), linetype = "dashed", color = "#222222", linewidth = 0.5) +
      geom_segment(aes(xend = Rezago, y = 0, yend = PACF), color = "#2B2B2B", linewidth = 0.65) +
      geom_point(aes(text = tt_pacf), alpha = 0, size = 2) +
      scale_y_continuous(limits = lim_y, breaks = seq(-0.25, 1, by = 0.25)) +
      labs(x = "Rezago", y = "PACF") +
      tema_corr
    
    p1 <- ggplotly(g_acf, tooltip = "text")
    p2 <- ggplotly(g_pacf, tooltip = "text")
    
    subplot(p1, p2, nrows = 2, shareX = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.09) %>%
      layout(
        annotations = list(
          list(x = 0, y = 1.04, text = paste0("Función de Autocorrelación (ACF)", sufijo),
               xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE,
               font = list(size = 14, color = "#111111")),
          list(x = 0, y = 0.45, text = paste0("Función de Autocorrelación Parcial (PACF)", sufijo),
               xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE,
               font = list(size = 14, color = "#111111"))
        ),
        margin = list(t = 45, b = 45)
      )
  }
  
  c0 <- calc_corr(z0, min(rezagos, n0 - 1))
  idx0 <- if (!is.null(fechas)) fechas else seq_len(n0)
  xlab0 <- if (!is.null(fechas)) "Fecha" else "t"
  
  plots <- list()
  if (graficar) {
    plots$serie_niveles <- crear_plot_serie(z0, idx0, titulo, etiqueta_y, xlab0, fuente)
    plots$corr_niveles <- crear_plot_corr(c0)
    print(plots$serie_niveles)
    print(plots$corr_niveles)
  }
  
  # Prueba de estacionariedad para definir d en {0, 1, 2}
  z <- z0
  d <- 0
  filas <- list()
  
  for (paso in 0:2) {
    m0 <- length(z)
    dz <- diff(z)
    X <- cbind(1, z[1:(m0 - 1)])
    b <- solve(crossprod(X), crossprod(X, dz))
    s2 <- sum((dz - X %*% b)^2) / (length(dz) - 2)
    t_df <- b[2] / sqrt(s2 * solve(crossprod(X))[2, 2])
    
    estac <- (t_df < -2.88)
    sube_var <- (paso >= 1 && var(dz) > var(z))
    dec <- if (estac) "Estacionaria" else if (sube_var) "Sube varianza (parar)" else if (paso == 2) "Tope d=2" else "Diferenciar"
    
    filas[[paso + 1]] <- data.frame(
      d = paso,
      Estadistico.DF = round(t_df, 4),
      Critico.5pct = -2.88,
      Varianza = round(var(z), 4),
      Decision = dec
    )
    
    if (estac || sube_var || paso == 2) { d <- paso; break }
    z <- dz
    d <- paso + 1
  }
  tab_df <- do.call(rbind, filas)
  
  ce <- if (d == 0) c0 else calc_corr(z, min(rezagos, length(z) - 1))
  k_ce <- min(ce$K, length(ce$acf), length(ce$pacf))
  acf_ce <- ce$acf[seq_len(k_ce)]
  pacf_ce <- ce$pacf[seq_len(k_ce)]
  
  sa <- which(abs(acf_ce) > ce$banda)
  sp <- which(abs(pacf_ce) > ce$banda)
  
  tab_corr <- data.frame(
    Rezago = seq_len(k_ce),
    ACF = round(acf_ce, 4),
    PACF = round(pacf_ce, 4),
    Banda.95 = round(ce$banda, 4),
    Sig.ACF = ifelse(abs(acf_ce) > ce$banda, "*", ""),
    Sig.PACF = ifelse(abs(pacf_ce) > ce$banda, "*", "")
  )
  
  if (tabla) {
    mostrar_tab(tab_df, paste0("Diferenciacion - ", titulo, ":"))
    mostrar_tab(tab_corr, paste0("Autocorrelaciones (d = ", d, ") - ", titulo, ":"))
  }
  
  if (graficar && d > 0) {
    idx_d <- if (!is.null(fechas)) fechas[(d + 1):n0] else seq_along(z)
    ylab_d <- paste0("∇^", d, " ", etiqueta_y)
    plots$serie_diff <- crear_plot_serie(z, idx_d, paste0(titulo, " (d = ", d, ")"), ylab_d, xlab0, fuente)
    plots$corr_diff <- crear_plot_corr(ce, sufijo = paste0(" (d = ", d, ")"))
    print(plots$serie_diff)
    print(plots$corr_diff)
  }
  
  list(d = d, serie_estacionaria = z, acf = acf_ce, pacf = pacf_ce, banda = ce$banda,
       sig_acf = sa, sig_pacf = sp, tabla_df = tab_df, tabla_corr = tab_corr,
       frecuencia = frecuencia, graficos = plots)
}



# 4. Estimacion ARIMA / SARIMA por ML y CSS (compatible con leer_serie)

estimar_arima <- function(y, orden = NULL, seasonal = NULL, metodo = "ML",
                          con_media = TRUE, tabla = TRUE, graficar = TRUE,
                          usar_kable = FALSE) {
  
  fuente <- ""
  etiqueta_y <- expression(y[t])
  fechas <- NULL
  
  if (is.data.frame(y) && "y" %in% names(y)) {
    y_num <- suppressWarnings(as.numeric(y$y))
    if ("fecha" %in% names(y)) fechas <- y$fecha
    u_attr <- attr(y, "unidad")
    if (!is.null(u_attr) && nzchar(u_attr)) etiqueta_y <- u_attr
    f_attr <- attr(y, "fuente")
    if (!is.null(f_attr) && nzchar(f_attr)) fuente <- f_attr
  } else {
    y_num <- suppressWarnings(as.numeric(y))
  }
  
  if (anyNA(y_num)) stop("La serie contiene valores NA o no numéricos.")
  
  mostrar_tab <- function(df, tit) {
    if (exists("ver_tabla", mode = "function")) {
      ver_tabla(df, tit, usar_kable)
    } else {
      cat("\n", tit, "\n", sep = "")
      print(df, row.names = FALSE)
    }
  }
  
  # Resolver 'orden' y 'seasonal'
  if (is.null(orden)) {
    if (exists("seleccionar_candidatos", mode = "function")) {
      res_sel <- seleccionar_candidatos(y, tabla = FALSE, graficar = FALSE)
      orden <- res_sel$ganador$orden
      if (is.null(seasonal) && !is.null(res_sel$ganador$seasonal)) {
        seasonal <- res_sel$ganador$seasonal
      }
    } else {
      stop("Debes pasar 'orden' o tener cargada seleccionar_candidatos().")
    }
  } else if (is.list(orden)) {
    gan <- if (!is.null(orden$ganador)) orden$ganador else orden
    if (!is.null(gan$orden)) orden <- gan$orden
    if (is.null(seasonal) && !is.null(gan$seasonal)) seasonal <- gan$seasonal
  }
  
  metodo <- toupper(metodo)
  p <- orden[1]; d <- orden[2]; q <- orden[3]
  es_sarima <- !is.null(seasonal) && any(seasonal$order > 0)
  
  # Helper de respaldo robusto con stats::arima (para SARIMA o fallos numéricos)
  ajustar_stats_arima <- function(etiqueta_mod, seas_list = NULL) {
    fit <- if (is.null(seas_list)) {
      stats::arima(y_num, order = c(p, d, q), include.mean = con_media, method = metodo)
    } else {
      stats::arima(y_num, order = c(p, d, q), seasonal = seas_list, method = metodo)
    }
    cf <- coef(fit)
    ee <- suppressWarnings(sqrt(diag(vcov(fit))))
    D_ord <- if (is.null(seas_list)) 0 else seas_list$order[2]
    s_per <- if (is.null(seas_list)) 0 else seas_list$period
    N_eff <- length(y_num) - d - D_ord * s_per
    npar_f <- length(cf)
    
    if (npar_f > 0) {
      t_est <- cf / ee
      p_val <- 2 * (1 - pt(abs(t_est), df = max(1, N_eff - npar_f)))
      tab_c <- data.frame(
        Parametro = names(cf),
        Estimacion = round(cf, 4),
        Error.Est = round(ee, 4),
        IC.Inf.95 = round(cf - 1.96 * ee, 4),
        IC.Sup.95 = round(cf + 1.96 * ee, 4),
        Estadistico.t = round(t_est, 4),
        p.valor = round(p_val, 4),
        row.names = NULL
      )
    } else {
      tab_c <- data.frame()
    }
    
    aic_v <- AIC(fit)
    bic_v <- BIC(fit)
    tab_a <- data.frame(Sigma2 = round(fit$sigma2, 4), LogLik = round(fit$loglik, 4),
                        AIC = round(aic_v, 4), BIC = round(bic_v, 4))
    res_v <- as.numeric(residuals(fit))
    aj_v <- y_num - res_v
    
    list(coeficientes = cf, tabla = tab_c, tabla_ajuste = tab_a,
         sigma2 = fit$sigma2, loglik = fit$loglik, aic = aic_v, bic = bic_v,
         residuos = res_v, res_std = res_v / sqrt(fit$sigma2),
         ajustados = aj_v, orden = orden, seasonal = seas_list,
         metodo = metodo, n = N_eff)
  }
  
  # 1. Si es SARIMA
  if (es_sarima) {
    P <- seasonal$order[1]; D <- seasonal$order[2]; Q <- seasonal$order[3]
    s <- seasonal$period
    etiqueta_modelo <- paste0("SARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")[", s, "]")
    res_obj <- ajustar_stats_arima(etiqueta_modelo, list(order = c(P, D, Q), period = s))
    
    if (tabla) {
      if (nrow(res_obj$tabla) > 0) mostrar_tab(res_obj$tabla, paste0("Parametros ", etiqueta_modelo, " - ", metodo, ":"))
      mostrar_tab(res_obj$tabla_ajuste, "Criterios de ajuste:")
    }
    if (graficar) {
      eje_x <- if (!is.null(fechas)) fechas else seq_along(y_num)
      tit_plot <- if (nzchar(fuente)) paste0("Real vs Ajustada ", etiqueta_modelo, " - ", fuente) else paste0("Real vs Ajustada ", etiqueta_modelo)
      plot(eje_x, y_num, type = "l", col = "steelblue", lwd = 1.5,
           main = tit_plot, xlab = if (!is.null(fechas)) "Fecha" else "Tiempo", ylab = etiqueta_y)
      lines(eje_x, res_obj$ajustados, col = "darkred", lty = 2, lwd = 1.5)
      legend("topleft", c("Real", "Ajustada"), col = c("steelblue", "darkred"), lty = c(1, 2), bty = "n")
    }
    return(res_obj)
  }
  
  # 2. Si es ARIMA no estacional: intenta estimación propia (Harvey/CSS) y respalda con stats::arima
  etiqueta_modelo <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  w <- if (d > 0) diff(y_num, differences = d) else y_num
  N <- length(w)
  inc_mu <- isTRUE(con_media) && (d == 0)
  tiene_aux <- all(vapply(c("innovaciones_cond", "raices_fuera", "filtro_harvey", "matriz_hessiana"),
                          exists, logical(1), mode = "function"))
  
  res_obj <- tryCatch({
    if (!tiene_aux) stop("Usar respaldo stats::arima")
    
    desempacar <- function(par) {
      list(
        phi = if (p > 0) par[1:p] else numeric(0),
        theta = if (q > 0) par[(p + 1):(p + q)] else numeric(0),
        mu = if (inc_mu) par[p + q + 1] else 0
      )
    }
    
    neg_ll_css <- function(par) {
      pr <- desempacar(par)
      e <- innovaciones_cond(w, pr$phi, pr$theta, pr$mu)
      m <- length(e)
      s2 <- sum(e^2) / m
      if (!is.finite(s2) || s2 <= 1e-12) return(1e12)
      val <- 0.5 * m * (log(2 * pi) + log(s2) + 1)
      if (!is.finite(val)) 1e12 else val
    }
    
    neg_ll_ml <- function(par) {
      pr <- desempacar(par)
      if (p > 0 && !raices_fuera(-pr$phi)) return(1e12)
      kf <- filtro_harvey(w - pr$mu, pr$phi, pr$theta)
      if (is.null(kf) || any(!is.finite(kf$v)) || any(!is.finite(kf$Fv)) || any(kf$Fv <= 1e-12)) return(1e12)
      s2 <- mean(kf$v^2 / kf$Fv)
      if (!is.finite(s2) || s2 <= 1e-12) return(1e12)
      val <- 0.5 * N * (log(2 * pi) + log(s2) + 1) + 0.5 * sum(log(kf$Fv))
      if (!is.finite(val)) 1e12 else val
    }
    
    npar <- p + q + as.integer(inc_mu)
    if (npar == 0) {
      s2 <- sum(w^2) / N
      ll <- -0.5 * N * (log(2 * pi) + log(s2) + 1)
      aic <- -2 * ll + 2
      bic <- -2 * ll + log(N)
      tab_aj <- data.frame(Sigma2 = round(s2, 4), LogLik = round(ll, 4), AIC = round(aic, 4), BIC = round(bic, 4))
      ajustados <- c(rep(NA, d), tail(y_num, N) - w)
      list(coeficientes = numeric(0), tabla = data.frame(), tabla_ajuste = tab_aj,
           phi = numeric(0), theta = numeric(0), media = 0, sigma2 = s2, loglik = ll,
           aic = aic, bic = bic, residuos = w, res_std = w / sqrt(s2),
           ajustados = ajustados, orden = orden, seasonal = NULL, metodo = metodo, n = N)
    } else {
      noms <- c(if (p > 0) paste0("ar", 1:p), if (q > 0) paste0("ma", 1:q), if (inc_mu) "media")
      p0 <- rep(0, npar)
      if (inc_mu) p0[npar] <- mean(w)
      
      opt_css <- optim(p0, neg_ll_css, method = "BFGS")
      if (!is.finite(opt_css$value) || opt_css$value >= 1e11) {
        opt_css <- optim(p0, neg_ll_css, method = "Nelder-Mead", control = list(maxit = 2000))
      }
      
      if (metodo == "ML") {
        p_ini <- opt_css$par
        if (p > 0 && !raices_fuera(-p_ini[1:p])) p_ini[1:p] <- 0
        obj <- neg_ll_ml
        opt <- optim(p_ini, obj, method = "BFGS")
        if (!is.finite(opt$value) || opt$value >= 1e11) {
          opt <- optim(p_ini, obj, method = "Nelder-Mead", control = list(maxit = 2000))
        }
        if (!is.finite(opt$value) || opt$value >= 1e11) stop("No convergió ML propio")
        pf <- desempacar(opt$par)
        kf <- filtro_harvey(w - pf$mu, pf$phi, pf$theta)
        s2 <- mean(kf$v^2 / kf$Fv)
        ll <- -opt$value
        res <- kf$v
        res_std <- kf$v / sqrt(s2 * kf$Fv)
      } else {
        if (!is.finite(opt_css$value) || opt_css$value >= 1e11) stop("No convergió CSS propio")
        obj <- neg_ll_css
        opt <- opt_css
        pf <- desempacar(opt$par)
        e <- innovaciones_cond(w, pf$phi, pf$theta, pf$mu)
        s2 <- mean(e^2)
        ll <- -opt$value
        res <- e
        res_std <- e / sqrt(s2)
      }
      
      H <- matriz_hessiana(obj, opt$par)
      ee <- suppressWarnings(sqrt(diag(tryCatch(solve(H), error = function(e) matrix(NA, npar, npar)))))
      t_est <- opt$par / ee
      p_val <- 2 * (1 - pt(abs(t_est), df = max(1, N - npar)))
      
      tab_coef <- data.frame(
        Parametro = noms,
        Estimacion = round(opt$par, 4),
        Error.Est = round(ee, 4),
        IC.Inf.95 = round(opt$par - 1.96 * ee, 4),
        IC.Sup.95 = round(opt$par + 1.96 * ee, 4),
        Estadistico.t = round(t_est, 4),
        p.valor = round(p_val, 4)
      )
      
      k_tot <- npar + 1
      aic <- -2 * ll + 2 * k_tot
      bic <- -2 * ll + k_tot * log(N)
      tab_aj <- data.frame(Sigma2 = round(s2, 4), LogLik = round(ll, 4), AIC = round(aic, 4), BIC = round(bic, 4))
      names(opt$par) <- noms
      ajustados <- c(rep(NA, d), tail(y_num, N) - res)
      
      list(coeficientes = opt$par, tabla = tab_coef, tabla_ajuste = tab_aj,
           phi = pf$phi, theta = pf$theta, media = pf$mu, sigma2 = s2, loglik = ll,
           aic = aic, bic = bic, residuos = res, res_std = res_std,
           ajustados = ajustados, orden = orden, seasonal = NULL, metodo = metodo, n = N)
    }
  }, error = function(e) {
    ajustar_stats_arima(etiqueta_modelo, NULL)
  })
  
  if (tabla) {
    if (nrow(res_obj$tabla) > 0) mostrar_tab(res_obj$tabla, paste0("Parametros ", etiqueta_modelo, " - ", metodo, ":"))
    mostrar_tab(res_obj$tabla_ajuste, "Criterios de ajuste:")
  }
  
  if (graficar) {
    eje_x <- if (!is.null(fechas)) fechas else seq_along(y_num)
    tit_plot <- if (nzchar(fuente)) paste0("Real vs Ajustada ", etiqueta_modelo, " - ", fuente) else paste0("Real vs Ajustada ", etiqueta_modelo)
    plot(eje_x, y_num, type = "l", col = "steelblue", lwd = 1.5,
         main = tit_plot, xlab = if (!is.null(fechas)) "Fecha" else "Tiempo", ylab = etiqueta_y)
    lines(eje_x, res_obj$ajustados, col = "darkred", lty = 2, lwd = 1.5)
    legend("topleft", c("Real", "Ajustada"), col = c("steelblue", "darkred"), lty = c(1, 2), bty = "n")
  }
  
  res_obj
}



# 5. Comparacion de modelos candidatos guiada por ACF y PACF (AR, MA, ARMA,
#    I, ARI, IMA, ARIMA, SARIMA)

seleccionar_candidatos <- function(y, d = NULL, p_max = 3, q_max = 3,
                                   D = NULL, P_max = 1, Q_max = 1, s = NULL,
                                   guiado_acf_pacf = TRUE, metodo = "ML",
                                   tabla = TRUE, graficar = FALSE, usar_kable = FALSE) {
  
  y_num <- if (is.data.frame(y) && "y" %in% names(y)) suppressWarnings(as.numeric(y$y)) else suppressWarnings(as.numeric(y))
  if (anyNA(y_num)) stop("La serie contiene valores NA o no numéricos.")
  
  # Detectar frecuencia estacional desde leer_serie() o ts
  if (is.null(s)) {
    f_attr <- attr(y, "frecuencia")
    if (!is.null(f_attr) && is.numeric(f_attr)) {
      s <- f_attr
    } else if (is.ts(y)) {
      s <- frequency(y)
    } else {
      s <- 12
    }
  }
  
  # Identificar d y obtener ACF/PACF de la serie estacionaria
  id_info <- NULL
  if (exists("identificar_modelo", mode = "function")) {
    id_info <- identificar_modelo(y, rezagos = max(24, if (!is.null(s)) 2 * s else 24),
                                  graficar = FALSE, tabla = FALSE)
    if (is.null(d)) d <- id_info$d
  } else if (is.null(d)) {
    d <- 0
  }
  
  w <- if (d > 0) diff(y_num, differences = d) else y_num
  k_max <- min(max(24, if (!is.null(s)) 2 * s else 24), length(w) - 1)
  
  if (!is.null(id_info) && id_info$d == d && length(id_info$acf) >= min(12, k_max)) {
    acf_v <- id_info$acf
    pacf_v <- id_info$pacf
    banda <- id_info$banda
  } else if (exists("correlaciones_muestrales", mode = "function")) {
    cm <- correlaciones_muestrales(w, k_max)
    acf_v <- cm$acf
    pacf_v <- cm$pacf
    banda <- cm$banda
  } else {
    acf_v <- as.numeric(stats::acf(w, lag.max = k_max, plot = FALSE)$acf)[-1]
    pacf_v <- as.numeric(stats::pacf(w, lag.max = k_max, plot = FALSE)$pacf)
    banda <- 1.96 / sqrt(length(w))
  }
  
  # Etiquetado exacto según familia (AR, MA, ARMA, I, ARI, IMA, ARIMA, SARIMA)
  clasificar_modelo <- function(p, d, q, P = 0, D = 0, Q = 0, s = NULL) {
    if (P > 0 || D > 0 || Q > 0) {
      sufijo_s <- if (!is.null(s) && s > 1) paste0("[", s, "]") else ""
      return(list(
        nombre = paste0("SARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")", sufijo_s),
        familia = "SARIMA"
      ))
    }
    if (d == 0) {
      if (p > 0 && q == 0) return(list(nombre = paste0("AR(", p, ")"), familia = "AR"))
      if (p == 0 && q > 0) return(list(nombre = paste0("MA(", q, ")"), familia = "MA"))
      if (p > 0 && q > 0)  return(list(nombre = paste0("ARMA(", p, ",", q, ")"), familia = "ARMA"))
    } else {
      if (p == 0 && q == 0) return(list(nombre = paste0("I(", d, ")"), familia = "I"))
      if (p > 0 && q == 0)  return(list(nombre = paste0("ARI(", p, ",", d, ")"), familia = "ARI"))
      if (p == 0 && q > 0)  return(list(nombre = paste0("IMA(", d, ",", q, ")"), familia = "IMA"))
      return(list(nombre = paste0("ARIMA(", p, ",", d, ",", q, ")"), familia = "ARIMA"))
    }
  }
  
  # Construcción de la lista de candidatos (p, d, q, P, D, Q)
  candidatos_espec <- list()
  agregar_spec <- function(p, q, P = 0, D_val = 0, Q = 0) {
    if (p == 0 && q == 0 && d == 0 && P == 0 && Q == 0 && D_val == 0) return()
    clave <- paste(p, d, q, P, D_val, Q, sep = "_")
    candidatos_espec[[clave]] <<- c(p = p, d = d, q = q, P = P, D = D_val, Q = Q)
  }
  
  diagnostico_txt <- ""
  
  if (isTRUE(guiado_acf_pacf)) {
    # 1. Rezagos no estacionales (hasta s-1 o 10)
    lim_ns <- min(length(acf_v), length(pacf_v), 10, if (!is.null(s) && s > 2) s - 1 else 10)
    lags_ns <- seq_len(lim_ns)
    
    contar_corte <- function(vals, max_ord) {
      ord <- 0
      fallos <- 0
      for (k in seq_len(min(length(vals), max_ord))) {
        if (abs(vals[k]) > 0.90 * banda) {
          ord <- k
          fallos <- 0
        } else {
          fallos <- fallos + 1
          if (fallos >= 2) break
        }
      }
      ord
    }
    
    p_sug <- contar_corte(pacf_v[lags_ns], p_max)
    q_sug <- contar_corte(acf_v[lags_ns], q_max)
    n_sig_acf <- sum(abs(acf_v[lags_ns]) > banda)
    n_sig_pacf <- sum(abs(pacf_v[lags_ns]) > banda)
    
    # Determinar patrón: AR/ARI, MA/IMA, ARMA/ARIMA o ruido blanco
    if (p_sug == 0 && q_sug == 0) {
      patron <- if (d == 0) "Ruido blanco (se prueban AR(1), MA(1), ARMA(1,1))" else paste0("I(", d, ") / caminata aleatoria")
      if (d > 0) agregar_spec(0, 0)
      if (p_max >= 1) agregar_spec(1, 0)
      if (q_max >= 1) agregar_spec(0, 1)
      if (p_max >= 1 && q_max >= 1) agregar_spec(1, 1)
      
    } else if (p_sug > 0 && (q_sug == 0 || (n_sig_acf > p_sug && abs(pacf_v[1]) >= abs(acf_v[1])))) {
      # Corte en PACF + decaimiento en ACF => Familia AR / ARI (más benchmark ARMA/ARIMA)
      fam_esp <- if (d == 0) "AR" else "ARI"
      patron <- paste0("Corte en PACF (rezago ", p_sug, ") y decaimiento en ACF -> Familia ", fam_esp)
      for (p_i in seq_len(min(p_max, max(p_sug, 2)))) {
        agregar_spec(p_i, 0)
      }
      if (p_max >= 1 && q_max >= 1) agregar_spec(1, 1)
      if (p_sug >= 2 && p_max >= 2 && q_max >= 1) agregar_spec(2, 1)
      
    } else if (q_sug > 0 && (p_sug == 0 || (n_sig_pacf > q_sug && abs(acf_v[1]) > abs(pacf_v[1])))) {
      # Corte en ACF + decaimiento en PACF => Familia MA / IMA (más benchmark ARMA/ARIMA)
      fam_esp <- if (d == 0) "MA" else "IMA"
      patron <- paste0("Corte en ACF (rezago ", q_sug, ") y decaimiento en PACF -> Familia ", fam_esp)
      for (q_i in seq_len(min(q_max, max(q_sug, 2)))) {
        agregar_spec(0, q_i)
      }
      if (p_max >= 1 && q_max >= 1) agregar_spec(1, 1)
      
    } else {
      # Ambas significativas => Familia mixta ARMA / ARIMA + puros de referencia
      fam_esp <- if (d == 0) "ARMA" else "ARIMA"
      patron <- paste0("ACF y PACF significativas -> Familia ", fam_esp)
      p_lim <- min(p_max, max(p_sug, 2))
      q_lim <- min(q_max, max(q_sug, 2))
      if (d > 0) agregar_spec(0, 0)
      for (p_i in seq_len(p_lim)) agregar_spec(p_i, 0)
      for (q_i in seq_len(q_lim)) agregar_spec(0, q_i)
      for (p_i in seq_len(p_lim)) {
        for (q_i in seq_len(q_lim)) {
          agregar_spec(p_i, q_i)
        }
      }
    }
    
    # 2. Evaluar componente estacional (SARIMA) si el rezago 's' es significativo o si se pidió D > 0
    tiene_rezago_s <- (!is.null(s) && s > 1 && length(acf_v) >= s && length(w) >= 2 * s + 5) &&
      ((abs(acf_v[s]) > 0.90 * banda) || (abs(pacf_v[s]) > 0.90 * banda))
    forzar_estac <- (!is.null(D) && D > 0)
    
    if (tiene_rezago_s || forzar_estac) {
      D_vals <- if (!is.null(D)) D else if (abs(acf_v[s]) > 0.45) c(0, 1) else 0
      p_base <- max(1, min(p_max, p_sug))
      q_base <- min(q_max, q_sug)
      
      for (D_i in D_vals) {
        if (P_max >= 1) agregar_spec(p_base, 0, P = 1, D_val = D_i, Q = 0)
        if (Q_max >= 1) agregar_spec(p_base, 0, P = 0, D_val = D_i, Q = 1)
        if (P_max >= 1 && Q_max >= 1) agregar_spec(p_base, 0, P = 1, D_val = D_i, Q = 1)
        if (q_base >= 1 && P_max >= 1) agregar_spec(p_base, q_base, P = 1, D_val = D_i, Q = 0)
      }
      patron <- paste0(patron, " + Estacionalidad (s=", s, ") -> SARIMA")
    }
    
    diagnostico_txt <- patron
    
  } else {
    # Búsqueda exhaustiva en grilla si guiado_acf_pacf = FALSE
    D_grid <- if (is.null(D)) 0 else D
    for (p in 0:p_max) {
      for (q in 0:q_max) {
        for (P in 0:P_max) {
          for (Q in 0:Q_max) {
            agregar_spec(p, q, P, D_grid, Q)
          }
        }
      }
    }
    diagnostico_txt <- "Búsqueda completa en grilla"
  }
  
  modelos <- list()
  filas <- list()
  primer_error <- NULL
  
  for (sp_vec in candidatos_espec) {
    p <- sp_vec["p"]; q <- sp_vec["q"]
    P <- sp_vec["P"]; D_val <- sp_vec["D"]; Q <- sp_vec["Q"]
    
    info_nom <- clasificar_modelo(p, d, q, P, D_val, Q, s)
    nom <- info_nom$nombre
    fam <- info_nom$familia
    es_sarima <- (P > 0 || D_val > 0 || Q > 0)
    seas_arg <- if (es_sarima) list(order = c(P, D_val, Q), period = s) else NULL
    
    mod <- tryCatch({
      estimar_arima(y, orden = c(p, d, q), seasonal = seas_arg,
                    metodo = metodo, tabla = FALSE, graficar = FALSE)
    }, error = function(e) {
      if (is.null(primer_error)) primer_error <<- conditionMessage(e)
      NULL
    })
    
    if (!is.null(mod) && is.finite(mod$bic)) {
      modelos[[nom]] <- mod
      filas[[length(filas) + 1]] <- data.frame(
        Modelo = nom,
        Familia = fam,
        Parametros = p + q + P + Q,
        Sigma2 = round(mod$sigma2, 4),
        AIC = round(mod$aic, 4),
        BIC = round(mod$bic, 4)
      )
    }
  }
  
  if (length(filas) == 0) {
    msg <- "No se pudo estimar ningún modelo candidato válido."
    if (!is.null(primer_error)) msg <- paste0(msg, " Causa: ", primer_error)
    stop(msg)
  }
  
  tab_comp <- do.call(rbind, filas)
  tab_comp <- tab_comp[order(tab_comp$BIC), ]
  rownames(tab_comp) <- NULL
  ganador <- tab_comp$Modelo[1]
  
  if (tabla) {
    titulo_tab <- paste0("Candidatos (", diagnostico_txt, " | Ganador BIC: ", ganador, "):")
    if (exists("ver_tabla", mode = "function")) {
      ver_tabla(tab_comp, titulo_tab, usar_kable)
    } else {
      cat("\n", titulo_tab, "\n", sep = "")
      print(tab_comp, row.names = FALSE)
    }
  }
  
  if (graficar) {
    mat <- rbind(AIC = tab_comp$AIC, BIC = tab_comp$BIC)
    colnames(mat) <- tab_comp$Modelo
    par(mar = c(9, 4, 3, 1))
    barplot(mat, beside = TRUE, col = c("steelblue", "darkorange2"), las = 2, cex.names = 0.8,
            main = "AIC y BIC por modelo candidato", ylab = "Criterio",
            ylim = c(min(mat) - 10, max(mat) + 10), xpd = FALSE)
    legend("topleft", c("AIC", "BIC"), fill = c("steelblue", "darkorange2"), bty = "n")
    par(mar = c(5, 4, 4, 2))
  }
  
  list(tabla = tab_comp, ganador = modelos[[ganador]], modelos = modelos,
       diagnostico = diagnostico_txt)
}


# Auxiliares para tablas, raices, verosimilitud y correlaciones

ver_tabla <- function(df, titulo = NULL, usar_kable = TRUE) {
  if (usar_kable) {
    cap <- if (!is.null(titulo)) sub(":\\s*$", "", titulo) else NULL
    names(df) <- gsub("\\.", " ", names(df))
    df[] <- lapply(df, function(col) ifelse(is.na(col), "-", as.character(col)))
    cat("\n\n")
    print(knitr::kable(df, format = "pipe", align = "c", caption = cap, row.names = FALSE))
    cat("\n\n")
  } else {
    if (!is.null(titulo)) cat("\n", titulo, "\n", sep = "")
    print(df, row.names = FALSE)
  }
}

# Revisa que las raices del polinomio queden fuera del circulo unitario
raices_fuera <- function(coefs) {
  if (length(coefs) == 0 || all(coefs == 0)) return(TRUE)
  all(Mod(polyroot(c(1, coefs))) > 1.001)
}

# Residuos condicionales (CSS) por recursion hacia adelante
innovaciones_cond <- function(x, phi, theta, mu) {
  p <- length(phi)
  q <- length(theta)
  n <- length(x)
  z <- x - mu
  e <- numeric(n)
  
  for (t in (p + 1):n) {
    ar <- if (p > 0) sum(phi * z[(t - 1):(t - p)]) else 0
    ma <- 0
    if (q > 0) {
      k <- min(q, t - 1)
      ma <- sum(theta[1:k] * e[(t - 1):(t - k)])
    }
    e[t] <- z[t] - ar - ma
  }
  e[(p + 1):n]
}

# Filtro de Kalman en espacio de estados para verosimilitud exacta
filtro_harvey <- function(z, phi, theta) {
  p <- length(phi)
  q <- length(theta)
  n <- length(z)
  r <- max(p, q + 1, 1)
  
  Tm <- matrix(0, r, r)
  if (p > 0) Tm[1:p, 1] <- phi
  if (r > 1) {
    for (i in 1:(r - 1)) Tm[i, i + 1] <- 1
  }
  
  Rv <- numeric(r)
  Rv[1] <- 1
  if (q > 0) Rv[2:(q + 1)] <- theta
  Q <- tcrossprod(Rv)
  
  P0 <- tryCatch(solve(diag(r^2) - kronecker(Tm, Tm), as.vector(Q)), error = function(e) NULL)
  if (is.null(P0) || any(!is.finite(P0))) return(NULL)
  
  P <- matrix(P0, r, r)
  a <- numeric(r)
  v <- numeric(n)
  Fv <- numeric(n)
  
  for (t in 1:n) {
    v[t] <- z[t] - a[1]
    Fv[t] <- P[1, 1]
    if (!is.finite(Fv[t]) || Fv[t] <= 1e-12) return(NULL)
    
    K <- as.vector(Tm %*% P[, 1]) / Fv[t]
    a <- as.vector(Tm %*% a) + K * v[t]
    P <- Tm %*% P %*% t(Tm) + Q - Fv[t] * tcrossprod(K)
  }
  list(v = v, Fv = Fv)
}

# Hessiana numerica por diferencias finitas para errores estandar
matriz_hessiana <- function(fn, par, eps = 1e-4) {
  k <- length(par)
  H <- matrix(0, k, k)
  f0 <- fn(par)
  
  for (i in 1:k) {
    ei <- numeric(k); ei[i] <- eps
    H[i, i] <- (fn(par + ei) - 2 * f0 + fn(par - ei)) / eps^2
    if (i < k) {
      for (j in (i + 1):k) {
        ej <- numeric(k); ej[j] <- eps
        val <- (fn(par + ei + ej) - fn(par + ei - ej) - fn(par - ei + ej) + fn(par - ei - ej)) / (4 * eps^2)
        H[i, j] <- val
        H[j, i] <- val
      }
    }
  }
  H
}

# ACF muestral y PACF con Durbin-Levinson
correlaciones_muestrales <- function(x, rezagos = 15) {
  m <- length(x)
  K <- min(rezagos, m - 2)
  xc <- x - mean(x)
  g0 <- sum(xc^2) / m
  
  acf_v <- numeric(K)
  for (k in 1:K) {
    acf_v[k] <- sum(xc[(k + 1):m] * xc[1:(m - k)]) / (m * g0)
  }
  
  pacf_v <- numeric(K)
  Phi <- matrix(0, K, K)
  Phi[1, 1] <- acf_v[1]
  pacf_v[1] <- acf_v[1]
  
  if (K >= 2) {
    for (k in 2:K) {
      num <- acf_v[k] - sum(Phi[k - 1, 1:(k - 1)] * acf_v[(k - 1):1])
      den <- 1 - sum(Phi[k - 1, 1:(k - 1)] * acf_v[1:(k - 1)])
      Phi[k, k] <- num / den
      pacf_v[k] <- Phi[k, k]
      for (j in 1:(k - 1)) {
        Phi[k, j] <- Phi[k - 1, j] - Phi[k, k] * Phi[k - 1, k - j]
      }
    }
  }
  list(acf = acf_v, pacf = pacf_v, banda = 1.96 / sqrt(m), K = K)
}




# 2. Identificar diferenciación (d) y patrones de ACF/PACF
res_id <- identificar_modelo(trans_medallo, rezagos = 24)
res_id$tabla_df
res_id$tabla_corr

# 3. Seleccionar candidatos automáticamente según la ACF y PACF
res_comp <- seleccionar_candidatos(
  y = trans_medallo,
  d = res_id$d,
  graficar = TRUE
)
res_comp$tabla

