perc <- function(fcountry,decades){

  Greenhouse_Gas_Emissions <- Greenhouse_Gas_Emissions %>%
    mutate(decade = paste0(year  %/% 10 * 10, "-", (year + 10) %/% 10 * 10 ))

  mean_decad <- Greenhouse_Gas_Emissions %>%
    group_by(country, decade) %>%
    select_if(is.numeric) %>%
    select(-year) %>%
    summarise_all(mean, na.rm = TRUE)

  mean_decad <-  ddply(mean_decad, .(country),
                       function (d) {
                         d$co2_growth <- c(NA, (tail(d$co2, -1) / head(d$co2, -1) - 1)*100)
                         d
                       }
  )
  mean_decad <-  ddply(mean_decad, .(country),
                       function (d) {
                         d$methane_growth <- c(NA, (tail(d$methane, -1) / head(d$methane, -1) - 1)*100)
                         d
                       }
  )
  mean_decad <-  ddply(mean_decad, .(country),
                       function (d) {
                         d$nitrous_oxide_growth <- c(NA, (tail(d$nitrous_oxide, -1) / head(d$nitrous_oxide, -1) - 1)*100)
                         d
                       }
  )

  for (i in fcountry) {
    mean_decad_sel <- mean_decad %>%
      select(country, co2_growth, methane_growth, nitrous_oxide_growth, decade) %>%
      filter(decade %in%  decades & country %in% i)

    barplot(t(as.matrix(mean_decad_sel[, 2:4])),
            beside = TRUE,
            names.arg = mean_decad_sel$decade,
            legend.text = c("Co2 % Growth", "Methane % Growth", "Nitrous Oxide % Growth"),
            main = c("Percentage of increment respect last decade for", i),
            args.legend = list(x = "top", inset = c(- 0.15, 0)),
            ylab = "% of Increase",
            xlab = "Decade",
            cex.names=0.8,
            cex.axis=0.8,
            ylim = c(min(0,min(mean_decad_sel[, 2:4], na.rm=T )), max(mean_decad_sel[, 2:4], na.rm=T)*1.6)
    )
  }
}

regression <- function(fcountry){

  Greenhouse_Gas_Emissions <- Greenhouse_Gas_Emissions %>%
    mutate(decade = paste0(year  %/% 10 * 10, "-", (year + 10) %/% 10 * 10 ))

  mean_decad <- Greenhouse_Gas_Emissions %>%
    group_by(country, decade) %>%
    select_if(is.numeric) %>%
    select(-year) %>%
    summarise_all(mean, na.rm = TRUE)


  for (i in fcountry){
    reg_decad_sel <- mean_decad %>%
      select(country, co2, methane, nitrous_oxide, decade) %>%
      filter(country %in% i, decade %in%  c("1990-2000", "2000-2010", "2010-2020"))
    reg_decad_sel$decade <- c(1,2,3)

    plot(reg_decad_sel$decade, reg_decad_sel$co2, main = c("Evolution of Co2 emissions by decade for ", i),
         xlab = "Decade", ylab = "Mean expected value for CO2 emissions",
         pch = 19, frame = FALSE)
    abline(lm(co2~decade, data = reg_decad_sel), col = "blue")

    plot(reg_decad_sel$decade, reg_decad_sel$methane, main = c("Evolution of Methane emissions by decade for ", i),
         xlab = "Decade", ylab = "Mean expected value for Methane emissions",
         pch = 19, frame = FALSE)
    abline(lm(methane~decade, data = reg_decad_sel), col = "red")

    plot(reg_decad_sel$decade, reg_decad_sel$nitrous_oxide, main = c("Evolution of Nitrous Oxide emissions by decade for ", i),
         xlab = "Decade", ylab = "Mean expected value for Nitrous Oxide emissions",
         pch = 19, frame = FALSE)
    abline(lm(nitrous_oxide~decade, data = reg_decad_sel), col = "green")
  }
}

statistics <- function(fcountry){

  Greenhouse_Gas_Emissions <- Greenhouse_Gas_Emissions %>%
    mutate(decade = paste0(year  %/% 10 * 10, "-", (year + 10) %/% 10 * 10 ))

  mean_decad <- Greenhouse_Gas_Emissions %>%
    group_by(country, decade) %>%
    select_if(is.numeric) %>%
    select(-year) %>%
    summarise_all(mean, na.rm = TRUE)

  Data  <-c("Will Increment their CO2?", "Expected increase (million tonnes)","Expected % of increase", "Sd of the regression model", "R-squared value")
  df <-data.frame(row.names = Data)
  options(digits=2)

  for (i in fcountry){
    reg_decad_sel <- mean_decad %>%
      select(country, co2, methane, nitrous_oxide, decade) %>%
      filter(country %in% i, decade %in%  c("1990-2000", "2000-2010", "2010-2020"))
    reg_decad_sel$decade <- c(1,2,3)

    lmc02 <- lm(co2~decade, data = reg_decad_sel)
    lmc02 <- summary(lmc02)
    if (lmc02$coefficients[2]>0){
      incr <-1
    } else {
      incr <-0
    }

    i <- c(incr,lmc02$coefficients[2], ((lmc02$coefficients[2]/(lmc02$coefficients[1]+3*lmc02$coefficients[2]))*100) , lmc02$sigma ,lmc02$r.squared)

    df <-data.frame(df, i)

  }
  names(df)<-fcountry

  df
}
