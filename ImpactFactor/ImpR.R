ImpactFactors <-
  function(IFrange = c(2, 4), Year = 2014, log = F){
    warn_before <- options()$warn
    options(warn = -1)
    dt <-
      "~/GitHub/BeeLabR/Entertainment/ImpactFactor/General/" %>%
      paste0(., Year, ".txt") %>%
      read.csv(skip = 1, sep = ";") %>%
      {
        select(., 3:4, 8:9) %>%
        setnames(c("ImpactFactor", "FiveYearImpactFactor", "EigenFactor", "ArticleInfluenceFactor")) %>%
        mutate(Journal = rownames(.)) # rownamesでテキストデータの行名に入っている雑誌名をとってきてる
      } %>%
      data.table %>%
      filter(between(ImpactFactor, min(IFrange), max(IFrange)))
    
    fig <-
      dt %>%
      ggplot(aes(x = ImpactFactor, y = EigenFactor, col = Journal)) +
      geom_point(size = 5, shape = 23, fill = "grey") +
      geom_text(aes(label = Journal), size = 3) +
#      scale_color_manual(values = hue_pal(l = 80)(row_number(dt))) +
#      scale_fill_manual(values = hue_pal(l = 30)(row_number(dt))) +
      guides(size = F, color = F, fill = F) +
      coord_cartesian(xlim = c(min(IFrange) * 0.97 , max(IFrange) * 1.03))

    if(log){
      print(fig + scale_x_log10() + scale_y_log10())
    } else {
      print(fig)
    }
    
    options(warn = warn_before)
  }