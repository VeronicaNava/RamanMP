# Identification of peak based on local maximum with user-defined threshold
# Fist column must be frequency data

peak.finder<-function(spectrum, threshold=0, m=5, max.peak=0){
  spectrum<-na.omit(spectrum)
  shape <- diff(sign(diff(spectrum[,2], na.pad = FALSE)))
  colnames(spectrum)<-c("Var1", "Var2")
  spectrum_sin<-spectrum[,2]
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(spectrum_sin), w, length(spectrum_sin))
    if(all(spectrum_sin[c(z : i, (i + 2) : w)] <= spectrum_sin[i + 1])) return(i + 1) else return(numeric(0))
  })
  peak_index <- unlist(pks)

  peak_inten<-spectrum_sin[peak_index]
  spectra_new <- subset(spectrum, spectrum[,2]>threshold)
  value_x<-spectra_new[spectra_new[,2] %in% peak_inten,]




  #Graphical parameters
  min_yaxis<- min(spectrum[,2])
  max_yaxis<-max(spectrum[,2])
  diff_yaxis<-max_yaxis-min_yaxis
  gap<-diff_yaxis/2
  nudge_value<-diff_yaxis/2
 Var1 <- Var2 <- NULL

  if(max.peak==0){
    value<-value_x[,1]
    plot_peaks<-ggplot2::ggplot(spectrum, ggplot2::aes(x=Var1, y=Var2, label=round(Var1, digits=0)))+
      ggplot2::geom_path(col="gray5")+
      ggrepel::geom_text_repel(
        data = spectrum %>% dplyr::filter(spectrum[,1]  %in% value), # Filter data first
       nudge_y       = nudge_value,
        segment.size  = 0.3,
        segment.color = "grey50",
        direction     = "both",
        angle=90)+
      ggplot2::ylim(min_yaxis,(max_yaxis+gap))+
      ggplot2::labs(x=expression(Raman~shift~(cm^-1)), y="Intensity (a.u.)")+
      ggplot2::theme_minimal()+
      ggplot2::theme(axis.title.x = ggplot2::element_text(size="16"),
            axis.title.y = ggplot2::element_text(size="16"),
            axis.text.x= ggplot2::element_text(size="14"),
            axis.text.y= ggplot2::element_blank())

    print(plot_peaks)
    colnames(value_x)<-c("Wavelength", "Intensity")
    return(value_x)
  }
  else{
    new_order<-value_x[order(value_x[,2],decreasing=TRUE),]
    new_selection<-new_order[1:max.peak,]
    value2<-new_selection[,1]

    plot_peaks2<-ggplot2::ggplot(spectrum,  ggplot2::aes(x=Var1, y=Var2, label=round(Var1, digits=0)))+
      ggplot2::geom_line(col="gray5")+
      ggrepel::geom_text_repel(
       data= spectrum %>%  dplyr::filter(spectrum[,1]  %in% value2), # Filter data first
        nudge_y       = nudge_value,
        segment.size  = 0.3,
        segment.color = "grey50",
        direction     = "both",
        angle=90)+
      ggplot2::ylim(min_yaxis,(max_yaxis+gap))+
      ggplot2::labs(x=expression(Raman~shift~(cm^-1)), y="Intensity (a.u.)")+
      ggplot2::theme_minimal()+
      ggplot2::theme(axis.title.x = ggplot2::element_text(size="16"),
            axis.title.y = ggplot2::element_text(size="16"),
            axis.text.x= ggplot2::element_text(size="14"),
            axis.text.y= ggplot2::element_blank())

    print(plot_peaks2)
    new_final<-new_selection[order(new_selection[,1],decreasing=FALSE),]
    colnames(new_final)<-c("Wavelength", "Intensity")
    return(new_final)
  }
}
