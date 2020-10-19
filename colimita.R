library(dplyr)
library(ggplot2)

setwd("C:/Users/marco.esteban.DOMINIOIIEG/Documents/Colimita/rds")

archivos <- list.files()

# temp <- list()
# t <- Sys.time()
# for(i in 1:length(archivos)-1){
#   temp[[i]] <- read.csv(paste0("C:/Users/marco.esteban.DOMINIOIIEG/Documents/Colimita/", archivos[i]),
#                    sep = "|",
#                    stringsAsFactors = F) %>%
#     mutate(fecha = substr(archivos[i], 5, stop = 14))
#   print(paste0(i, " de ", length(archivos), ", tiempo: ", Sys.time()-t))
# }
# 
# for(i in 1:(length(archivos)-1)){
#   readr::write_rds(x = temp[[i]],
#                    path = paste0("C:/Users/marco.esteban.DOMINIOIIEG/Documents/Colimita/", 
#                           substr(archivos[i], 5, stop = 14), ".rds"))
#   print(paste0(i, " de ", length(archivos)-1, ", tiempo: ", Sys.time()-t))
# }

bases <- list()

t <- Sys.time()
for(i in 1:length(archivos)){
  bases[[i]] <- readr::read_rds(paste0("C:/Users/marco.esteban.DOMINIOIIEG/Documents/Colimita/rds/", archivos[i]))
  print(paste0(i, " de ", length(archivos), ", tiempo: ", Sys.time()-t))
}

colima <- list()

t <- Sys.time()
for(i in 1:length(archivos)){
  colima[[i]] <- bases[[i]] %>%
    filter(cve_entidad == 6, cve_municipio == "A45")
  print(paste0(i, " de ", length(archivos), ", tiempo: ", Sys.time()-t))
}

colimita <- bind_rows(colima) %>%
  group_by(fecha) %>%
  summarise(empleos = sum(ta))

ggplot(colimita) +
  geom_point(aes(factor(fecha), empleos), size = 4, color = "darkblue", alpha = .6) +
  geom_line(aes(x = c(1:18), y = empleos))+
  scale_x_discrete(labels = paste0(lubridate::month(colimita$fecha, label = T, abbr = F), " ",
                                   lubridate::year(colimita$fecha))) + 
  labs(x = "", y = "Empleados asegurados ante el imss")+
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 13, color = 'black', face = 'bold'),
        panel.grid.major.y  = element_line(colour = "grey", size = .3, 
                                           linetype = 'dashed'),
        panel.grid.minor.y  = element_line(colour = "grey", size = .3, 
                                           linetype = 'dashed'),
        panel.grid.major.x = element_line(colour = "grey", size = .3, 
                                          linetype = 'dashed'),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.key = element_rect(fill = 'white', color = 'white'),
        legend.background = element_rect(fill = 'white'),
        legend.title = element_text(size = 15, 
                                    color = '#232323',
                                    face = 'bold',
                                    hjust = .5),
        legend.text = element_text(size = 11, 
                                   color = '#232323'),
        plot.background = element_rect(fill = "white"),
        axis.line = element_line(colour = '#232323'), 
        axis.ticks = element_line(colour = '#232323')
  )

mov <- read.csv("C:/Users/marco.esteban.DOMINIOIIEG/Documents/Colimita/movilidad.csv")

cambio <- cbind(data.frame(fecha = colimita$fecha[13:18],
                     Empleos= colimita$empleos[13:18]/colimita$empleos[13]-1
                     ), mov[,-1]/100) %>%
  tidyr::gather(concepto, cambio, Empleos:Hogares) %>%
  mutate(grupo = ifelse(concepto == "Empleos", "Empleos", "Movilidad"))

ggplot(cambio) +
  geom_point(aes(factor(fecha), cambio, color = concepto), size = 6, alpha = .8) +
  geom_line(aes(x = factor(fecha), y = cambio, color = concepto, group = concepto),
            size = 3, alpha = 0.6)+
  geom_label(aes(x, y, label = lab),
             data = data.frame(x = 3.2, y = -0.005, grupo = "Empleos",
                               lab = "Periodo de mayor caída en empleos (-1,765)"))+
  geom_label(aes(x, y, label = lab),
             data = data.frame(x = 5.3, y = -0.005, grupo = "Empleos",
                               lab = "Recuperación de empleos (+262)"))+
  geom_label(aes(x, y, label = lab),
             data = data.frame(x = 3.2, y = 0.01, grupo = "Movilidad",
                               lab = "Periodo de mayor disminución en movilidad"))+
  geom_label(aes(x, y, label = lab),
             data = data.frame(x = 4.9, y = 0.01, grupo = "Movilidad",
                               lab = "Aumento en movilidad"))+
  geom_text(aes(x, y, label = lab), size = 5,
            data = data.frame(x = 1:6,
                              y = -.002+c(0, 0.0015624419, 0.0201257394, 0.0114951081, -0.0127041405,-0.0078308099),
                              grupo = "Empleos",
                              lab = c("53,762", "53,846", "54,844", "54,380", "53,079", "53,341")))+
  geom_text(aes(x, y, label = lab), size = 5,
            data = data.frame(x = 1:6,
                              y = -.002+c(0, -.08, -.24, -.57, -.55, -.43),
                              grupo = "Movilidad",
                              lab = c("100%", "92.2%", "75.5%", "43.1%", "45.1%", "56.6%")))+
  scale_x_discrete(labels = paste0(lubridate::month(cambio$fecha, label = T, abbr = F), " ",
                                   lubridate::year(cambio$fecha))) + 
  facet_grid(grupo~., scales = "free")+
  labs(x = "", y = "Cambio",
       color = "", title = "Cambio en movilidad (Google) y empleos (IMSS) con respecto a enero de 2020")+
  theme(axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 13, color = 'black', face = 'bold'),
        panel.grid.major.y  = element_line(colour = "grey", size = .3, 
                                           linetype = 'dashed'),
        panel.grid.minor.y  = element_line(colour = "grey", size = .3, 
                                           linetype = 'dashed'),
        panel.grid.major.x = element_line(colour = "grey", size = .3, 
                                          linetype = 'dashed'),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.key = element_rect(fill = 'white', color = 'white'),
        legend.background = element_rect(fill = 'white'),
        legend.title = element_text(size = 15, 
                                    color = '#232323',
                                    face = 'bold',
                                    hjust = .5),
        legend.text = element_text(size = 11, 
                                   color = '#232323'),
        plot.background = element_rect(fill = "white"),
        axis.line = element_line(colour = '#232323'), 
        axis.ticks = element_line(colour = '#232323'),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text.y = element_text(
          size = 17, color = "black"),
        strip.background = element_rect(
          color="black", fill="#bdcdd6", size=1.5, linetype="solid"
        ),
        panel.border = element_rect(color = "black", fill = NA, size = .7)
  )

ggsave("C:/Users/marco.esteban.DOMINIOIIEG/Documents/Colimita/g.png", height = 7, width = 12)
