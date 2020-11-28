#Exportacion de archivos a excel- LA TABLA PROFESIONAL

write.csv(qdivision_profesionales_dep, file = "porcentaje_adolescentes.csv")
write.csv(qdivision_profesionales_dep, file = "porcentaje_profesionales.csv")

#Envio de correos 


library(rJava)
library(mailR)

send.mail(from = "farmaciacercana2020@gmail.com",
          to = c("christian_joseph7@hotmail.com"), # Colocar correos
          subject = "Test de envío email con R",
          body = "Esta es una prueba para envíos de correo con R",
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "farmaciacercana2020@gmail.com",
                      passwd = "universidad2020", ssl = TRUE),  # Cambiar contraseña
          authenticate = TRUE,
          send = TRUE)