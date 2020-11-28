################3. Modelado de datos estruturados##################

driver=MySQL()
host="35.172.128.83"
port=3306
user="admininfo"
password="admininfo"
dbname="partosadolescentes"

if(dbCanConnect(drv=driver,port=port,user=user,host=host,password=password,dbname=dbname))#Para ver si se puede conectar con la base de datos
{
  conexion<-dbConnect(drv=driver,port=port,user=user,host=host,
                      password=password,
                      dbname=dbname) 
}
View(conexion)
dbIsValid(conexion)
View(dbListTables(conexion))