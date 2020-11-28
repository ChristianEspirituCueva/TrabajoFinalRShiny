########4.Transformacion y consultas exploratorias##########




#1. Seleccionar elementos de tabla departamentos
qdepartamentos<-dbGetQuery(conexion,"select * from departamentos")


#2. Seleccionar elementos de la columna fecha de la tabla fechas
qfechas_fecha<-dbGetQuery(conexion,"SELECT fecha FROM partosadolescentes.fechas")


#3.	Consultar la division o descartar datos de la tabla profesionales que tengan datos en el Departamento de Amazonas(id=1)
qdivision_profesionales_dep<-dbGetQuery(conexion,"SELECT * from partosadolescentes.porcentaje_profesionales pp
                                                    Where NOT EXISTS(
                                                    select * 
                                                    from partosadolescentes.departamentos as d
                                                    where d.nombre = 'Amazonas'
                                                    and pp.departamento_id=d.id)")
View(qdivision_profesionales_dep)
#4.	Consultar la division o descartar datos de la tabla profesionales que tengan datos el 2012(id=1)
qdivision_profesionales_fecha<-dbGetQuery(conexion,"SELECT * from partosadolescentes.porcentaje_profesionales pp
                                                    Where NOT EXISTS(
                                                    select * 
                                                    from partosadolescentes.fechas as f
                                                    where f.fecha = '2012'
                                                    and pp.fecha_id=f.id)")


#5.	Consultar el filtrado de datos en tabla procentaje_profesionales por fecha 2012   ............
qfiltrado_fecha<-dbGetQuery(conexion,"select departamento_id, porcentaje, fecha_id
                                from partosadolescentes.porcentaje_profesionales as pp 
                                where pp.fecha_id='1'")


#6 Consultar el filtrado de datos en tabla porcentaje_profesionales donde los porcentaje sean nulos
qfiltrado_porcentaje<-dbGetQuery(conexion,"select departamento_id, porcentaje 
                                 from partosadolescentes.porcentaje_profesionales as pp 
                                 where pp.porcentaje is null")



#7 consultar el Join de datos en la tabla porcentaje para saber el departamento donde se encuentra ese porcentaje
qjoin_porcentaje_dep<-dbGetQuery(conexion,"select pp.id as id_porcentaje_profesional,
                                       pp.porcentaje,
                                       pd.id as id_departamento,
                                       pd.nombre as departamento 
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.departamentos pd
                                       on pp.departamento_id = pd.id")


#8 consultar el Join de datos en la tabla porcentaje para saber la fecha(año) cuando se realizó ese porcentjae
qjoin_porcentaje_fec<-dbGetQuery(conexion,"select pp.id as id_porcentaje_profesional,
                                       pp.porcentaje,
                                       pf.id as id_fecha,
                                       pf.fecha as año 
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       on pp.fecha_id = pf.id")



#9 Seleccionamos toda la tabla de porcentaje_profesionales
df_tabla_profesionales<-dbGetQuery(conexion,"select * from partosadolescentes.porcentaje_profesionales")


#10 Seleccionamos toda la tabla de porcentaje_emb_juv
df_tabla_porcentaje_adolescentes_emb<-dbGetQuery(conexion,"select * from partosadolescentes.porcentaje_adolescentes_emb")


#11 consultar el Join de datos en la tabla porcentaje para saber el departamento donde se realizó ese porcentaje
df_prof_ados_dep<-dbGetQuery(conexion,"select 
                                       pd.nombre as Departamento,
                                       pp.id as id_porcentaje_profesional,
                                       pp.porcentaje as porcentaje_profesional,
                                       pa.id as id_porcentaje_adolescente,
                                       pa.porcentaje as porcentaje_adolescente
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.porcentaje_adolescentes_emb as pa
                                       join partosadolescentes.departamentos pd
                                       on pp.id = pa.id and pp.departamento_id=pd.id")


#12 consultar el Join de datos en la tabla porcentaje para saber el departamento por cada fecha(año) cuando se realizó ese porcentjae
df_prof_fec_dep<-dbGetQuery(conexion,"select 
                                       pd.nombre as Departamento,
                                       pf.fecha as Año,
                                       pp.porcentaje as porcentaje_profesional
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       join partosadolescentes.departamentos pd
                                       on pp.fecha_id = pf.id and pp.departamento_id=pd.id")

#13.	Consultar el filtrado de datos en tabla procentaje_profesionales por fecha 2012,amazonas y ayacucho   ............
df_prof_depar_nom_12<-dbGetQuery(conexion,"select pd.nombre as Departamento, porcentaje, fecha_id
                                from partosadolescentes.porcentaje_profesionales as pp
                                join partosadolescentes.departamentos as pd
                                on pd.id=pp.departamento_id 
                                where pp.fecha_id='1' and pd.id < 5")


#14 consultar el Join de datos en la tabla porcentaje para saber el departamento por cada fecha(año) cuando se realizó ese porcentjae
df_prof_fec_dep_4p<-dbGetQuery(conexion,"select 
                                       pd.nombre as Departamento,
                                       pf.fecha as Año,
                                       pp.porcentaje as porcentaje_profesional
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       join partosadolescentes.departamentos pd
                                       on pp.fecha_id = pf.id and pp.departamento_id=pd.id
                                       where pd.id < 5")


#15 Consultar el filtrado de datos en tabla porcentaje_profesionales donde los porcentaje sean nulos
qfiltrado_porcentaje<-dbGetQuery(conexion,"select departamento_id, porcentaje 
                                 from partosadolescentes.porcentaje_profesionales as pp 
                                 where pp.porcentaje is null")
#encontrar cantidad de nulos por años
#16.Consultar el filtrado de datos en tabla procentaje_adloscentes por respectiva fecha y departamento
df_emb_fecha_m4_dep_m3<-dbGetQuery(conexion,"select pd.nombre as Departamento, porcentaje, pf.fecha
                                       from partosadolescentes.porcentaje_adolescentes_emb as pa
                                       join partosadolescentes.departamentos as pd
                                       on pd.id=pa.departamento_id 
                                       Join partosadolescentes.fechas as pf
                                       on pf.id=pa.fecha_id
                                       where pa.fecha_id<'4' and pd.id < 3 ")

#17.
qjoin_porcentaje_emb<-dbGetQuery(conexion,"select pp.id as id_porcentaje_embarazadas,
                                       pp.porcentaje,
                                       pd.id as id_departamento,
                                       pd.nombre as departamento,
                                       pf.fecha 
                                       from partosadolescentes.porcentaje_adolescentes_emb as pp 
                                       join partosadolescentes.departamentos pd
                                       on pp.departamento_id = pd.id
                                       join partosadolescentes.fechas pf
                                       on pp.fecha_id = pf.id
                                       where pp.departamento_id=27")

#18.
paraKNN<-dbGetQuery(conexion,"select pp.id as id_porcentaje_embarazadas,
                                           pp.porcentaje as porcentaje_profes,
                                           pf.fecha as año,
                                           pa.porcentaje as porcentaje_adoles
                                           from partosadolescentes.porcentaje_profesionales as pp 
                                           join partosadolescentes.porcentaje_adolescentes_emb pa
                                           on pp.id= pa.id
                                           join partosadolescentes.fechas pf
                                           on pp.fecha_id = pf.id")


qpuebra2012<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id='1'")




qpuebra2013<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id='2'")

