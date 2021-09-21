gage.rr.binary=function(part,appr,result,rep,patt,datos){
# Argumentos:
  # part=identificador del objeto
  # appr=identificador del observador
  # rep=réplica u ordinal de la observación (2 observaciones por observador)
  # result=catalogació del objeto por el observador (defectuoso/no defectuoso)
  # patt=catalogación (real) del objeto (defectuoso/no defectuoso)
  # datos= data.frame con todos los datos.
  
# Repetitividad por observador: concordancias entre las 2 mediciones de cada observador
# Repetitividad+exactitud : concordancias entre las 2 mediciones y con el patrón
# Reproducibilidad: concordancias entre las  mediciones de los dos inspectores
# Reproducibilidad + exactitud: concordancia  entre las  mediciones de los dos observador y con el patrón
  
n=length(unique(datos$part)) # número de elementos medidos
concuerda=list()
repetitividad=repetitividad.patt=vector()
for(i in 1:2){
  concuerda[[i]]=which(datos$result[(datos$appr==i) & (datos$rep==2)]==datos$result[(datos$appr==i) & (datos$rep==1)])
  repetitividad[i]=length(concuerda[[i]])
  #repetitividad + exactitud
  repetitividad.patt[i]=sum(datos$patt[concuerda[[i]]]==datos$result[concuerda[[i]]])
}

# % repetitividad
r=repetitividad/n*100  # within appraisers
re=repetitividad.patt/n*100  # within appraisers + exactitud
# intervalos de confianza para las proporciones
r.int=re.int=list()
r.mat=re.mat=vector()

matrix(nrow=2,ncol=2)
for(i in 1:2){
r.int[[i]]=prop.test(repetitividad[i],n)$conf.int*100
re.int[[i]]=prop.test(repetitividad.patt[i],n)$conf.int*100
r.mat[i]=paste0("(",round(r.int[[i]][1],2),"%",",",round(r.int[[i]][2],2),"%)")
re.mat[i]=paste0("(",round(re.int[[i]][1],2),"%",",",round(re.int[[i]][2],2),"%)")
}

#Reproducibilidad
reproduce=which(datos$result[datos$appr==1]==datos$result[datos$appr==2])  # between appraisers
# reproducibilidad + exactitud
reproduce.patt=sum(datos$patt[reproduce]==datos$result[reproduce])  # between appraisers + exactitud

reprod=length(reproduce)/(2*n)*100 # % coincidencias de 2n elementos
reprod.patt=reproduce.patt/(2*n)*100 # % coincidencias correctas de 2n elementos

reprod.ic=paste(round(prop.test(length(reproduce),2*n)$conf.int*100,2),"%")
reprod.patt.ic=paste(round(prop.test(reproduce.patt,2*n)$conf.int*100,2),"%")


res.repetitividad=data.frame(inspected=n,matched=repetitividad,
                             Repetitivity=paste0(r,"%"),IC95=r.mat)
dimnames(res.repetitividad)[[1]]=c("Observador1","Observador2")
res.repetitividad.exact=data.frame(inspected=n,
                         matched=repetitividad.patt,
                         Repet.Exact=paste0(re,"%"),
                         IC95=re.mat)
dimnames(res.repetitividad.exact)[[1]]=c("Observador1","Observador2")

res.reproducibilidad=data.frame(inspected=2*n,matched=length(reproduce),
                                Reproducibility=paste0(reprod,"%"),
                            IC95=paste0("(",reprod.ic[1],",",reprod.ic[2],")"))
dimnames(res.reproducibilidad)[[1]]=c("Results")
res.reproducibilidad.exact=data.frame(inspected=2*n,matched=reproduce.patt,
                     Reprod.Exact=paste0(reprod.patt,"%"),
                     IC95=paste0("(",reprod.patt.ic[1],",",reprod.patt.ic[2],")"))
dimnames(res.reproducibilidad.exact)[[1]]=c("Results")


return(list(Repetitivity=res.repetitividad, Repetitivity.Exact=res.repetitividad.exact,
            Reproducibility=res.reproducibilidad,Reproducibility.Exact=res.reproducibilidad.exact))
}

#gage.rr.binary(part,appr,result,rep,patt,datos)

