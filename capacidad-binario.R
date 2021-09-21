gage.rr.binary=function(part,appr,patt,rev,result,datos){
# Argumentos:
  # part=identificador del objeto
  # appr=identificador del inspector
  # patt=catalogación del objeto (defectuoso/no defectuoso)
  # rev=revisión del inspector (1 y 2 por inspector)
  # result=resultado de la revisión del inspector
  # datos= data.frame con todos los datos.
  
# Repetitividad por inspector: concordancias entre las 2 mediciones de cada inspector
# Repetitividad+exactitud : concordancias entre las 2 mediciones y con el patrón
# Reproducibilidad: concordancias entre las  mediciones de los dos inspectores
# Reproducibilidad + exactitud: concordancia  entre las  mediciones de los dos inspectores y con el patrón
  
  n=dim(datos)[1]/(max(datos$appr)*max(datos$rev))
concuerda=list()
repetitividad=acierta.insp=vector()
for(i in 1:2){
  concuerda[[i]]=which(datos$result[(datos$appr==i) & (datos$rev==2)]==datos$result[(datos$appr==i) & (datos$rev==1)])
  repetitividad[i]=length(concuerda[[i]])
  #repetitividad + exactitud
  acierta.insp[i]=sum(datos$patt[concuerda[[i]]]==datos$result[concuerda[[i]]])
}

# % repetitividad
r=repetitividad/n*100  # within appraisers
re=acierta.insp/n*100  # within appraisers + exactitud
# intervalos de confianza para las proporciones
r.int=re.int=list()
r.mat=re.mat=matrix(nrow=2,ncol=2)
for(i in 1:2){
r.int[[i]]=prop.test(repetitividad[i],n)$conf.int*100
re.int[[i]]=prop.test(acierta.insp[i],n)$conf.int*100
for(j in 1:2){
r.mat[i,j]=paste(round(r.int[[i]][j],2),"%")
re.mat[i,j]=paste(round(re.int[[i]][j],2),"%")
}}

# reproducibilidad
#Reproducibilidad
reproduce.cual=which(datos$result[datos$rev==1]==datos$result[datos$rev==2])  # between appraisers
acierta=sum(datos$patt[reproduce.cual]==datos$result[reproduce.cual])  # between appraisers + exactitud

reprod=length(reproduce.cual)/(2*n)*100 # % coincidencias de 2n elementos
reprod.patt=acierta/(2*n)*100 # % coincidencias correctas de 2n elementos

reprod.ic=paste(round(prop.test(length(reproduce.cual),2*n)$conf.int*100,2),"%")
reprod.patt.ic=paste(round(prop.test(acierta,2*n)$conf.int*100,2),"%")


res.repetitividad=data.frame(inspected=n,matched.rev=repetitividad,Repetitivity=paste(r,"%"),Repet.IC95=r.mat)
dimnames(res.repetitividad)[[1]]=c("Inspector1","Inspector2")
res.repetitividad.exact=data.frame(inspected=n,
                         matched.patt=acierta.insp,Repetitivity.Exact=paste(re,"%"),Repet.Exact.IC95=re.mat)
dimnames(res.repetitividad.exact)[[1]]=c("Inspector1","Inspector2")

res.reproducibilidad=data.frame(inspected=2*n,matched.rev=length(reproduce.cual),Reproducibility=paste(reprod,"%"),
                            Reprod.IC95.l=reprod.ic[1], Reprod.IC95.u=reprod.ic[2])
dimnames(res.reproducibilidad)[[1]]=c("Results")
res.reproducibilidad.exact=data.frame(inspected=2*n,matched.patt=acierta,
                     Reproducibility.Exact=paste(reprod.patt,"%"),
                     Reprod.Exact.IC95.l=reprod.patt.ic[1],Reprod.Exact.IC95.u=reprod.patt.ic[2])
dimnames(res.reproducibilidad.exact)[[1]]=c("Results")


return(list(Repetitivity=res.repetitividad, Repetitivity.Exact=res.repetitividad.exact,
            Reproducibility=res.reproducibilidad,Reproducibility.Exact=res.reproducibilidad.exact))
}

#gage.rr.binary(part,appr,patt,rev,result,datos)

