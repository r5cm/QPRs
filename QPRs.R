# Login Salesforce --------------------------------------------------------

## Andes
library(RForcecom)
username <- "admin@andes.org"
password <- "gfadm913XQWRiDpPU6NzJC9Cmm185FF2"
session <- rforcecom.login(username, password)

## Anserma
library(RForcecom)
username <- "admin@anserma.org"
password <- "admgf2017#Nz9VeZ766qGsxVoRF1p29WYh"
session <- rforcecom.login(username, password)



# Descargar datos de Salesforce -------------------------------------------

# Objetos mAg
objetos <- rforcecom.getObjectList(session)
objetosmAg <- objetos[grep("gfmAg*|FMP*", x =  objetos$name), ]

o.farmerBL <- rforcecom.getObjectDescription(session,
                                             "gfmAg__farmerBaseline__c")
o.pmfMetas <- rforcecom.getObjectDescription(session,
                                             "FMP_Diagnostic_TargetDefinition__c")
o.pmfSeg <- rforcecom.getObjectDescription(session,
                                            "FMP_Follow_Up__c")


# OBTENER DATOS

## Productores con Línea de base
queryLB <- "SELECT Estado_de_LB__c, gfmAg__Farmer__r.Documento_identidad__c, socioEconomicSubmission__r.gfsurveys__endDate__c FROM gfmAg__farmerBaseline__c WHERE Farmer_status__c = 'Activo'"
farmersLB <- rforcecom.query(session, queryLB)

# Filtrar no consentimientos y Falta LB
indice.lb <- grep("Completa|Incompleta", farmersLB$Estado_de_LB__c)
farmersLB <- farmersLB[indice.lb, c("gfmAg__Farmer__c.Documento_identidad__c",
                                    "gfsurveys__Submission__c.gfsurveys__endDate__c")]
farmersLB <- data.frame(farmersLB,
                        tipo.visita = rep("Línea de base", nrow(farmersLB)))

## Productores con PMF-Metas
queryFmpMeta <- "SELECT Documento_de_identidad__c, Submission__c, Submission__r.gfsurveys__endDate__c FROM FMP_Diagnostic_TargetDefinition__c WHERE Farmer_status__c = 'Activo'"
fmpMeta <- rforcecom.query(session, queryFmpMeta)
fmpMeta <- fmpMeta[!is.na(fmpMeta$Submission__c), c("Documento_de_identidad__c",
                                                    "gfsurveys__Submission__c.gfsurveys__endDate__c")]
fmpMeta <- data.frame(fmpMeta,
                      tipo.visita = rep("PMF-Metas", nrow(fmpMeta)))

## Productores  con PMF-Seguimiento
queryFmpSeg <- "SELECT FMPDiagnostic__r.Documento_de_identidad__c, Submission__c, Submission__r.gfsurveys__endDate__c FROM FMP_Follow_Up__c  WHERE FMPDiagnostic__r.Farmer_status__c = 'Activo'"
fmpSeg <- rforcecom.query(session, queryFmpSeg)
fmpSeg <- fmpSeg[, c("FMP_Diagnostic_TargetDefinition__c.Documento_de_identidad__c",
                     "gfsurveys__Submission__c.gfsurveys__endDate__c")]
fmpSeg <- data.frame(fmpSeg,
                     tipo.visita = rep("FMP-Seguimiento", nrow(fmpSeg)))

## Consolidar todos
nombres.cons <- c("cc", "fecha", "tipo.visita")
names(farmersLB) <- nombres.cons
names(fmpMeta) <- nombres.cons
names(fmpSeg) <- nombres.cons

interacc <- rbind(farmersLB, fmpMeta, fmpSeg)

## Eliminar fechas de las horas
interacc$fecha <- substr(interacc$fecha, 1, 10)
interacc$fecha <- as.Date(interacc$fecha)

## Filtrar para incluir solo datos del trimestre
fecha_inf <- as.Date("2017-04-01")
fecha_sup <- as.Date("2017-06-30")

indice.q <- interacc$fecha >= fecha_inf & interacc$fecha <= fecha_sup
interaccQ <- subset(interacc, indice.q)

# #/% unique Farmers reguarly receiving information on marketgs/et --------
# 
# Personas que recibieron cualquier tipo de visita (en cada visita se
# provee información de los servicios ofrecidos por el proyecto).
# Regularly: Una en el actual y por lo menos otra en los dos 
# trimestres anteriores

# Filtrar observaciones de dos trimestres anteriores
fecha_inf_Qm2 <- as.Date("2016-10-01")
indice.Qm2 <- interacc$fecha > fecha_inf_Qm2 & interacc$fecha < fecha_inf
interaccQm2 <- subset(interacc, indice.Qm2)

# Crear lista con los que se visitaron este trimestre
cc.Qactual <- data.frame(unique(interaccQ$cc))
names(cc.Qactual)[1] <- "cc"

# Delos trimestres anteriores filtrar lo que no se visitaron en el actual
library(dplyr)
visita.Qm2Q1 <- semi_join(interaccQm2, cc.Qactual, by = "cc")
cc.Qm2Q1 <- data.frame(cc = visita.Qm2Q1$cc)

# Bind de los dos y cálculo de indicador
visitaQm2Q1 <- cbind(cc.Qactual$cc, cc.Qm2Q1$cc)
write.csv(cc.Qactual$cc, "1.csv")
write.csv(cc.Qm2Q1$cc, "2.csv")


# #/% unique Farmers receiving advisory support on related topics  --------
# at least once
# 
# Productores que recibieron cualquier tipo de visita.

i04512 <- length(unique(interaccQ$cc))

# #/% unique Farmers receiving information on markets/etc offered  --------
# at least once
# 
# Personas que recibieron cualquier tipo de visita (en cada visita se
# provee información de los servicios ofrecidos por el proyecto).

i04515 <- length(unique(interaccQ$cc))

# ??#/% of unique Farmers who regularly use advisory services/acce --------
# ess information
# 
# Personas que recibieron cualquier tipo de visita (Una en el actual
# y por lo menos otra en los dos trimestres anteriores).

visitas.x.prod <- data.frame(table(interaccQ_3$cc))
i04510 <- nrow(visitas.x.prod[visitas.x.prod$Freq >= 2, ])





