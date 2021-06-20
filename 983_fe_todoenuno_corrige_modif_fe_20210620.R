#Feature Engineering con variables extendidas y manuales
#creo nuevas variables dentro del mismo mes


#este script esta pensado para correr en la nube

#limpio la memoria
rm( list=ls() )
gc()

library( "data.table" )
library( "Rcpp" )

t0      <-  Sys.time()

setwd( "~/buckets/b1/" )
setwd( "C:/Users/pbeltramino/Desktop/MGCD/Laboratorio de Implementacion I")
#lectura rapida del dataset  usando fread  de la libreria  data.table
install.packages("R.utils")
dataset <- fread( "./datasetsOri/paquete_premium.txt.gz")

dataset <-dataset[foto_mes>201912 & foto_mes<=202003]

columnas_originales <-  copy(colnames( dataset ))

#acomodo los errores del dataset
dataset[ foto_mes==201701,  ccajas_consultas   := NA ]
dataset[ foto_mes==201702,  ccajas_consultas   := NA ]

dataset[ foto_mes==201801,  internet   := NA ]
dataset[ foto_mes==201801,  thomebanking   := NA ]
dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
dataset[ foto_mes==201801,  tcallcenter   := NA ]
dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
dataset[ foto_mes==201801,  ccajas_otras   := NA ]

dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos  := NA ]

dataset[ foto_mes==201905,  mrentabilidad     := NA ]
dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
dataset[ foto_mes==201905,  mcomisiones       := NA ]
dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
dataset[ foto_mes==201905,  mactivos_margen  := NA ]
dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

dataset[ foto_mes==201910,  mpasivos_margen  := NA ]
dataset[ foto_mes==201910,  mactivos_margen  := NA ]
dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
dataset[ foto_mes==201910,  mcomisiones       := NA ]
dataset[ foto_mes==201910,  mrentabilidad     := NA ]
dataset[ foto_mes==201910,  mrentabilidad_annual     := NA ]
dataset[ foto_mes==201910,  chomebanking_transacciones   := NA ]
dataset[ foto_mes==201910,  ctarjeta_visa_descuentos   := NA ]
dataset[ foto_mes==201910,  ctarjeta_master_descuentos   := NA ]
dataset[ foto_mes==201910,  mtarjeta_visa_descuentos   := NA ]
dataset[ foto_mes==201910,  mtarjeta_master_descuentos    := NA ]
dataset[ foto_mes==201910,  ccajeros_propios_descuentos   := NA ]
dataset[ foto_mes==201910,  mcajeros_propios_descuentos   := NA ]

dataset[ foto_mes==202001,  cliente_vip   := NA ]




#INICIO de la seccion donde se deben hacer cambios con variables nuevas
#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
dataset[ , mv_status02       := Master_status +  Visa_status ]
dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 9, Master_status) , ifelse( is.na(Visa_status), 9, Visa_status) ) ]
dataset[ , mv_status04       := ifelse( is.na(Master_status), 9, Master_status)  +  ifelse( is.na(Visa_status), 9, Visa_status)  ]
dataset[ , mv_status05       := ifelse( is.na(Master_status), 9, Master_status)  +  10*ifelse( is.na(Visa_status), 9, Visa_status)  ]

dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                        ifelse( is.na(Master_status), 9, Master_status), 
                                        Visa_status)  ]

dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                        ifelse( is.na(Visa_status), 9, Visa_status), 
                                        Master_status)  ]


#combino MasterCard y Visa
dataset[ , mv_delinquency          := pmax( Master_delinquency, Visa_delinquency, na.rm = TRUE) ]
dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]


# INICIO seccion variables flow y stock

#Primero preparamos los montos $

#dataset[ , mganancia_banco := rowSums( cbind( mcomisiones,  mactivos_margen, mpasivos_margen) , na.rm=TRUE ) ]
dataset[ , liquidez_cc := rowSums( cbind( mcuenta_corriente_adicional,  mcuenta_corriente) , na.rm=TRUE ) ]
dataset[ , liquidez_ca := rowSums( cbind( mcaja_ahorro,  mcaja_ahorro_adicional) , na.rm=TRUE ) ]
dataset[ , liquidez := rowSums( cbind( liquidez_cc,  liquidez_ca) , na.rm=TRUE ) ]
dataset[ , gastos_sin_prestamos := rowSums( cbind( mautoservicio,  mtarjeta_visa_consumo, mtarjeta_master_consumo) , na.rm=TRUE ) ]
dataset[ , sum_prestamos := rowSums( cbind( mprestamos_personales,  mprestamos_prendarios, mprestamos_hipotecarios) , na.rm=TRUE ) ]
dataset[ , sum_plazosfijos := rowSums( cbind( mplazo_fijo_dolares,  mplazo_fijo_pesos) , na.rm=TRUE ) ]
dataset[ , sum_inversiones := rowSums( cbind( minversion1_pesos,  minversion1_dolares, minversion2) , na.rm=TRUE ) ]
dataset[ , tot_inversiones := rowSums( cbind( sum_plazosfijos,  sum_inversiones) , na.rm=TRUE ) ]
dataset[ , acreditaciones := rowSums( cbind( mpayroll,  mpayroll2) , na.rm=TRUE ) ]
#dataset[ , debitos_aut := rowSums( cbind( mcuenta_debitos_automaticos,  mttarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
#dataset[ , debitos_no_aut := rowSums( cbind( debitos_aut,  mpagomiscuentas) , na.rm=TRUE ) ]
#dataset[ , tot_debitos := rowSums( cbind( debitos_aut,  debitos_no_aut) , na.rm=TRUE ) ]
#dataset[ , sum_descuentos := rowSums( cbind( mcajeros_propios_descuentos,  mtarjeta_visa_descuentos, mtarjeta_master_descuentos) , na.rm=TRUE ) ]
#  dataset[ , sum_comisiones_pagadas := rowSums( cbind( mcomisiones_mantenimiento,  mcomisiones_otras) , na.rm=TRUE ) ]
#dataset[ , var_mens_dol := rowSums( cbind( mforex_buy,  -1*mforex_sell) , na.rm=TRUE ) ]
#dataset[ , sum_montos_oper_dol := rowSums( cbind( mforex_buy,  mforex_sell) , na.rm=TRUE ) ]
# dataset[ , transferencias_netas := rowSums( cbind( mtransferencias_recibidas,  -1*mtransferencias_emitidas) , na.rm=TRUE ) ]
#dataset[ , sum_transferencias := rowSums( cbind(  mtransferencias_recibidas,  mtransferencias_emitidas) , na.rm=TRUE ) ]
dataset[ , cheques_emitidos := rowSums( cbind( mcheques_emitidos,  mcheques_emitidos_rechazados) , na.rm=TRUE ) ]
dataset[ , cheques_recibidos := rowSums( cbind( mcheques_depositados,  mcheques_depositados_rechazados) , na.rm=TRUE ) ]
dataset[ , operaciones_netas_cheques := rowSums( cbind( cheques_recibidos,  -1*cheques_emitidos) , na.rm=TRUE ) ]
dataset[ , operaciones_totales_cheques := rowSums( cbind( cheques_recibidos,  cheques_emitidos) , na.rm=TRUE ) ]
dataset[ , uso_cajero := rowSums( cbind(  matm,  matm_other) , na.rm=TRUE ) ]

#Luego preparamos las cantidades

#  dataset[ , ctipo_paquete := rowSums( cbind( tpaquete2,  tpaquete3) , na.rm=TRUE ) ]
dataset[ , coperaciones := rowSums( cbind( ctarjeta_debito_transacciones,  ctarjeta_visa_transacciones, ctarjeta_master_transacciones, cextraccion_autoservicio, ccallcenter_transacciones, chomebanking_transacciones, ccajas_transacciones) , na.rm=TRUE ) ]
dataset[ , cprestamos := rowSums( cbind( cprestamos_personales,  cprestamos_prendarios, cprestamos_hipotecarios) , na.rm=TRUE ) ]
#  dataset[ , cinversiones := rowSums( cbind( cplazo_fijo, cinversion1, cinversion2) , na.rm=TRUE ) ]
#  dataset[ , cseguros := rowSums( cbind( cseguro_vida,  cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
#  dataset[ , cpayroll := rowSums( cbind( cpayroll_trx,  cpayroll2_trx) , na.rm=TRUE ) ]
#  dataset[ , cdebitos_aut := rowSums( cbind( ccuenta_debitos_automaticos,  ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
#  dataset[ , cdebitos_no_aut := rowSums( cbind( cpagodeservicios,  cpagomiscuentas) , na.rm=TRUE ) ]
#  dataset[ , ctotal_debitos := rowSums( cbind( cdebitos_aut,  cdebitos_no_aut) , na.rm=TRUE ) ]
#  dataset[ , cdescuentos := rowSums( cbind( ccajeros_propios_descuentos,  ctarjeta_visa_descuentos, ctarjeta_master_descuentos) , na.rm=TRUE ) ]
# dataset[ , ccomisiones_pagadas := rowSums( cbind( ccomisiones_mantenimiento,  ccomisiones_otras) , na.rm=TRUE ) ]
# dataset[ , ctransferencias := rowSums( cbind( ctransferencias_recibidas,  ctransferencias_emitidas) , na.rm=TRUE ) ]
#  dataset[ , ccheques_operados := rowSums( cbind( ccheques_depositados,  ccheques_emitidos, ccheques_depositados_rechazados,ccheques_emitidos_rechazados) , na.rm=TRUE ) ]
#  dataset[ , c_tot_cheques_emitidos := rowSums( cbind(ccheques_emitidos,  ccheques_emitidos_rechazados) , na.rm=TRUE ) ]
#  dataset[ , c_tot_cheques_recibidos:= rowSums( cbind( ccheques_depositados,  ccheques_depositados_rechazados) , na.rm=TRUE ) ]
#  dataset[ , cuso_cajas := rowSums( cbind( ccajas_transacciones,  ccajas_consultas, ccajas_depositos, ccajas_extracciones, ccajas_otras) , na.rm=TRUE ) ]
#  dataset[ , cuso_atm := rowSums( cbind( catm_trx,  catm_trx_other) , na.rm=TRUE ) ]
dataset[ , c_tar_cred := rowSums( cbind( ctarjeta_visa,  ctarjeta_master) , na.rm=TRUE ) ]
#  dataset[ , c_inv_sinpf := rowSums( cbind( cinversion1,  cinversion2) , na.rm=TRUE ) ]

# Relaciones de monto sobre cantidad

dataset[ , prom_cc:= ifelse(ccuenta_corriente == 0, 100000, liquidez_cc / ccuenta_corriente) ]
dataset[ , prom_ca:= ifelse(ccaja_ahorro == 0, 100000, liquidez_ca / ccaja_ahorro) ]
#  dataset[ , prom_gast_tar:= ifelse(c_tar_cred == 0, 100000, gastos_sin_prestamos / c_tar_cred) ]
#  dataset[ , prom_prestamos:= ifelse(cprestamos == 0, 100000, sum_prestamos / cprestamos) ]
#  dataset[ , prom_plazosfijos:= ifelse(cplazo_fijo == 0, 100000, sum_plazosfijos / cplazo_fijo) ]
#  dataset[ , prom_inversiones:= ifelse(c_inv_sinpf == 0, 100000, sum_inversiones / c_inv_sinpf) ]
#  dataset[ , prom_inversiones_total:= ifelse(cinversiones == 0, 100000, tot_inversiones / cinversiones) ]
#  dataset[ , prom_debito_auto:= ifelse(cdebitos_aut == 0, 100000, debitos_aut / cdebitos_aut) ]
#  dataset[ , prom_debito_no_auto:= ifelse(cdebitos_no_aut == 0, 100000, debitos_no_aut / cdebitos_no_aut) ]
#  dataset[ , prom_debito_tot:= ifelse(ctotal_debitos == 0, 100000, tot_debitos / ctotal_debitos) ]
#  dataset[ , prom_descuentos:= ifelse(cdescuentos == 0, 100000, sum_descuentos / cdescuentos) ]
#  dataset[ , prom_var_dol:= ifelse(cforex == 0, 100000, var_mens_dol / cforex) ]
#  dataset[ , prom_mon_dol:= ifelse(cforex == 0, 100000, sum_montos_oper_dol / cforex) ]
#  dataset[ , prom_che_reci:= ifelse(c_tot_cheques_recibidos == 0, 100000, cheques_recibidos / c_tot_cheques_recibidos) ]
#  dataset[ , prom_che_emi:= ifelse(c_tot_cheques_emitidos == 0, 100000, cheques_emitidos / c_tot_cheques_emitidos) ]
#  dataset[ , prom_ope_netas_cheques:= ifelse(ccheques_operados == 0, 100000, operaciones_netas_cheques / ccheques_operados) ]
#  dataset[ , prom_ope_tot_cheques:= ifelse(ccheques_operados == 0, 100000, operaciones_totales_cheques / ccheques_operados) ]
#  dataset[ , prom_uso_ATM:= ifelse(cuso_atm == 0, 100000, uso_cajero / cuso_atm) ]

## Promedio rempazo /0 por Na en vez de 100000

#  dataset[ , prom_cc_02:= ifelse(ccuenta_corriente == 0, NA, liquidez_cc / ccuenta_corriente) ]
dataset[ , prom_ca_02:= ifelse(ccaja_ahorro == 0, NA, liquidez_ca / ccaja_ahorro) ]
dataset[ , prom_gast_tar_02:= ifelse(c_tar_cred == 0, NA, gastos_sin_prestamos / c_tar_cred) ]
dataset[ , prom_prestamos_02:= ifelse(cprestamos == 0, NA, sum_prestamos / cprestamos) ]
#  dataset[ , prom_plazosfijos_02:= ifelse(cplazo_fijo == 0, NA, sum_plazosfijos / cplazo_fijo) ]
#  dataset[ , prom_inversiones_02:= ifelse(c_inv_sinpf == 0, NA, sum_inversiones / c_inv_sinpf) ]
#  dataset[ , prom_inversiones_total_02:= ifelse(cinversiones == 0, NA, tot_inversiones / cinversiones) ]
#  dataset[ , prom_debito_auto_02:= ifelse(cdebitos_aut == 0, NA, debitos_aut / cdebitos_aut) ]
#  dataset[ , prom_debito_no_auto_02:= ifelse(cdebitos_no_aut == 0, NA, debitos_no_aut / cdebitos_no_aut) ]
#  dataset[ , prom_debito_tot_02:= ifelse(ctotal_debitos == 0, NA, tot_debitos / ctotal_debitos) ]
#  dataset[ , prom_descuentos_02:= ifelse(cdescuentos == 0, NA, sum_descuentos / cdescuentos) ]
#  dataset[ , prom_var_dol_02:= ifelse(cforex == 0, NA, var_mens_dol / cforex) ]
#  dataset[ , prom_mon_dol_02:= ifelse(cforex == 0, NA, sum_montos_oper_dol / cforex) ]
#  dataset[ , prom_che_reci_02:= ifelse(c_tot_cheques_recibidos == 0, NA, cheques_recibidos / c_tot_cheques_recibidos) ]
#  dataset[ , prom_che_emi_02:= ifelse(c_tot_cheques_emitidos == 0, NA, cheques_emitidos / c_tot_cheques_emitidos) ]
#  dataset[ , prom_ope_netas_cheques_02:= ifelse(ccheques_operados == 0, NA, operaciones_netas_cheques / ccheques_operados) ]
#  dataset[ , prom_ope_tot_cheques_02:= ifelse(ccheques_operados == 0, NA, operaciones_totales_cheques / ccheques_operados) ]
#  dataset[ , prom_uso_ATM_02:= ifelse(cuso_atm == 0, NA, uso_cajero / cuso_atm) ]


#Por ultimo las relaciones

# dataset[ , descubierto_liquidez:= ifelse(mcuentas_saldo == 0, 100000, mdescubierto_preacordado / mcuentas_saldo) ]
dataset[ , descubierto_liquidez_02:= ifelse(mcuentas_saldo == 0, NA, mdescubierto_preacordado / mcuentas_saldo) ]
#Proprociones $/$
# dataset[ , liquidez_inversiones:= ifelse(tot_inversiones == 0, 100000, liquidez / tot_inversiones) ]
# dataset[ , liquidez_inversiones_02:= ifelse(tot_inversiones == 0, NA, liquidez / tot_inversiones) ]
# dataset[ , gastos_inversiones:= ifelse(tot_inversiones == 0, 100000, gastos_sin_prestamos / tot_inversiones) ]
# dataset[ , gastos_inversiones_02:= ifelse(tot_inversiones == 0, NA, gastos_sin_prestamos / tot_inversiones) ]

# dataset[ , gastos_ingresos:= ifelse(acreditaciones == 0, 100000, gastos_sin_prestamos / acreditaciones) ]
dataset[ , gastos_ingresos_02:= ifelse(acreditaciones == 0, NA, gastos_sin_prestamos / acreditaciones) ]

# dataset[ , prestamos_ingresos:= ifelse(acreditaciones == 0, 100000, sum_prestamos / acreditaciones) ]
# dataset[ , prestamos_ingresos_02:= ifelse(acreditaciones == 0, NA, sum_prestamos / acreditaciones) ]
# dataset[ , prestamos_inversiones:= ifelse(tot_inversiones == 0, 100000, sum_prestamos / tot_inversiones) ]
# dataset[ , prestamos_inversiones_02:= ifelse(tot_inversiones == 0, NA, sum_prestamos / tot_inversiones) ]

dataset[ , limite_ingresos:= ifelse(acreditaciones == 0, 100000, mv_mlimitecompra / acreditaciones) ]
dataset[ , limite_ingresos_02:= ifelse(acreditaciones == 0, NA, mv_mlimitecompra / acreditaciones) ]

# dataset[ , descubierto_ingresos:= ifelse(acreditaciones == 0, 100000, mdescubierto_preacordado / acreditaciones) ]
dataset[ , descubierto_ingresos_02:= ifelse(acreditaciones == 0, NA, mdescubierto_preacordado / acreditaciones) ]
dataset[ , descubierto_cc:= ifelse(liquidez_cc == 0, 100000, mdescubierto_preacordado / liquidez_cc) ]
dataset[ , descubierto_cc_02:= ifelse(liquidez_cc == 0, NA, mdescubierto_preacordado / liquidez_cc) ]



dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]

#FIN de la seccion donde se deben hacer cambios con variables nuevas

columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

#grabo con nombre extendido
fwrite( dataset,
        file="./datasets/paquete_premium_ext_corregido.txt.gz",
        sep= "\t" )

#------------------------------------------------------------------------------
#Aqui comienza la creación de variables historicas

#ordeno por  numero_de_cliente y foto_mes
setorder( dataset,  numero_de_cliente, foto_mes )

#Esta es la cantidad de meses que utilizo para la historia
ventana_regresion  <- 6

last <- nrow( dataset )
kcampo_id_idx  <-  match( "numero_de_cliente", names(dataset) )
#----------


#creo el vector_desde que indica cada ventana
#de esta forma se acelera el procesamiento ya que lo hago una sola vez

vector_ids   <- dataset[[  kcampo_id_idx  ]]

vector_desde <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
vector_desde[ 1:ventana_regresion ]  <-  1

for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }


columnas_no_procesar  <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )

#agrego al dataset las TENDENCIAS
columnas_originales_a_procesar  <- setdiff( columnas_originales,  columnas_no_procesar  )  

for(  campo  in  columnas_originales_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
   dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
   dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
   dataset[ , paste( campo, "__lag" , sep="" ):= nueva_col[ (4*last +1):(5*last) ]  ]
   
   #Por ahora, no agrego el promedio
   #dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}


#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#grabo el archivo completo
#al  agregarle el .gz  al final del nombre del archivo,  data.table sabe que debe comprimir el archivo
fwrite( dataset, 
        file= "./datasets/paquete_premium_hist_corregido.txt.gz", 
        sep="\t" )

gc()
#ahora agrego tendencias TAMBIEN  para las variables nuevas

columnas_extendidas_a_procesar  <- setdiff( columnas_extendidas,  columnas_no_procesar  )  

for(  campo  in  columnas_extendidas_a_procesar )
{
   campo_idx     <-   match( campo,  names(dataset) )
   col_original  <-   dataset[[  campo_idx  ]]

   nueva_col     <- fhistC( col_original, vector_desde ) 

   #agrego las nuevas columnas al dataset
   dataset[ , paste( campo, "__tend", sep="" ):= nueva_col[ (0*last +1):(1*last) ]  ]
   dataset[ , paste( campo, "__min" , sep="" ):= nueva_col[ (1*last +1):(2*last) ]  ]
   dataset[ , paste( campo, "__max" , sep="" ):= nueva_col[ (2*last +1):(3*last) ]  ]
   dataset[ , paste( campo, "__lag" , sep="" ):= nueva_col[ (4*last +1):(5*last) ]  ]
   
   #Por ahora, no agrego el promedio
   #dataset[ , paste( campo, "__avg" , sep="" ):= nueva_col[ (3*last +1):(4*last) ]  ]
}


#dejo la clase como ultimo campo
nuevo_orden <-  c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
setcolorder( dataset, nuevo_orden )


#grabo el archivo completo
#al  agregarle el .gz  al final del nombre del archivo,  data.table sabe que debe comprimir el archivo
fwrite( dataset, 
        file= "./datasets/paquete_premium_exthist_corregido.txt.gz", 
        sep="\t" )



t1      <-  Sys.time()
tiempo  <-  as.numeric(  t1 - t0, units = "secs")

cat( "El Feature Engineering ha corrido en :", tiempo, "  segundos.\n" )


#limpio la memoria
rm( list=ls() )
gc()

quit( save="no")