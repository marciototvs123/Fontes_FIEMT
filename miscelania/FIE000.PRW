#Include 'Protheus.ch'
#Include 'Protheus.ch'
#INCLUDE 'TOPCONN.CH'

user function FIE000()
    Local cModulo 	:= 'SIGAFAT' //Defino em qual módulo irei abrir a minha função
    MsApp():New(cModulo) //
    oApp:cInternet := NIL     
    oApp:CreateEnv()
    PtSetTheme("OCEAN")
    oApp:cStartProg    	:= 'u_SF34R03X'//'u_AGRA02'//'U_AGR002'
    oApp:lMessageBar	:= .T. 
    oApp:cModDesc		:= cModulo
    __lInternet 		:= .T.
    lMsFinalAuto 		:= .F.
    oApp:lMessageBar	:= .T. 
    oApp:Activate()
Return
