#INCLUDE "Protheus.ch"
#INCLUDE "TopConn.ch"
#INCLUDE "rwmake.ch"
#Include "ApWebSrv.ch"
#INCLUDE "ap5mail.ch"
#INCLUDE "shell.ch"
#Include "TbiConn.ch"

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	

User Function SF34R03X()
	Local aSays     	:= {}
	Local aButtons  	:= {}
	Local cCadastro  	:= OemToAnsi("Impressão Autorizacao De Repasse")
	Private cPerg		:= "SF34R03X"
	Default lSendMail  := .F.

	ajustaSx1(cPerg)
	Pergunte(cPerg,.T.)

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Monta tela principal                                                      ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	AAdd(aSays,OemToAnsi("Impressão Autorizacao De Repasse."	))
	AAdd(aSays,OemToAnsi("Imprimir conforme dados selecionados?"))
	AAdd(aButtons, { 5,.T.	,{|| Pergunte(cPerg,.T. )				}})
	AADD(aButtons, { 1,.T.	,{|o| (ExecBol(),o:oWnd:End())}})
	AADD(aButtons, { 2,.T.	,{|o| o:oWnd:End()						}})

	FormBatch( cCadastro, aSays,aButtons )
Return

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	
Static Function ExecBol
	Local aTitulos	:= {}
	Private cStatus := 1
	Private lEnd	:= .F.

	MsgRun( "Titulos De Repasses", "Selecionando registros para Impressão dos Repasses", { || CallRegs(@aTitulos)} )
	If Len(aTitulos) > 0 
		U_fSF34R03X(@aTitulos)
	EndIf	
Return

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	

Static Function CallRegs(aTitulos)

	Local cQry	:= " SELECT SE2.R_E_C_N_O_ SE2REG, A2_NOME, SE2.E2_NUM "
	cQry	    += " FROM "+RetSqlName("SE2")+" SE2 "
	cQry	    += " INNER JOIN "+RetSqlName("SA2")+" SA2 ON A2_COD = E2_FORNECE AND A2_LOJA = E2_LOJA AND SA2.D_E_L_E_T_ = '' "
	cQry		+= " WHERE SE2.E2_PREFIXO = '"+mv_par01+"'"
	cQry		+= " AND   SE2.E2_TIPO    = '"+mv_par02+"'"
	cQry		+= " AND SE2.E2_EMISSAO BETWEEN '"+DTOS(mv_par03)+"' AND '"+DTOS(mv_par04)+"'"
	cQry		+= " AND SE2.E2_FILIAL BETWEEN '"+mv_par05+"' AND '"+mv_par06+"'"
	cQry		+= " AND SE2.D_E_L_E_T_ = '' "

	If Select("TRBSA2") > 0
		dbSelectArea("TRBSA2")
		DbCloseArea()
	EndIf

	cQry := ChangeQuery(cQry) 
	dbUseArea(.T.,'TOPCONN',TcGenQry(,,cQry),"TRBSA2",.F.,.T. )
	
	dbSelectArea("TRBSA2")
	DbGotop()

	While TRBSA2->(!Eof())
		dbSelectArea("SE2")
		DbGoto(TRBSA2->SE2REG)

	
		aAdd(aTitulos, {	.F.,;						// 1=Mark
		SE2->E2_PREFIXO,;		// 2=Prefixo do Título
		SE2->E2_NUM,;			// 3=Número do Título
		SE2->E2_PARCELA,;		// 4=Parcela do Título
		SE2->E2_TIPO,;			// 5=Tipo do Título
		SE2->E2_NATUREZ,;		// 6=Natureza do Título
		SE2->E2_FORNECE,;		// 7=Cliente do título
		SE2->E2_LOJA,;			// 8=Loja do Cliente
		TRBSA2->A2_NOME,;		// 9=Nome do Cliente
		SE2->E2_EMISSAO,;		//10=Data de Emissão do Título
		SE2->E2_VENCTO,;		//11=Data de Vencimento do Título
		SE2->E2_VENCREA,;		//12=Data de Vencimento Real do Título
		SE2->E2_VALOR,;			//13=Valor do Título
		SE2->E2_HIST,;			//14=Histótico do Título
		TRBSA2->SE2REG,;		//15=Número do registro no arquivo
		'' ;		//16=Nosso Número
		})

		dbSelectArea("TRBSA2")
		TRBSA2->(dbSkip())
	End

	If Len(aTitulos) == 0
		aAdd(aTitulos, {.F.,"","","","","","","","","","","",0,"",0,""})
	EndIf

Return(Nil)

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	
USer Function fSF34R03X(aTitulos)
	Local oDlg
	Local oList1
	Local oMark
	Local bCancel   := {|| RTRBSA2(oDlg,@lRetorno,aTitulos,.F.) }
	Local bOk       := {|| RFINR01B(oDlg,@lRetorno,aTitulos,.F.,.T.) }
	Local bSendmail := {|| RFINR01B(oDlg,@lRetorno,aTitulos,.T.,.F.) }
	Local aAreaAtu	:= GetArea()
	Local aLabel	:= {" ","Prefixo","Número","Parcela","Tipo","Natureza","Cliente","Loja","Nome Fornecedor","Emissão","Vencimento","Venc.Real","Valor","Histórico","Nosso Número"}
	Local aBotao    := {}
	Local lRetorno	:= .T.
	Local lMark		:= .F.
	Local cList1
	Local cTitulo   := "Titulos de Repasses"
	Local lOrdem		:= .F.

	Private oOk			:= LoadBitMap(GetResources(),"LBOK")
	Private oNo			:= LoadBitMap(GetResources(),"LBNO")

	AADD(aBotao, {"S4WB011N" 	, { || U_fBOLVETIT("SE1",SE1->(aTitulos[oList1:nAt,15]),2)}, "[F12] - Visualiza Título", "Título" })
	SetKey(VK_F12,	{|| U_fBOLVETIT("SE1",SE1->(aTitulos[oLis1:nAt,15]),2)})

	DEFINE MSDIALOG oDlg TITLE cTitulo From 000,000 To 420,940 OF oMainWnd PIXEL
	@ 015,005 CHECKBOX oMark VAR lMark PROMPT "Marca Todos" FONT oDlg:oFont PIXEL SIZE 80,09 OF oDlg;
		ON CLICK (aEval(aTitulos, {|x,y| aTitulos[y,1] := lMark}), oList1:Refresh() )
	@ 030,003 LISTBOX oList1 VAR cList1 Fields HEADER ;
		aLabel[1],;
		aLabel[2],;
		aLabel[3],;
		aLabel[4],;
		aLabel[5],;
		aLabel[6],;
		aLabel[7],;
		aLabel[8],;
		aLabel[9],;
		aLabel[10],;
		aLabel[11],;
		aLabel[12],;
		aLabel[13],;
		aLabel[14],;
		aLabel[15] ;
		SIZE 463,175  NOSCROLL PIXEL
	oList1:SetArray(aTitulos)
	oList1:bLine	:= {|| {	If(aTitulos[oList1:nAt,1], oOk, oNo),;
		aTitulos[oList1:nAt,2],;
		aTitulos[oList1:nAt,3],;
		aTitulos[oList1:nAt,4],;
		aTitulos[oList1:nAt,5],;
		aTitulos[oList1:nAt,6],;
		aTitulos[oList1:nAt,7],;
		aTitulos[oList1:nAt,8],;
		aTitulos[oList1:nAt,9],;
		aTitulos[oList1:nAt,10],;
		aTitulos[oList1:nAt,11],;
		aTitulos[oList1:nAt,12],;
		Transform(aTitulos[oList1:nAt,13], "@E 999,999,999.99"),;
		aTitulos[oList1:nAt,14],;
		aTitulos[oList1:nAt,16] ;
		}}

	oList1:blDblClick 	:= {|| aTitulos[oList1:nAt,1] := !aTitulos[oList1:nAt,1], oList1:Refresh() }
	oList1:cToolTip		:= "1a. Opção > Duplo click para marcar/desmarcar o títulos." + Chr(13)+Chr(10) + Chr(13)+Chr(10) + "2a. Opção > Click na Coluna e Depois Sobre o Cabeçalho para Ordenar."
	oList1:bHeaderClick := {|| oList1:Refresh() , MaOrdCab(@oList1,@lOrdem), oList1:Refresh() }
	oList1:Refresh()

	@ 15,080 BMPBUTTON TYPE 01 ACTION RFINR01B(oDlg,@lRetorno,aTitulos,.F.,.T.)
	@ 15,110 BMPBUTTON TYPE 02 ACTION RTRBSA2(oDlg,@lRetorno,aTitulos,.F.)

	oEmail := TButton():New(15,150,"Enviar Titulos via E-mail" ,oDlg,{|| RFINR01B(oDlg,@lRetorno,aTitulos,.T.,.F.) }, 60 , 11 ,,,.F.,.T.,.F.,,.F.,,,.F. )

	ACTIVATE MSDIALOG oDlg CENTERED //ON INIT EnchoiceBar(oDlg,bOk,bcancel,,aBotao)

	SetKey(VK_F12,	Nil)

Return(lRetorno)


/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	
Static Function RTRBSA2(oDlg,lRetorno, aTitulos)

	lRetorno := .F.

	oDlg:End()

Return(lRetorno)

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	
Static Function RFINR01B(oDlg,lRetorno, aTitulos,lSendMail,lView)

	Local nLoop		:= 0
	Local nContador	:= 0
	Local lFim		:= .F.

	lRetorno := .T.

	For nLoop := 1 To Len(aTitulos)
		If aTitulos[nLoop,1]
			nContador++
		EndIf
	Next

	If nContador > 0
		MsAguarde({|lFim| ImpBol(aTitulos,lView,lSendMail)},"Processamento","Aguarde a finalização do processamento...")

//		ImpBol(aTitulos,lView,lSendMail)
	Else
		lRetorno := .F.
	EndIf

	oDlg:End()

Return(lRetorno)


/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	
User Function fBOLVETIT(cAlias, nRecAlias, nOpcEsc)

	Local aAreaAtu	:= GetArea()
	Local aAreaAux	:= (cAlias)->(GetArea())

	If !Empty(nRecAlias)
		dbSelectArea(cAlias)
		dbSetOrder(1)
		dbGoTo(nRecAlias)
		AxVisual(cAlias,nRecAlias,nOpcEsc)
	EndIf

	RestArea(aAreaAux)
	RestArea(aAreaAtu)

Return(Nil)

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/			
Static Function ImpBol(aTitulos,lView,lSendMail)

	If lSendMail
		nOpx := Aviso("Atenção","Deseja Imprimir os Titulos antes de enviar ?" , {"Sim","Não"} )
		If nOpx==1
			lView := .T.
		Endif
	Endif
	If lSendMail
		fImpBol(aTitulos,.F.,lSendMail) // Manda Enviar Email em unico Arquvio
	EndIf
	If lView
		fImpBol(aTitulos,lView,.F.) // Manda Imprimnir Boleto em unico Arquvio
	EndIf
Return(Nil)


// Funccao para imprimir boleto e enviar email, quando imprimir boleto imprimir tudo em unico arquivo, quando email em varios arquivos
Static Function fImpBol(aTitulos,lView,lSendMail)
	Local oPrint
	Local aDadTit	:= {}
	Local aSacado	:= {}
	Local aBolTxt	:= {"","","","","",""}
	Local nSaldo	:= 0
	Local nX		:= 0
	Local nLoop		:= 0
	Local cNNum		:= ""
	Local cNNdig	:= ""
	Local cFAXATU   :=""
	Local nTipo     := 2
	Local nBols     := 1
	Local cEmail    := ""
	Local cNomCli   := ""
	Local aCB_RN_NN	:= {}
	Local nVlJuro	:= GetMv("MV_TXPER") //SuperGetMV("MV_TXPER",.T.,1)
	
	Private cNomeArq  := ""
	Private cCaminho  := Alltrim(GetMv("VS_FLDBOL",,"\pdf\"))
	Private cDirTmp   := Alltrim(GetTempPath())

	If lView
		cNomeArq := "pdf_"+StrTran(Time(),":","")+StrZero(nLoop,2)
		If File(cCaminho+cNomeArq+".pdf")
			FErase( cCaminho+cNomeArq+".pdf" )
		Endif
		If File(cDirTmp+cNomeArq+".pdf")
			FErase( cCaminho+cNomeArq+".pdf" )
		Endif
		oPrint := FwMSPrinter():New( cNomeArq, 6 , .T. , cDirTmp , .T., , , , , .F., ,.F. )
		oPrint:cPathPDF := cDirTmp
		oPrint:SetPortrait()					// Modo retrato
		oPrint:SetPaperSize(9)					// Papel A4
		oPrint:lInJob:= .T.
	EndIf
	For nLoop := 1 To Len(aTitulos)
		If aTitulos[nLoop,1]
			dbSelectArea("SE2")
			dbGoTo(aTitulos[nLoop,15])
			If lSendMail
				cNomeArq := "pdf_"+StrTran(Time(),":","")+StrZero(nLoop,2)
				If File(cCaminho+cNomeArq+".pdf")
					FErase( cCaminho+cNomeArq+".pdf" )
				Endif
				If File(cDirTmp+cNomeArq+".pdf")
					FErase( cCaminho+cNomeArq+".pdf" )
				Endif
				oPrint := FwMSPrinter():New( cNomeArq, 6 , .T. , cDirTmp , .T., , , , , .F., ,.F. )
				oPrint:cPathPDF := cDirTmp
				oPrint:SetPortrait()					// Modo retrato
				oPrint:SetPaperSize(9)					// Papel A4
				oPrint:lInJob:= .T.
			EndIf	
			Impress(oPrint,aTitulos[nLoop,15],aTitulos[nLoop,9])
			If lSendMail
				oPrint:SetViewPDF(.F.)
				oPrint:Print()

				cSubject := "Titulos De Repasses"

				//cEmail   := cEmail

				cHtml := " Prezado "+cNomCli+", "+Chr(13)+chr(10)
				cHtml += " Estamos enviando em anexo o Titulos de Repasse para pagamento."+Chr(13)+chr(10)
				cHtml += " "+Chr(13)+chr(10)
				cHtml += " Atenciosamente, "+Chr(13)+chr(10)

				ATEnvMail( cHtml , cEmail , cSubject , { cNomeArq +".pdf" } )
			Endif
		EndIf	
	Next nLoop
	If lView
		oPrint:SetViewPDF(.T.)
		oPrint:Print()
	EndIf
Return()

/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	
Static Function Impress(oPrint, nRegSE2, sA2_NOME)
	LOCAL oFont8
	LOCAL oFont8n
	LOCAL oFont11c
	LOCAL oFont10
	LOCAL oFont14
	LOCAL oFont16n
	LOCAL oFont15
	LOCAL oFont14n
	LOCAL oFont24
	Local nRow		:= 0
	Local cBmp		:= ""
	Local cStartPath	:= GetSrvProfString("Startpath","")
	Local cBmp			:= cStartPath +  "FIEMT.BMP" 	// Empresa+Filial
	local nBols := 1
	local nX := 1
	local nY := 1


    //dbSelectArea("SM0")
	//dbSetOrder(1)   && forca o indice na ordem certa
	//nRegistro := Recno()
	//dbSeek(SUBS(cNumEmp,1,2)+SC8->C8_FILENT)


	oFont8		:= TFont():New("Arial",		9,08,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont8n		:= TFont():New("Arial",		9,08,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont11c	:= TFont():New("Courier New",	9,11,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont20c	:= TFont():New("Courier New",	9,19,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont10		:= TFont():New("Arial",		9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont10c	:= TFont():New("Courier New",		9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont12		:= TFont():New("Arial",		9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont12n	:= TFont():New("Arial",		9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont14		:= TFont():New("Arial",		9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont13		:= TFont():New("Arial",		9,13,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont20		:= TFont():New("Arial",		9,20,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont21		:= TFont():New("Arial",		9,21,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont16n	:= TFont():New("Arial",		9,16,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont15		:= TFont():New("Arial",		9,15,.T.,.T.,5,.T.,5,.T.,.F.)
	oFont15n	:= TFont():New("Arial",		9,15,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont14n	:= TFont():New("Arial",		9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	oFont24		:= TFont():New("Arial",		9,24,.T.,.T.,5,.T.,5,.T.,.F.)

	dbSelectArea("SE2")
	dbGoTo(nRegSE2)

	aFilRet := fDadosFil(SE2->E2_FILIAL)

//	aadd(aRet,aSM0Data2[1][2]) 	//Codigo Empresa
//	aadd(aRet,sFilial)			//Filial
//	aadd(aRet,aSM0Data2[3][2])	// Nome Filial

	For nY := 1 to 2
		If nY == 1
			oPrint:StartPage()   // Inicia uma nova página
			nRow := 0
		Else
			nRow := 1480
		Endif
		oPrint:Box  (nRow+0100,100,nRow+1430,2300)													// Quadro

		oPrint:SayBitMap(nRow+120,140,cBmp,280,150)
		oPrint:Say  (nRow+180,0700,"AUTORIZAÇÃO DE REPASSE",						oFont20)		// Texto Fixo
		oPrint:Say  (nRow+180,1800,SE2->E2_TIPO + ' - ' + SE2->E2_NUM,					oFont20)	// Texto Fixo
		oPrint:Say  (nRow+240,480,Alltrim(aFilRet[2]) + ' - ' + Alltrim(aFilRet[3]),	oFont13)	// Texto Fixo
		oPrint:Say  (nRow+240,1900,Alltrim(aFilRet[2]),									oFont15)	// Texto Fixo
		oPrint:Line (nRow+0100,1650,nRow+300,1650)													// horizontal

		oPrint:Line (nRow+0300,100,nRow+300,2300)													// horizontal

		oPrint:Say  (nRow+340,1100,"Contas a Pagar",								oFont15)	// Texto Fixo


		oPrint:Line (nRow+0380,100,nRow+380,2300)													// horizontal

		oPrint:Line (nRow+0380,350,nRow+580,350)													// horizontal
		oPrint:Say  (nRow+0400,120 ,"Documento",									oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,150 ,SE2->E2_NUM,									oFont15)	// Texto Fixo

		oPrint:Say  (nRow+0500,120 ,"Fornecedor",									oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0540,150 ,SE2->E2_FORNECE,								oFont15)	// Texto Fixo

		oPrint:Line (nRow+0380,520,nRow+480,520)													// horizontal
		oPrint:Say  (nRow+0400,370 ,"Tipo",											oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,390 ,SE2->E2_TIPO,									oFont15)	// Texto Fixo

		oPrint:Say  (nRow+0500,370 ,"Nome",											oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0540,390 ,sA2_NOME,										oFont12n)	// Texto Fixo


		oPrint:Line (nRow+0380,660,nRow+480,660)													// horizontal
		oPrint:Say  (nRow+0400,540 ,"Prefixo",										oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,560 ,SE2->E2_PREFIXO,								oFont15)	// Texto Fixo

		oPrint:Line (nRow+0380,1000,nRow+480,1000)													// horizontal
		oPrint:Say  (nRow+0400,680 ,"Natureza",										oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,700 ,SE2->E2_NATUREZ,								oFont15)	// Texto Fixo

		oPrint:Line (nRow+0380,1320,nRow+580,1320)													// horizontal
		oPrint:Say  (nRow+0400,1020 ,"Emissao",										oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,1040 ,dtoc(SE2->E2_EMISSAO),							oFont15)	// Texto Fixo

		oPrint:Line (nRow+0380,1640,nRow+480,1640)													// horizontal
		oPrint:Say  (nRow+0400,1340 ,"Vencimento",									oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,1360 ,dtoc(SE2->E2_VENCREA),							oFont15)	// Texto Fixo
		oPrint:Say  (nRow+0500,1340 ,"Historico",									oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0540,1360 ,SE2->E2_HIST,									oFont15)	// Texto Fixo

		oPrint:Line (nRow+0380,2000,nRow+480,2000)													// horizontal
		oPrint:Say  (nRow+0400,1660 ,"Valor",										oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,1680 ,Transf(SE2->E2_VALOR,"@E 999,999,999.99"),		oFont15)	// Texto Fixo

		oPrint:Say  (nRow+0400,2020 ,"Saldo",										oFont12)	// Texto Fixo
		oPrint:Say  (nRow+0440,2040 ,Transf(SE2->E2_SALDO,"@E 999,999,999.99"),		oFont15)	// Texto Fixo

		oPrint:Line (nRow+0480,100,nRow+480,2300)													// horizontal
		oPrint:Line (nRow+0580,100,nRow+580,2300)													// horizontal

	//	oPrint:Say  (nRow+0600,120 ,"Observacao",										oFont12)	// Texto Fixo
	//	oPrint:Say  (nRow+0640,150 ,'SE2->E2_OBS',		oFont15)	// Texto Fixo

		oPrint:Line (nRow+0750,100,nRow+750,2300)													// horizontal
		cText := "Autorizo o pagamento do titulo, referente ao saldo do encontro de "
		cText += "contas numero "+Alltrim(SE2->E2_NUM)+" realizado em "+dToc(SE2->E2_EMISSAO)+" entre as empresas " + Alltrim(aFilRet[3]) + " e "
		cText += Alltrim(sA2_NOME) + " no valor de R$ "+ Alltrim(Transf(SE2->E2_VALOR,"@E 999,999,999.99")) + " ("+Extenso(SE2->E2_VALOR)+")"
		aRet  := fCentTxt(cText)
		For nX := 1 To Len(aRet)
			oPrint:Say  (nRow+730+(nX*80),150 ,aRet[nX],		oFont20c)	// Texto Fixo
		Next nX	
		//oPrint:Say  (nRow+940,150 ,cTextA2,		oFont20c)	// Texto Fixo
		//oPrint:Say  (nRow+1020,150 ,cTextA3,		oFont20c)	// Texto Fixo
		cDataExt  := ''
        cDataExt += cValToChar(Day(date()))
        cDataExt += " de "
        cDataExt += MesExtenso(date())
        cDataExt += " de "
        cDataExt += cValToChar(Year(date()))

		oPrint:Say  (nRow+1280,150 ,"Cuiaba, "+cDataExt ,		oFont16n)	// Texto Fixo

		oPrint:Line (nRow+1280,1150,nRow+1280,2150)													// horizontal
		oPrint:Say  (nRow+1320,1600,"Assinatura",		oFont15)	// Texto Fixo

		oPrint:Line (nRow+1370,100,nRow+1370,2300)													// horizontal

		oPrint:Say  (nRow+1410,1900,dtoc(ddatabase)+ ' - ' + time(),		oFont10c)	// Texto Fixo

		If  nY == 1

			oPrint:Line (nRow+1500,100,nRow+1500,2300)													// horizontal

		EndIf
		oPrint:EndPage()
	Next nY	
Return



/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/	

static function ajustaSx1(cPerg)
	//Aqui utilizo a função putSx1, ela cria a pergunta na tabela de perguntas
	oPerg := TPergunta():New(cPerg)

	oPerg:AddGet  ("Prefixo ?"			,            "C", TAMSX3("E2_PREFIXO") [1], 0, Nil, "", "")
	oPerg:AddGet  ("Tipo ?"				,            "C", TAMSX3("E2_TIPO") [1], 0, Nil, "", "")
	oPerg:AddGet  ("Emissao Inicial ?"	,            "D", 08, 0, Nil, "", "")
	oPerg:AddGet  ("Emissao Final ?"	,            "D", 08, 0, Nil, "", "")
	oPerg:AddGet  ("Filial Inicial ?"	,            "C", 8, 0, Nil, "XM0", "")
	oPerg:AddGet  ("Filial Final ?"		,            "C", 8, 0, Nil, "XM0", "")
	oPerg:Update()
	oPerg:Execute(.F.)	
return


/*/{Protheus.doc}''
''
@author Julio Nobre
@since ''
@version ''
@type function
@see ''
@obs ''
@param ''
@return ''
/*/

Static Function ATEnvMail(cHtml,cEmail,cSubject,aAnexos)

	Local lResult    	:= .T.
	Local cError     	:= ""
	Local cTo      		:= ""
	Local lAuth    		:= GetMv("MV_RELAUTH")
	Local nTimeout		:= 240
	Local cMailConta	:= Alltrim(GetMv("MV_RELACNT"))
	Local cMailServer	:= Alltrim(GetMv("MV_RELSERV"))
	Local cMailSenha 	:= Alltrim(GetMv("MV_RELPSW"))
	Local cFileSrv      := "\boletos"
	Local cAnexos		:= ""
	Local nI:=1

	Default aAnexos     := {}
//	cEmail := 'marcio.ssilva@totvs.com.br'

	cTo	:= RTrim(cEmail)

	If !Empty(cMailServer) .And. !Empty(cMailConta) .And. !Empty(cMailSenha)

		CONNECT SMTP SERVER cMailServer ACCOUNT cMailSenha PASSWORD cMailSenha RESULT lResult

		If lResult
			If lAuth
				lResult := MailAuth(cMailConta,cMailSenha)
			EndIf
			If lResult
				If Len(aAnexos)>0
					cAnexos:=''
					For nI:=1 to Len(aAnexos)
						CpyT2S( cDirTmp+aAnexos[nI] , cCaminho , .F. )
						cAnexos += If(!Empty(cAnexos),",","")+cCaminho+aAnexos[nI]
					Next
				Endif

				SEND MAIL  				;
					FROM       cMailConta	;
					TO		   cTo			;
					SUBJECT	   cSubject		;
					BODY	   cHtml		;
					ATTACHMENT cAnexos      ;
					RESULT	   lResult
				If !lResult
					GET MAIL ERROR cError
					cError := cError+".Falha no Envio do e-mail."
					If (!IsBlind())
						ALERT(cError)
					Else
						Conout(cError)
					Endif
				EndIf
			Else
				//Erro na autenticacao da conta
				GET MAIL ERROR cError
				cError := cError+".Falha no Envio do e-mail."
				If (!IsBlind())
					ALERT(cError)
				Else
					Conout(cError)
				Endif
			Endif
		Else
			GET MAIL ERROR cError
			cError := cError+".Falha no Envio do e-mail."
			If (!IsBlind())
				ALERT(cError)
			Else
				Conout(cError)
			Endif
		Endif
	Endif

Return()

*******************************
Static Function fCentTxt(cText)
***************
Local aRet  := {}
Local nCort := 69
Local nX    := 0
Local nX1   := 0
Local tWlaco := .T.

nCol :=  nCort
While tWlaco
	If Subs(Alltrim(cText),nCol,1) == ' ' .Or. Len(Alltrim(cText)) < nCort
		aadd(aRet,Left(Alltrim(cText),nCol))
		If Len(Alltrim(cText)) > nCort
			cText := Right(Alltrim(cText), Len(Alltrim(cText)) - nCol)
			nCol :=  nCort
		Else
			tWlaco := .F.
		EndIF		
	Else 
		nCol -= 1	
	EndIf
EndDo
For nX := 1 to (Len(aRet) - 1)
	nTotEsp := nCort - Len(Alltrim(aRet[nX]))
	sLinha := Alltrim(aRet[nX])
	While nTotEsp > 0 
		   For nX1 := 1 to Len(sLinha) 
		   		If Subs(sLinha,nX1,1) == " " .And. nTotEsp > 0 
					sLinha := Left(sLinha,nX1) + " " +Right(sLinha,len(sLInha) - nX1) 
					nTotEsp -= 1
					nX1 += 1
				EndIf
		   Next nX1
	EndDo
	aRet[nX] := sLinha
Next nX 
Return(aRet)

********************************
Static Function fDadosFil(sFilial)
***************
Local aFieldSM0 := { ;
    "M0_CODIGO",;    //Posição [1]
    "M0_CODFIL",;    //Posição [2]
    "M0_NOMECOM",;   //Posição [3]
    "M0_CGC",;       //Posição [4]
    "M0_INSCM",;     //Posição [5]
    "M0_CIDENT",;    //Posição [6]
    "M0_ESTENT",;    //Posição [7]
    "M0_ENDENT",;    //Posição [8]
    "M0_BAIRENT",;   //Posição [9]
    "M0_CEPENT",;    //Posição [10]
    "M0_COMPENT",;   //Posição [11]
    "M0_TEL";        //Posição [12]
}
Local aSM0Data2 := {}
Local aRet		:= {}
 
//... Aqui você pode montar sua query, ou alguma outra tratativa na sua lógica, no meu caso, montei uma query e busquei o F2_FILIAL
 
aSM0Data2 := FWSM0Util():GetSM0Data(, sFilial, aFieldSM0)
If Len(aSM0Data2) > 0
	aadd(aRet,aSM0Data2[1][2]) 	//Codigo Empresa
	aadd(aRet,sFilial)			//Filial
	aadd(aRet,aSM0Data2[3][2])	// Nome Filial
EndIf
Return(aRet)
