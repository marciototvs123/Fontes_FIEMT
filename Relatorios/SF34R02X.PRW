#Include 'Protheus.ch'
#Include 'Protheus.ch'
#INCLUDE 'TOPCONN.CH'


User Function SF34R02X()
	Local oReport := nil
	Local cPerg:= Padr("SF34R02X",10)
	
	AjustaSX1(cPerg)	
	Pergunte(cPerg,.F.)	          
		
	oReport := RptDef(cPerg)
	oReport:PrintDialog()
Return

Static Function RptDef(cNome)
	Local oReport := Nil
	Local oSection1:= Nil
	
	oReport := TReport():New(cNome,"RELATORIO DE DESPESA POR CENTRO DE CUSTO",cNome,{|oReport| ReportPrint(oReport)},"Descrição do Relatório")
	oReport:cFontBody := 'Courier New'
	oReport:nFontBody := 8
	oReport:SetPortrait()    
	oReport:SetTotalInLine(.F.)
	oReport:SetLandScape()

	oSection1:= TRSection():New(oReport, "CT2", {"CT1","CT2","CT2TRB"}, , .F., .T.)
	TRCell():New(oSection1,"CCT2_FILIAL"		,/*Tabela*/	,"EMPRESA"		     	,PesqPict("CT2","CT2_FILIAL"),TamSx3("CT2_FILIAL")[1],/*lPixel*/,{|| cCT2_FILIAL})
	TRCell():New(oSection1,"DCT2_DATA"			,/*Tabela*/	,RetTitle("CT2_DATA")	,PesqPict("CT2","CT2_DATA")	 ,20					,/*lPixel*/,{|| dCT2_DATA })
	TRCell():New(oSection1,"CCT2_HIST"			,/*Tabela*/	,RetTitle("CT2_HIST")	,PesqPict("CT2","CT2_HIST")	 ,60					,/*lPixel*/,{|| cCT2_HIST })
	TRCell():New(oSection1,"CCT1_CONTA"			,/*Tabela*/	,"DESPESA"				,PesqPict("CT1","CT1_CONTA") ,TamSx3("CT1_CONTA")[1],/*lPixel*/,{|| cCT1_CONTA })
	TRCell():New(oSection1,"CCT1_DESC01"		,/*Tabela*/	,"DESCRICAO DA CONTA"	,PesqPict("CT1","CT1_DESC01"),60					,/*lPixel*/,{|| cCT1_DESC01 })
	TRCell():New(oSection1,"NCT2_VALOR"			,/*Tabela*/	,RetTitle("CT2_VALOR")	,PesqPict("CT2","CT2_VALOR") ,TamSx3("CT2_VALOR")[1],/*lPixel*/,{|| cCT2_VALOR })

	aAdd(oSection1:Cell("CCT2_FILIAL"):aFormatCond, {"CT2TRB->tipo < 'd'" ,,CLR_RED})
	aAdd(oSection1:Cell("DCT2_DATA"):aFormatCond, {"CT2TRB->tipo < 'd'" ,,CLR_RED})
	aAdd(oSection1:Cell("CCT2_HIST"):aFormatCond, {"CT2TRB->tipo < 'd'" ,,CLR_RED})
	aAdd(oSection1:Cell("CCT1_CONTA"):aFormatCond, {"CT2TRB->tipo < 'd'" ,,CLR_RED})
	aAdd(oSection1:Cell("CCT1_DESC01"):aFormatCond, {"CT2TRB->tipo < 'd'" ,,CLR_RED})
	aAdd(oSection1:Cell("NCT2_VALOR"):aFormatCond, {"CT2TRB->tipo < 'd'" ,,CLR_RED})

Return(oReport)

Static Function ReportPrint(oReport)
	Local oSection1 := oReport:Section(1)
	Local cQuery    := ""		
	Local lPrim 	:= .F.	      
	Local dDataDe	:= mv_par01
	Local dDataAte	:= mv_par02
	Local sCtaDe	:= mv_par03
	Local sctaAte	:= mv_par04
	Local sCCDe		:= mv_par05
	Local sCCAte	:= mv_par06
	Local sItemDe	:= mv_par07
	Local sItemAte	:= mv_par08
	Local sLotInib	:= mv_par09
	Local nFilial	:= mv_par10
	Local aSelFil   := {}
	local sRetFilial := ''

	If nFilial == 1
		If Empty(aSelFil)
			aSelFil := AdmGetFil(.F.,.F.,"SE5")
			If Empty(aSelFil)
				Aadd(aSelFil,cFilAnt)
			Endif
		Endif
	Endif

	sRetFilial := fRetInFilial(aSelFil) // retorna as filiais Com IN Para ORACLE

	IF Select("CT2TRB") <> 0
		DbSelectArea("CT2TRB")
		DbCloseArea()
	ENDIF
//ct2_lote
	cQuery := "	select ctt.ctt_custo ctt_custo,ctt.ctt_desc01 ctt_desc01, ct2_itemd ctd_item, ctd_desc01 , ct2d.ct2_filial ct2_filial, ct2d.ct2_data ct2_data, ct2d.ct2_hist  ct2_hist, "
	cQuery += "	ct1.ct1_conta ct1_conta, ct1_desc01, 'd' tipo, ct2d.ct2_valor ct2_valor "
	cQuery += "	from "+RETSQLNAME("CTT")+" ctt "
	cQuery += "	inner join "+RETSQLNAME("CT2")+" ct2d on ctt.ctt_filial = ct2d.ct2_filial and (ct2_data between '"+dtos(dDataDe)+"' and '"+dtos(dDataAte)+"') and (ct2_debito between '"+Alltrim(sCtaDe)+"' and '"+Alltrim(sCtaAte)+"') and (ct2_itemd between '"+Alltrim(sItemDe)+"' and '"+Alltrim(sItemAte)+"') and ctt_custo = ct2_ccd and ct2d.d_e_l_e_t_ = ' ' "
	If !Empty(sLotInib)
		cQuery += "	and ct2d.ct2_lote not in ("+sLotInib+")"
	EndIf	
	cQuery += "	inner join "+RETSQLNAME("CT1")+" ct1  on ctt.ctt_filial = ct1.ct1_filial  and ct1_conta = ct2_debito and ct1.d_e_l_E_t_ = ' ' "
	cQuery += "	inner join "+RETSQLNAME("CTD")+" ctd  on ctt.ctt_filial = ctd.ctd_filial  and ctd_item = ct2_itemd and ctd.d_e_l_E_t_ = ' ' "
	cQuery += "	where ctt_custo between '"+sCCDe+"' and '"+sCCAte+"' "
	iF !Empty(sRetFilial)
		cQuery += "	and ctt_filial " + sRetFilial
	EndIf	
	cQuery += "	and ctt.d_e_l_E_t_ = ' ' "
	cQuery += "	union "
	cQuery += "	select ctt.ctt_custo ctt_custo,ctt.ctt_desc01 ctt_desc01, ct2_itemc ctd_item, ctd_desc01 , ct2c.ct2_filial ct2_filial, ct2c.ct2_data ct2_data, ct2c.ct2_hist  ct2_hist, "
	cQuery += "	ct1.ct1_conta ct1_conta, ct1_desc01,  'c' tipo, ct2c.ct2_valor ct2_valor "
	cQuery += "	from "+RETSQLNAME("CTT")+" ctt "
	cQuery += "	inner join "+RETSQLNAME("CT2")+" ct2c on ctt.ctt_filial = ct2c.ct2_filial and (ct2_data between '"+dtos(dDataDe)+"' and '"+dtos(dDataAte)+"') and (ct2_credit between '"+Alltrim(sCtaDe)+"' and '"+Alltrim(sCtaAte)+"') and (ct2_itemc between '"+Alltrim(sItemDe)+"' and '"+Alltrim(sItemAte)+"') and ctt_custo = ct2_ccc and ct2c.d_e_l_e_t_ = ' ' "
	If !Empty(sLotInib)
		cQuery += "	and ct2c.ct2_lote not in ('"+sLotInib+"')"
	EndIf	
	cQuery += "	inner join "+RETSQLNAME("CT1")+" ct1  on ctt.ctt_filial = ct1.ct1_filial  and ct1_conta = ct2_credit and ct1.d_e_l_E_t_ = ' ' "
	cQuery += "	inner join "+RETSQLNAME("CTD")+" ctd  on ctt.ctt_filial = ctd.ctd_filial  and ctd_item = ct2_itemc and ctd.d_e_l_E_t_ = ' ' "
	cQuery += "	where ctt_custo between '"+sCCDe+"' and '"+sCCAte+"' "
	iF !Empty(sRetFilial)
		cQuery += "	and ctt_filial " + sRetFilial
	EndIf	
	cQuery += "	and ctt.d_e_l_E_t_ = ' ' "
	cQuery += "	order by ctt_custo, ctd_item, ct2_filial, ct2_data, ct1_conta "

	cQuery := ChangeQuery(cQuery) 
	dbUseArea(.T.,'TOPCONN',TcGenQry(,,cQuery),"CT2TRB",.F.,.T. )
	
	dbSelectArea("CT2TRB")
	dbGoTop()

	oReport:OnPageBreak({|| R170Header(oReport)})
	//oReport:SetOrientation("Portrait") // Define a orientação do relatório para retrato
    //oReport:SetLandscape(.T.)	
    //oReport:SetPageSize("A4") // Define o tamanho da página como A4
    //oReport:SetMargin(20, 20, 20, 20)

	oSection1:Init()
	nCor := oReport:ClrFore() 
	nTotGeral := 0 
	
	While !Eof()
		
		If oReport:Cancel()
			Exit
		EndIf
		oReport:IncMeter()
		oReport:SkipLine()
		oReport:PrintText("Centro De Custos: "+Alltrim(CT2TRB->ctt_custo)+" - "+Alltrim(CT2TRB->ctt_desc01))
		oReport:SkipLine()
		oReport:FatLine()
		sCTT_CUSTO := Alltrim(CT2TRB->ctt_custo)
		nTotCC := 0 

		While !EOF() .And. sCTT_CUSTO == Alltrim(CT2TRB->ctt_custo)
			oReport:SkipLine()
			oReport:PrintText("Item Contabil: "+Alltrim(CT2TRB->ctd_item) + ' - ' + Alltrim(CT2TRB->ctd_desc01))
			oReport:SkipLine()
			oReport:FatLine()
			oReport:SkipLine()
			nTotItem := 0 
			sCTD_ITEM := Alltrim(CT2TRB->ctd_item)
			While !EOF() .And. sCTT_CUSTO == Alltrim(CT2TRB->ctt_custo) .And. sCTD_ITEM == Alltrim(CT2TRB->ctd_item)
				IncProc("Imprimindo......")
				If lPrim
					oSection1:PrintHeader()
					//oSection1:Printline()
				EndIf   
				lPrim 	:= .F.	  
				oSection1:Cell("CCT2_FILIAL"):SetValue(CT2TRB->ct2_filial)
				oSection1:Cell("DCT2_DATA"):SetValue(STOD(CT2TRB->ct2_data))
				oSection1:Cell("CCT2_HIST"):SetValue(CT2TRB->ct2_hist)
				oSection1:Cell("CCT1_CONTA"):SetValue(CT2TRB->ct1_conta)
				oSection1:Cell("CCT1_DESC01"):SetValue(CT2TRB->ct1_desc01)
				nCt2_valor := IIF( CT2TRB->tipo == 'c',CT2TRB->ct2_valor*(-1),CT2TRB->ct2_valor)
				oSection1:Cell("NCT2_VALOR"):SetValue(nCt2_valor)
			    oSection1:Printline()
				nTotItem += nCt2_valor
				nTotCC 	 += nCt2_valor
				nTotGeral += nCt2_valor
				dbSelectArea("CT2TRB")
				DbSkip()
			EndDo
			oReport:SkipLine()
			oReport:FatLine()
			oReport:PrintText("Total Por Item: ")
			oReport:PrintText(transf(nTotItem,"@E 999,999,999.99"),oReport:row(),2970)
			oReport:SkipLine()
			oReport:FatLine()
			oReport:SkipLine()
			lPrim 	:= .T.	      
		EndDo
		oReport:PrintText("TOTAL POR CENTRO DE CUSTOS: ")
		oReport:PrintText(transf(nTotCC,"@E 999,999,999.99"),oReport:row(),2970)

		oReport:SkipLine()
		oReport:FatLine()
		oReport:SkipLine()
	EndDo
	oReport:PrintText("TOTAL GERAL: ")
	oReport:PrintText(transf(nTotGeral,"@E 999,999,999.99"),oReport:row(),2970)

	oReport:FatLine()
	oSection1:Finish()
Return

static function ajustaSx1(cPerg)
	//Aqui utilizo a função putSx1, ela cria a pergunta na tabela de perguntas
	oPerg := TPergunta():New(cPerg)
	oPerg:AddGet  ("Data Inicial       ?",            "D", 08, 0, Nil, "", "")
	oPerg:AddGet  ("Data Final         ?",            "D", 08, 0, Nil, "", "")
	oPerg:AddGet  ("Conta Inicial      ?",            "C", 20, 0, Nil, "CT1", "")
	oPerg:AddGet  ("Conta FInal        ?",            "C", 20, 0, Nil, "CT1", "")
	oPerg:AddGet  ("De  CC             ?",            "C", 09, 0, Nil, "CTT", "")
	oPerg:AddGet  ("Ate CC             ?",            "C", 09, 0, Nil, "CTT", "")
	oPerg:AddGet  ("De  Item Cont.     ?",            "C", 20, 0, Nil, "CTD", "")
	oPerg:AddGet  ("Ate Item Cont.     ?",            "C", 20, 0, Nil, "CTD", "")
	oPerg:AddGet  ("Lotes Inibir S/ ','?",            "C", 60, 0, Nil, "", "")
	oPerg:AddCombo("Seleciona Filiais  ?", { "Sim", "Nao" }   , 2)
	oPerg:Update()
	oPerg:Execute(.F.)	
return


//-------------------------------------------------------------------
Static Function fRetInFilial(aSelFil) // Retorna comando IN em oracle
Local cRetornoIn := ""
Local nFor := 0

	For nFor := 1 To Len(aSelFil)
		cRetornoIn += Left(aSelFil[nFor],4) + '|' 
	Next nFor

Return(IIF(EMpty(cRetornoIn),'', " IN " + FormatIn( SubStr( cRetornoIn , 1 , Len( cRetornoIn ) -1 ) , '|' )))



Static Function R170Header(oReport)
Local cStartPath	:= GetSrvProfString("Startpath","")
Local cLogo			:= cStartPath +  "FIEMT.BMP" 	// Empresa+Filial
Local ofont	:= TFont():New("Arial Black",16,16,,.F.,,,,.T.,.F.)


oReport:SayBitmap (oReport:Row(),100,cLogo,291,250)
oReport:Say( oReport:Row()+80, 700, "RELATORIO DE DESPESA POR CENTRO DE CUSTO", ofont )
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()
oReport:SkipLine()

Return
