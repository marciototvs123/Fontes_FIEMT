#Include 'Protheus.ch'
#Include 'Protheus.ch'
#INCLUDE 'TOPCONN.CH'
/////////////////////////////
//FONTES  - Descricao
////////////////////////////
// MA410COR - PONTO DE ENTRADA PARA Alterar cores do cadastro do status do pedido
// MA410LEG - PONTO DE ENTRADA PARA COLOCAR LEGENDA NAS CORES DOS STATUS DO PEDIDO DE VENDA
////////////////////////////


************************
User Function MA410COR() //PONTO DE ENTRADA PARA Alterar cores do cadastro do status do pedido
*************
Local aCores := {} // PARAMIXB traz a estrutura do array padrão
    aAdd(aCores, {"C5_NOTA == 'XXXXXXXXX'", "BR_CINZA", "Eliminado Residuo"})
    aAdd(aCores, {"Empty(C5_LIBEROK).And.Empty(C5_NOTA) .And. Empty(C5_BLQ)", "ENABLE", "Pedido em Aberto"})
    aAdd(aCores, {"!Empty(C5_NOTA).Or.C5_LIBEROK=='E' .And. Empty(C5_BLQ)", "DISABLE", "Pedido Encerrado"})
    aAdd(aCores, {"!Empty(C5_LIBEROK).And.Empty(C5_NOTA).And. Empty(C5_BLQ)", "BR_AMARELO", "Pedido Liberado"})
    aAdd(aCores, {"C5_BLQ == '1'", "BR_AZUL"})
    aAdd(aCores, {"C5_BLQ == '2'", "BR_LARANJA"})
Return aCores


************************
User Function MA410LEG() //PONTO DE ENTRADA PARA COLOCAR LEGENDA NAS CORES DOS STATUS DO PEDIDO DE VENDA
*************
Local aLegenda := PARAMIXB
    aLegenda := {}
    AADD(aLegenda,{"BR_CINZA" ,"Eliminado Residuo"})
    AADD(aLegenda,{"ENABLE" ,"Pedido de Venda em aberto"})
    AADD(aLegenda,{"DISABLE" ,"Pedido de Venda encerrado"})
    AADD(aLegenda,{"BR_AMARELO" ,"Pedido de Venda liberado"})
    AADD(aLegenda,{"BR_AZUL" ,"Pedido Bloqueado por Regra"})
    AADD(aLegenda,{"BR_LARANJA" ,"Pedido Bloqueado por Verba"})
Return(aLegenda)

