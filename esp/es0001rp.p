/*                                                                                                */
/* Relatorio desenvolvido para listagem de ordens de produ»’o, infoma»’o de reservas e cogera»„es */
/* levando em considera»’o a realidade de engenharia da Bcube NO dia 19/02/2020 conforme abaixo:  */
/* -Itens de lateral, tampa, cabecote,conjuto e peca s’o fantasmas                                */
/* -Cada semiacabado tem apenas um ITEM na sua reserva                                            */
/* -A engenharia possui apenas 3 niveis desconsiderando os produtos fantasmas                     */

/* include de controle de vers’o */
{include/i-prgvrs.i es0001RP 12.1.24.001}

/* defini»’o das temp-tables para recebimento de par³metros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec           as INTEGER
    FIELD l-vw-con-pg         AS LOGICAL
    FIELD l-vw-grup-trib-clie AS LOGICAL
    FIELD l-vw-grup-trib-prod AS LOGICAL
    FIELD l-vw-cliet-grup     AS LOGICAL
    FIELD l-vw-prod-ca        AS LOGICAL
    FIELD l-vw-prod-grup      AS LOGICAL
    FIELD l-vw-trans          AS LOGICAL
    FIELD l-vw-vend           AS LOGICAL
    FIELD l-vw-tb-preco       AS LOGICAL
    FIELD l-vw-tb-preco-item  AS LOGICAL.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
FIELD raw-digita	   AS RAW.

DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{utp/ut-glob.i}

/* include padr’o para variÿveis de relat½rio  */
{include/i-rpvar.i}

/* defini»’o de variÿveis  */

DEFINE VARIABLE i-linha         AS INTEGER              NO-UNDO.
DEFINE VARIABLE c-dir           AS CHARACTER            NO-UNDO.
DEFINE VARIABLE c-arq-rel       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE               NO-UNDO.

/* VALORIZA›øO DAS OUTRAS VARIÉVEIS QUE COMPOEM O CABE›ALHO PADRøO */
ASSIGN c-programa     = "ES0001RP":U
       c-versao       = "12.1":U
       c-revisao      = ".24.001":U
       c-empresa      = "Bcube"
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "Integracao DW":U.

ASSIGN c-dir = SESSION:TEMP-DIRECTORY.
//ASSIGN c-arq-rel = c-dir + "ES0001-" + STRING(TIME).


FIND FIRST tt-param.

/* executando de forma persistente o utilitÿrio de acompanhamento e excel */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Aguarde...").
DEFINE VARIABLE h-handle AS HANDLE NO-UNDO.    
RUN utp/utapi033.p PERSISTENT SET h-handle.

IF tt-param.l-vw-con-pg = YES THEN DO:
        // manipula dados da tabela VW-CONDICAO-PAGAMENTO
            RUN esp\es0001-vw-cond-pagrp.p.
          END.
IF tt-param.l-vw-grup-trib-clie = YES THEN DO:
        // manipula dados da tabela VW_GRUPO_TRIBUTARIO_CLIENTE
            RUN esp\es0001-vw-grup-trub-clietrp.p.
        END.
IF tt-param.l-vw-grup-trib-prod = YES THEN DO:
        // manipula dados da tabela VW_GRUPO_TRIBUTARIO_PRODUTO	
           RUN esp\es0001-vw-grup-trub-prodrp.p.
        END.
IF tt-param.l-vw-cliet-grup = YES THEN DO:
        // manipula dados da tabela VW_CLIENTE_GRUPO	
          RUN esp\es0001-vw-cliet-gruprp.p.
        END.
IF tt-param.l-vw-prod-ca = YES THEN DO:
        // manipula dados da tabela VW_PRODUTO_CATEGORIA	
          RUN esp\es0001-vw-prod-categrp.p.
        END.
IF tt-param.l-vw-prod-grup = YES THEN DO:
        // manipula dados da tabela VW_PRODUTO_GRUPO		
          RUN esp\es0001-vw-prod-gruprp.p.
        END. 
IF tt-param.l-vw-trans = YES THEN DO:
        // manipula dados da tabela VW_PRODUTO_GRUPO		
          RUN esp\es0001-vw-transrp.p.
        END. 
IF tt-param.l-vw-vend = YES THEN DO:
        // manipula dados da tabela VW_VENDEDOR			
          RUN esp\es0001-vw-vendrp.p.
        END.

IF tt-param.l-vw-tb-preco = YES THEN DO:
        // manipula dados da tabela VW_VENDEDOR			
          RUN esp\es0001-vw-tab-precorp.p.
END.
IF tt-param.l-vw-tb-preco-item = YES THEN DO:
        // manipula dados da tabela VW_VENDEDOR			
          RUN esp\es0001-vw-tab-prec-itemrp.p.
END.

RUN pi-finalizar in h-acomp.




