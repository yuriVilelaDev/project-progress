FOR EACH natur-oper NO-LOCK:
     FIND FIRST VW_GRUPO_TRIBUTARIO_PRODUTO	 WHERE VW_GRUPO_TRIBUTARIO_PRODUTO.CODIGO = natur-oper.nat-operacao  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_GRUPO_TRIBUTARIO_PRODUTO  	 THEN
        DO:
           IF  VW_GRUPO_TRIBUTARIO_PRODUTO.DESCRICAO = natur-oper.denominacao THEN NEXT.
           ELSE
              ASSIGN VW_GRUPO_TRIBUTARIO_PRODUTO.DESCRICAO = natur-oper.denominacao.
        END.
      ELSE 
          DO:
             CREATE VW_GRUPO_TRIBUTARIO_PRODUTO.
                    ASSIGN 
                        VW_GRUPO_TRIBUTARIO_PRODUTO.CODIGO          = natur-oper.nat-operacao
                        VW_GRUPO_TRIBUTARIO_PRODUTO.DESCRICAO		= natur-oper.denominacao.
           END.
 END.
