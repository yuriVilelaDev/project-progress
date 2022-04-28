
FOR EACH natur-oper NO-LOCK:
     FIND FIRST VW_GRUPO_TRIBUTARIO_CLIENTE	 WHERE VW_GRUPO_TRIBUTARIO_CLIENTE.CODIGO = natur-oper.nat-operacao  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_GRUPO_TRIBUTARIO_CLIENTE	 THEN
        DO:
           IF  VW_GRUPO_TRIBUTARIO_CLIENTE.DESCRICAO = natur-oper.denominacao THEN NEXT.
           ELSE
              ASSIGN VW_GRUPO_TRIBUTARIO_CLIENTE.DESCRICAO = natur-oper.denominacao.
        END.
      ELSE 
          DO:
             CREATE VW_GRUPO_TRIBUTARIO_CLIENTE.
                    ASSIGN 
                        VW_GRUPO_TRIBUTARIO_CLIENTE.CODIGO          = natur-oper.nat-operacao
                        VW_GRUPO_TRIBUTARIO_CLIENTE.DESCRICAO		= natur-oper.denominacao.
           END.
 END.
