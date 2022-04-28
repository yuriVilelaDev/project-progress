FOR EACH transporte NO-LOCK:
     FIND FIRST VW_TRANSPORTADORA	 WHERE VW_TRANSPORTADORA.CODIGO = STRING(transporte.cod-transp)  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_TRANSPORTADORA	  	 THEN
        DO:
           IF VW_TRANSPORTADORA.NOME = transporte.nome OR VW_TRANSPORTADORA.TELEFONE = transporte.telefone THEN NEXT.
           ELSE
              ASSIGN VW_TRANSPORTADORA.NOME = transporte.nome
                     VW_TRANSPORTADORA.TELEFONE  = transporte.telefone.
        END.
      ELSE 
          DO:
             CREATE VW_TRANSPORTADORA.
                    ASSIGN 
                        VW_TRANSPORTADORA.CODIGO         = string(transporte.cod-transp)
                        VW_TRANSPORTADORA.NOME		     = transporte.nome
                        VW_TRANSPORTADORA.TELEFONE       = transporte.telefone
                        VW_TRANSPORTADORA.FILIAL_CODIGO	 = "*".
           END.
 END.
