FOR EACH grup-estoque NO-LOCK:
     FIND FIRST VW_PRODUTO_GRUPO	 WHERE VW_PRODUTO_GRUPO.CODIGO =STRING(grup-estoque.ge-codigo)  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_PRODUTO_GRUPO	  	 THEN
        DO:
           IF  VW_PRODUTO_GRUPO.DESCRICAO = grup-estoque.descricao THEN NEXT.
           ELSE
              ASSIGN VW_PRODUTO_GRUPO.DESCRICAO = grup-estoque.descricao.
        END.
      ELSE 
          DO:
             CREATE VW_PRODUTO_GRUPO.
                    ASSIGN 
                        VW_PRODUTO_GRUPO.CODIGO          = string(grup-estoque.ge-codigo)
                        VW_PRODUTO_GRUPO.DESCRICAO		= grup-estoque.descricao.
           END.
 END.
