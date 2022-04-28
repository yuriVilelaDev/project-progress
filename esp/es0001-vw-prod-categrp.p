FOR EACH familia NO-LOCK:
     FIND FIRST VW_PRODUTO_CATEGORIA	 WHERE VW_PRODUTO_CATEGORIA.CODIGO = familia.fm-codigo  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_PRODUTO_CATEGORIA  	 THEN
        DO:
           IF  VW_PRODUTO_CATEGORIA.DESCRICAO = familia.descricao THEN NEXT.
           ELSE
              ASSIGN VW_PRODUTO_CATEGORIA.DESCRICAO = familia.descricao.
        END.
      ELSE 
          DO:
             CREATE VW_PRODUTO_CATEGORIA.
                    ASSIGN 
                        VW_PRODUTO_CATEGORIA.CODIGO          = familia.fm-codigo
                        VW_PRODUTO_CATEGORIA.DESCRICAO		= familia.descricao.
           END.
 END.
