
FOR EACH tb-preco NO-LOCK:
            FIND FIRST VW_TABELA_PRECO	 WHERE VW_TABELA_PRECO.CODIGO = tb-preco.  SHARE-LOCK NO-ERROR.
         IF AVAIL VW_TABELA_PRECO	  	 THEN
            DO:
               IF VW_TABELA_PRECO.DATA_VIGENCIA_FINAL = tb-preco.descricao OR  VW_TABELA_PRECO.DATA_VIGENCIA_FINAL = tb-preco.descricao OR VW_TABELA_PRECO.STATUS = tb-preco.situacao  THEN NEXT.
               ELSE
                  ASSIGN
                      VW_TABELA_PRECO.PRECO = tb-preco.nr-tabpre
                       VW_TABELA_PRECO.TABELA_PRECO_CODIGO = tb-preco.dt-fimval.
            
                                                  
            END.
          ELSE 
              DO:
                CREATE VW_TABELA_PRECO.
                    ASSIGN 
                        VW_TABELA_PRECO.PRECO = tb-preco.nr-tabpre
                        VW_TABELA_PRECO.PRODUTO_CODIGO = tb-preco.descricao
                        VW_TABELA_PRECO.TABELA_PRECO_CODIGO = tb-preco.dt-fimval.

           END.
       
 END.

