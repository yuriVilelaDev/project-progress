
FOR EACH tb-preco NO-LOCK:
            FIND FIRST VW_TABELA_PRECO	 WHERE VW_TABELA_PRECO.CODIGO = tb-preco.  SHARE-LOCK NO-ERROR.
         IF AVAIL VW_TABELA_PRECO	  	 THEN
            DO:
               IF VW_TABELA_PRECO.DATA_VIGENCIA_FINAL = tb-preco.descricao OR  VW_TABELA_PRECO.DATA_VIGENCIA_FINAL = tb-preco.descricao OR VW_TABELA_PRECO.STATUS = tb-preco.situacao  THEN NEXT.
               ELSE
                  ASSIGN
                        VW_TABELA_PRECO.DATA_VIGENCIA_FINAL = tb-preco.descricao
                        VW_TABELA_PRECO.DECRICAO = tb-preco.dt-fimval
                        VW_TABELA_PRECO.FILIAL_CODIGO ="*"
                        VW_TABELA_PRECO.STATUS = IF tb-preco.situacao = 1 THEN 
                                                        "A".
                                                 ELSE
                                                     "I".
            END.
          ELSE 
              DO:
                CREATE VW_TABELA_PRECO.
                    ASSIGN 
                        VW_TABELA_PRECO.CODIGO = tb-preco.nr-tabpre
                        VW_TABELA_PRECO.DATA_VIGENCIA_FINAL = tb-preco.descricao
                        VW_TABELA_PRECO.DECRICAO = tb-preco.dt-fimval
                        VW_TABELA_PRECO.FILIAL_CODIGO ="*"
                        VW_TABELA_PRECO.STATUS = IF tb-preco.situacao = 1 THEN 
                                                        "A".
                                                 ELSE
                                                     "I".
           END.
       
 END.

