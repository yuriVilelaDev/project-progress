FOR EACH gr-cli NO-LOCK:
     FIND FIRST VW_CLIENTE_GRUPO WHERE VW_CLIENTE_GRUPO.CODIGO = string(gr-cli.cod-gr-cli)  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_CLIENTE_GRUPO	THEN
        DO:
           IF VW_CLIENTE_GRUPO.DESCRICAO	 = gr-cli.descricao THEN NEXT.
           ELSE
              ASSIGN VW_CLIENTE_GRUPO.DESCRICAO	 = gr-cli.descricao.
        END.
      ELSE 
          DO:
             CREATE VW_CLIENTE_GRUPO.
                    ASSIGN 
                        VW_CLIENTE_GRUPO.CODIGO          = string(gr-cli.cod-gr-cli)
                        VW_CLIENTE_GRUPO.DESCRICAO	      = gr-cli.descricao.
           END.
 END.
