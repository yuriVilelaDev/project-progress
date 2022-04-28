FOR EACH repres NO-LOCK:
    FOR EACH  mgcad.cidade OF repres NO-LOCK.
        //FOR EACH ems5.repres_financ OF repres NO-LOCK.
            FIND FIRST VW_VENDEDOR	 WHERE VW_VENDEDOR.CODIGO = STRING(repres.cod-rep)  SHARE-LOCK NO-ERROR.
         IF AVAIL VW_VENDEDOR	  	 THEN
            DO:
             /*  IF VW_VENDEDOR.NOME = repres_financ.nome OR VW_VENDEDOR.TELEFONE = repres_financ.telefone THEN NEXT.
               ELSE
                  ASSIGN VW_VENDEDOR.NOME = transporte.nome
                         VW_VENDEDOR.TELEFONE  = repres_financ.telefone.*/
            END.
          ELSE 
              DO:
                CREATE VW_VENDEDOR.
                    ASSIGN 
                        VW_VENDEDOR.CODIGO                     = string(repres.cod-rep)
                      //  VW_VENDEDOR.CODIGO_SUPERVISOR		   = repres_financ.nome
                        VW_VENDEDOR.EMAIL	                   = repres.e-mail
                        VW_VENDEDOR.ENDERECO_BAIRRO		       = repres.bairro
                        VW_VENDEDOR.ENDERECO_CEP	           = repres.cep
                        //VW_VENDEDOR.ENDERECO_COMPLEMENTO	   = repres.
                        //VW_VENDEDOR.ENDERECO_NUMERO		       = repres_financ.telefone
                        VW_VENDEDOR.ENDERECO_RUA	           = repres.endereco
                       // VW_VENDEDOR.ESTADO_CODIGO_IBGE		   = repres_financ.nome
                        VW_VENDEDOR.MUNICIPIO_CODIGO_IBGE	   = mgcad.cidade.cdn-munpio-ibge
                        VW_VENDEDOR.NOME			           = repres.nome
                       // VW_VENDEDOR.PERCENTUAL_COMISSAO	       = repres_financ.val_perc_comis_repres
                       // VW_VENDEDOR.RAZAO_SOCIAL		       = repres_financ.telefone
                        VW_VENDEDOR.TELEFONE				   = repres.telefone[1].
           END.
       // END.
    END.   
 END.
