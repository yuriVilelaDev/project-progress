FOR EACH cond-pagto NO-LOCK:
     FIND FIRST VW_CONDICAO_PAGAMENTO WHERE VW_CONDICAO_PAGAMENTO.CODIGO = string(cond-pagto.cod-cond-pag)  SHARE-LOCK NO-ERROR.
     IF AVAIL VW_CONDICAO_PAGAMENTO THEN
        DO:
           IF  VW_CONDICAO_PAGAMENTO.DESCRICAO = cond-pagto.descricao THEN NEXT.
           ELSE
              ASSIGN VW_CONDICAO_PAGAMENTO.DESCRICAO = cond-pagto.descricao.
        END.
      ELSE 
          DO:
             CREATE VW_CONDICAO_PAGAMENTO.
                    ASSIGN 
                        VW_CONDICAO_PAGAMENTO.CODIGO          = string(cond-pagto.cod-cond-pag)
                        VW_CONDICAO_PAGAMENTO.DESCRICAO       = cond-pagto.descricao
                        VW_CONDICAO_PAGAMENTO.FILIAL_CODIGO   = "*"
                        VW_CONDICAO_PAGAMENTO.NUMERO_PARCELAS = cond-pagto.num-parcelas
                        VW_CONDICAO_PAGAMENTO.PRAZO_MEDIO     = cond-pagto.qtd-dias-prazo-medio.
           END.
 END.
