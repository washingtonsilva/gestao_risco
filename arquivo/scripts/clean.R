
# pacotes utilizados ------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(plyr)
library(likert)


# importando o arquivo csv ------------------------------------------------

## definindo o diretorio e o arquivo csv
path <- 'dados_brutos/data_raw.csv'

## importando com readr::read_csv()

dados <- read_csv(path, 
                  na = c(""),
                  col_select = -c(1:3,73:74),
                  name_repair = "universal") |> 
                  clean_names()
names(dados)

## salvando nomes em arquivo txt
sink("dados_brutos/nomes.txt")
names(dados)
sink()


# preparando os dados para analise -------------------------------------------

## salvando as questoes 3.3 e 3.5 em outro objeto
q_33_35 <- select(dados, c(x3_3_caso_a_instituicao_realize_a_gestao_de_riscos_qual_s_tecnica_s_e_utilizada_para_identificacao_dos_riscos_marque_todas_que_se_aplicam, 
                         x3_5_caso_a_instituicao_realize_a_gestao_de_riscos_qual_s_tecnica_s_e_utilizada_para_avaliar_os_riscos_marque_todas_que_se_aplicam))


## excluindo as questoes 3.3 e 3.5

dados <- select(dados, -c(x3_3_caso_a_instituicao_realize_a_gestao_de_riscos_qual_s_tecnica_s_e_utilizada_para_identificacao_dos_riscos_marque_todas_que_se_aplicam, 
                          x3_5_caso_a_instituicao_realize_a_gestao_de_riscos_qual_s_tecnica_s_e_utilizada_para_avaliar_os_riscos_marque_todas_que_se_aplicam))

## alterando os nomes das variaveis/colunas

dados <- dados %>% dplyr::rename(
q1_1 = x1_1_qual_e_a_sua_faixa_etaria,
q1_2 = x1_2_qual_o_seu_genero, 
q1_3 = x1_3_qual_e_o_seu_tipo_de_funcao, 
q1_4 = x1_4_qual_cargo_ocupa_no_momento, 
q1_5 = x1_5_qual_e_o_seu_grau_de_formacao , 
q1_6 = x1_6_quanto_tempo_de_experiencia_na_instituicao, 
q1_7 = x1_7_quanto_tempo_de_experiencia_no_cargo_que_ocupa_atualmente, 
q1_8 = x1_8_ja_participou_de_algum_curso_especifico_sobre_gestao_de_riscos,
q1_9 = x1_9_qual_o_instituto_federal_que_voce_atua, 
q2_1 = x2_1_a_missao_a_visao_e_os_valores_da_instituicao_sao_formalizados_e_comunicados_internamente_e_externamente,  
q2_2 = x2_2_a_instituicao_estabelece_estrutura_operacional_na_busca_dos_objetivos_estrategicos, 
q2_3 = x2_3_a_instituicao_dispoe_de_ouvidoria,
q2_4 = x2_4_a_instituicao_possui_medidas_de_desempenho_que_indicam_a_efetividade_dos_resultados, 
q2_5 = x2_5_as_responsabilidades_dos_membros_da_estrutura_interna_de_governanca_da_instituicao_estao_definidas, 
q2_6 = x2_6_a_entidade_dispoe_de_plano_de_integridade_conforme_decreto_9_203_2017, 
q2_7 = x2_7_a_organizacao_dispoe_de_auditoria_interna,
q2_8 = x2_8_a_organizacao_dispoe_de_corregedoria, 
q2_9 = x2_9_os_principais_processos_estao_identificados_e_mapeados, 
q2_10 = x2_10_o_modelo_de_gestao_de_riscos_da_organizacao_esta_estabelecido, 
q2_11 = x2_11_a_organizacao_dispoe_de_comite_de_etica,
q2_12 = x2_12_existe_na_instituicao_algum_suporte_de_especialistas_externos_terceirizados_consultoria_especializada, 
q2_13 = x2_13_a_alta_administracao_estabeleceu_modelo_de_gestao_de_pessoas, 
q2_14 = x2_14_a_gestao_estabeleceu_modelo_de_gestao_de_tecnologia_da_informacao, 
q2_15 = x2_15_a_gestao_estabeleceu_modelo_de_gestao_de_contratacoes_por_exemplo_terceirizacao_compras_compras_conjuntas_estoques_sustentabilidade,
q2_16 = x2_16_existe_na_instituicao_algum_plano_de_capacitacao_e_educacao_interna, 
q2_17 = x2_17_existe_na_instituicao_sistema_de_avaliacao_de_desempenho_dos_servidores, 
q2_18 = x2_18_a_gestao_de_riscos_e_integrada_ao_processo_de_planejamento_estrategico_da_instituicao, 
q3_1 = x3_1_a_instituicao_ja_definiu_um_comite_responsavel_pela_gestao_de_riscos , 
q3_2 = x3_2_ha_uma_definicao_de_quem_departamento_unidade_sera_o_responsavel_pelas_categorias_de_risco, 
q3_4 = x3_4_o_processo_de_identificacao_de_riscos_considera_explicitamente_a_possibilidade_de_fraudes_burla_de_controles_e_outros_atos_improprios,  
q3_6 = x3_6_utiliza_se_um_canal_reporte_formalizado_para_atribuir_a_alta_gestao_a_responsabilidade_dos_riscos_que_excederem_a_tolerancia,  
q3_7 =x3_7_voce_concorda_que_existe_contribuicao_dos_auditores_de_orgaos_de_controle_tcu_cgu_para_gerenciar_os_riscos_da_sua_instituicao,
q3_8 = x3_8_voce_concorda_que_existe_contribuicao_dos_auditores_internos_para_o_processo_de_gestao_de_riscos_da_sua_instituicao,  
q3_9 = x3_9_a_instituicao_leva_em_consideracao_riscos_decorrente_do_cenario_politico, 
q3_10 = x3_10_sao_identificados_riscos_referentes_as_atividades_de_aquisicao_compras,
q3_11 = x3_11_sao_identificados_riscos_referentes_as_atividades_de_contratos_continuados, 
q3_12 = x3_12_sao_identificados_riscos_referentes_as_atividades_de_convenios, 
q3_13 = x3_13_sao_identificados_riscos_referentes_as_atividades_financeiras_arrecadacao_da_receita_liquidacao_caixa_central_e_tesouraria, 
q3_14 = x3_14_sao_identificados_riscos_referentes_as_atividades_relacionadas_a_folha_de_pagamento, 
q3_15 = x3_15_sao_identificados_riscos_referentes_as_atividades_fins_de_ensino_pesquisa_e_extensao, 
q3_16 = x3_16_sao_identificados_riscos_referentes_a_imagem_da_instituicao, 
q3_17 = x3_17_existe_na_instituicao_uma_funcao_ou_unidade_organizacional_de_compliance_que_monitore_riscos_especificos_de_nao_conformidade_com_leis_e_regulamentos, 
q3_18 = x3_18_ocorre_monitoramento_periodico_continuo_da_gestao_de_riscos_na_instituicao, 
q3_19 = x3_19_os_resultados_das_atividades_de_monitoramento_sao_utilizados_para_as_tomadas_de_medidas_necessarias_a_correcao_de_deficiencias_e_a_melhoria_continua_do_desempenho_da_gestao_de_riscos,
q4_1 = x4_1_existe_a_possibilidade_de_se_ignorar_riscos_importantes_na_instituicao, 
q4_2 = x4_2_pode_se_lidar_de_maneira_inadequada_com_a_incerteza_resultante_de_informacoes_incompletas_ou_da_complexidade_do_proprio_risco, 
q4_3 = x4_3_pode_haver_falta_de_confianca_ou_entendimento_entre_os_servidores_envolvidos, 
q4_4 = x4_4_pode_haver_divergencias_em_torno_da_gravidade_percebida_de_um_risco_ou_de_estrategias_adotadas_para_administra_lo,
q4_5 = x4_5_pode_haver_estruturas_e_sistemas_institucionais_inadequados_para_a_gestao_de_riscos_na_instituicao, 
q4_6 = x4_6_a_instituicao_nao_dispoe_de_sistema_de_informacao_capaz_de_gerenciar_riscos_ou_o_mesmo_e_ineficiente, 
q4_7 = x4_7_pode_haver_dificuldade_na_renovacao_do_ciclo_de_gerenciamento_de_riscos_tendo_em_vista_que_o_mesmo_deve_ser_continuo, 
q4_8 = x4_8_a_falta_de_mapeamento_dos_processos_na_instituicao_pode_ser_decisiva_para_a_implantacao_eficaz_da_gestao_de_riscos_na_instituicao,
q4_9 = x4_9_a_falta_de_engajamento_dos_servidores_envolvidos_pode_ser_um_fator_comprometedor_para_a_gestao_de_riscos_na_instituicao, 
q4_10 = x4_10_a_falta_de_capacitacao_de_servidores_ainda_constitui_um_fator_limitante_para_o_sucesso_da_gestao_de_riscos_na_instituicao, 
q4_11 = x4_11_o_excesso_de_demandas_atuais_pode_comprometer_o_sucesso_da_implantacao_de_gestao_de_riscos_na_instituicao, 
q4_12 = x4_12_a_administracao_aloca_recursos_suficientes_e_apropriados_para_a_gestao_de_riscos_por_exemplo_pessoal_estruturas_sistemas_de_ti_programas_de_treinamento_metodos_e_ferramentas_para_gerenciar_riscos,
q5_1 = x5_1_os_riscos_identificados_sao_capazes_de_comprometer_a_implementacao_do_plano_estrategico_da_instituicao, 
q5_2 = x5_2_com_a_implantacao_do_sistema_de_gestao_de_riscos_as_leis_e_os_regulamentos_aplicaveis_estao_sendo_cumpridos_com_mais_facilidade_compliance, 
q5_3 = x5_3_os_riscos_da_organizacao_estao_dentro_dos_seus_criterios_de_risco_vale_dizer_dentro_do_apetite_a_risco_definido_e_das_variacoes_aceitaveis_no_desempenho_ou_tolerancias_a_risco_estabelecidas, 
q5_4 = x5_4_a_gestao_de_riscos_e_capaz_de_promover_a_reducao_de_custos_por_meio_do_desenvolvimento_de_sinergias_entre_unidades_de_negocios_e_departamentos,
q5_5 = x5_5_a_gestao_de_riscos_melhora_a_transparencia_para_as_partes_interessadas_reduzindo_o_escrutinio_regulatorio_e_despesas_com_litigios, 
q5_6 = x5_6_a_gestao_de_riscos_pode_promover_a_integridade_e_prevenir_a_improbidade_os_desvios_e_a_corrupcao, 
q5_7 = x5_7_a_gestao_de_riscos_mostrou_ser_capaz_de_reduzir_surpresas_e_prejuizos_operacionais_na_instituicao, 
q5_8 = x5_8_a_gestao_de_riscos_pode_auxiliar_na_identificacao_de_problemas_atuais_e_emergentes_da_instituicao_gerando_maior_confiabilidade,
q5_9 = x5_9_a_gestao_de_riscos_auxilia_a_monitorar_a_adequacao_e_a_eficacia_do_controle_interno, 
q5_10 = x5_10_a_gestao_de_risco_pode_ser_capaz_de_permitir_estabilidade_orcamentaria_prevenindo_surpresas_ao_elaborar_o_orcamento 
  )

names(dados)


## excluindo pontos (.) do final das questoes em escala de likert (5 pontos)

dados$q3_7 <- gsub('\\.', '', dados$q3_7)
dados$q3_8 <- gsub('\\.', '', dados$q3_8)
dados$q4_1 <- gsub('\\.', '', dados$q4_1)
dados$q4_2 <- gsub('\\.', '', dados$q4_2)
dados$q4_3 <- gsub('\\.', '', dados$q4_3)
dados$q4_4 <- gsub('\\.', '', dados$q4_4)
dados$q4_5 <- gsub('\\.', '', dados$q4_5)
dados$q4_6 <- gsub('\\.', '', dados$q4_6)
dados$q4_7 <- gsub('\\.', '', dados$q4_7)
dados$q4_8 <- gsub('\\.', '', dados$q4_8)
dados$q4_9 <- gsub('\\.', '', dados$q4_9)
dados$q4_10 <- gsub('\\.', '', dados$q4_10)
dados$q4_11 <- gsub('\\.', '', dados$q4_11)
dados$q4_12 <- gsub('\\.', '', dados$q4_12)
dados$q5_1 <- gsub('\\.', '', dados$q5_1)
dados$q5_2 <- gsub('\\.', '', dados$q5_2)
dados$q5_3 <- gsub('\\.', '', dados$q5_3)
dados$q5_4 <- gsub('\\.', '', dados$q5_4)
dados$q5_5 <- gsub('\\.', '', dados$q5_5)
dados$q5_6 <- gsub('\\.', '', dados$q5_6)
dados$q5_7 <- gsub('\\.', '', dados$q5_7)
dados$q5_8 <- gsub('\\.', '', dados$q5_8)
dados$q5_9 <- gsub('\\.', '', dados$q5_9)
dados$q5_10 <- gsub('\\.', '', dados$q5_10)

# convertendo para a classe factor ----------------------------------------

meus_niveis <- c("Discordo totalmente", "Discordo parcialmente", 
                 "Nem concordo, nem discordo", "Concordo parcialmente", 
                 "Concordo totalmente")

dados_categoricos <- dados %>% 
                     transmute(q1_1 = as.factor(q1_1), 
                               q1_2 = as.factor(q1_2), 
                               q1_3 = as.factor(q1_3), 
                               q1_4 = as.factor(q1_4),
                               q1_5 = as.factor(q1_5), 
                               q1_6 = as.factor(q1_6), 
                               q1_7 = as.factor(q1_7), 
                               q1_8 = as.factor(q1_8),
                               q1_9 = as.factor(q1_9), 
                               q2_1 = as.factor(q2_1),  
                               q2_2 = as.factor(q2_2), 
                               q2_3 = as.factor(q2_2),
                               q2_4 = as.factor(q2_2), 
                               q2_5 = as.factor(q2_5), 
                               q2_6 = as.factor(q2_6), 
                               q2_7 = as.factor(q2_7),
                               q2_8 = as.factor(q2_8), 
                               q2_9 = as.factor(q2_9), 
                               q2_10 = as.factor(q2_10), 
                               q2_11 = as.factor(q2_11),
                               q2_12 = as.factor(q2_12), 
                               q2_13 = as.factor(q2_13), 
                               q2_14 = as.factor(q2_14), 
                               q2_15 = as.factor(q2_15),
                               q2_16 = as.factor(q2_16), 
                               q2_17 = as.factor(q2_17), 
                               q2_18 = as.factor(q2_18), 
                               q3_1 = as.factor(q3_1), 
                               q3_2 = as.factor(q3_2), 
                               q3_4 = as.factor(q3_4),  
                               q3_6 = as.factor(q3_6),  
                               q3_7 = factor(q3_7, levels = meus_niveis), 
                               q3_8 = factor(q3_8, levels = meus_niveis),  
                               q3_9 = as.factor(q3_9), 
                               q3_10 = as.factor(q3_10), 
                               q3_11 = as.factor(q3_11), 
                               q3_12 = as.factor(q3_12), 
                               q3_13 = as.factor(q3_13), 
                               q3_14 = as.factor(q3_14), 
                               q3_15 = as.factor(q3_15), 
                               q3_16 = as.factor(q3_16), 
                               q3_17 = as.factor(q3_17), 
                               q3_18 = as.factor(q3_18), 
                               q3_19 = as.factor(q3_19),
                               q4_1 = as.factor(q4_1), 
                               q4_2 = as.factor(q4_2), 
                               q4_3 = as.factor(q4_3), 
                               q4_4 = as.factor(q4_4),
                               q4_5 = as.factor(q4_5), 
                               q4_6 = as.factor(q4_6), 
                               q4_7 = as.factor(q4_7), 
                               q4_8 = as.factor(q4_8),
                               q4_9 = as.factor(q4_9), 
                               q4_10 = as.factor(q4_10), 
                               q4_11 = as.factor(q4_11), 
                               q4_12 = as.factor(q4_12),
                               q5_1 = as.factor(q5_1), 
                               q5_2 = as.factor(q5_2), 
                               q5_3 = as.factor(q5_3), 
                               q5_4 = as.factor(q5_4),
                               q5_5 = as.factor(q5_5), 
                               q5_6 = as.factor(q5_6), 
                               q5_7 = as.factor(q5_7), 
                               q5_8 = as.factor(q5_8),
                               q5_9 = as.factor(q5_9), 
                               q5_10 = as.factor(q5_10)) %>%
                        as.data.frame()

class(dados_categoricos)

glimpse(dados_categoricos)

# selecionando as variaveis do Grupo 3 ------------------------------------

## selecionando as questoes com respostas na escala likert
dados_grupo3_likert <- dados_categoricos %>% select(q3_7, q3_8)  
glimpse(dados_grupo3_likert)

## selecionando as questoes com respostas categoricas binarias
dados_grupo3_binarias <- dados_categoricos %>%
  select(q3_1, q3_2, q3_4, q3_6, q3_9, q3_10, q3_11, q3_12,
         q3_13, q3_14, q3_15, q3_16, q3_17, q3_18, q3_19)  
glimpse(dados_grupo3_binarias)


## alterando o nome das questoes/variaveis tipo likert

names(dados_grupo3_likert) <- c(q3_7 = "Você concorda que existe contribuição 
                                        dos auditores de órgãos de controle 
                                        (TCU/CGU) para gerenciar os riscos da 
                                        sua instituição?",
                                q3_8 = "Você concorda que existe contribuição 
                                        dos auditores internos para o processo 
                                        de gestão de riscos da sua instituição?")

## criando estrutura de dados para o pacote likert 
grupo3_likert <- likert(dados_grupo3_likert)


## grafico de barras empilhadas basico via pacote likert
g1 <- plot(grupo3_likert, 
           type = "bar", 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = TRUE,
           plot.percent.high = TRUE,
           plot.percent.neutral = TRUE,
           plot.percents = FALSE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = TRUE,
           zero.print = TRUE) + 
           labs(y = "Porcentagem")

