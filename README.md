# Report Forecasting DMR-SS

Relatório para a definição de modelos de forecasting para os valores de remuneração e NPS da DMR-SS a nível CAE.

## Objectivo

+ Todos os meses a Segurança Social envia as DMRs para o INE. O INE já tem publicações que dependem totalmente destes dados. Existe a necessidade de criação de um "plano B" caso a SS não envie algum mês;

+ Um relatório mensal descreve os métodos e os resultados do forecasting;


## Relatório

Mensalmente, deve ser refeito o relatório em *Quarto* como a descrição dos métodos aplicados e um resumo dos resultados.



## RAP - Reproducible Analytical Pipeline

1.  Mensalmente a view V_DMRSS_FORECASTING é atualizada automáticamente em Oracle, como os dados da DMR-SS.

2.  Em R abrir o ficheiro `_targets.R` e mudar o ano e mês (linha 18) que queremos prever. Gravar.

3. O pipeline de análise reprodutível em R está implementado no ficheiro `_targets.R`.   
No ficheiro `_run_job.R`, fazemos o `tar_destroy` para garantir que não existem ficheiros pendentes que possam causar conflitos.   
Na console confirmar que vamos apagar os targes com a opção "1". Quando terminar, colocar o `tar_destroy` como comentário.
De seguida, como é um processo demorado (14 horas), em vez de executar o pipeline com `tar_make()`, vamos executar em segundo plano source-source as background job.

4. Depois da tabela estar criada, ela deve ser exportada do `R-Studio` para `SQL Oracle`. Para isso é necessário abrir o ficheiro `_upload.res.R` e executar.
Desta forma é feito o *append* dos resultados para DW2.
Quando terminar, temos que verificar se a tabela `T_DMRSS_FORECASTING` já está disponivel em Oracle.


5. ??? temos report?


## comments on commits

+ feat: a new feature introduced (add new barplot function)
+ fix: a bug fix (syntax error)
+ format: a change to formatting that doesn’t affect code logic
(ggplot theme changes)
+ perf: a change to performance (remove nested loop)
+ docs: a change to the documentation only (add examples)
