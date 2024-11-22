-- Criação da view para os primeiros teste
create or replace view v_dmrss_forecasting as
select /*+parallel(5)*/ t.ano,t.mes,t.cae3,
sum(nvl(t.dmrss_rem_total,0))dmrss_rem_total,
sum(nvl(t.dmrss_nps,0))dmrss_nps 
from FNTDMR.TF_DMR_BASE_COMUM t
where t.ano>'2014'
and -- selecionar as CAE dos IVNE
(t.CAE3 LIKE '05%' OR t.CAE3 LIKE '06%' OR t.CAE3 LIKE '07%' OR t.CAE3 LIKE '08%' OR t.CAE3 LIKE '09%' OR 
t.CAE3 LIKE '1%' OR t.CAE3 LIKE '2%' OR t.CAE3 LIKE '3%' OR t.CAE3 LIKE '4%' OR t.CAE3 LIKE '5%' OR 
t.CAE3 LIKE '60%' OR t.CAE3 LIKE '61%' OR t.CAE3 LIKE '62%' OR t.CAE3 LIKE '63%' OR t.CAE3 LIKE '68%' OR t.CAE3 LIKE '69%' OR
t.CAE3 LIKE '7%' OR 
t.CAE3 LIKE '80%' OR t.CAE3 LIKE '81%' OR t.CAE3 LIKE '82%')
and -- excluir UAE
t.nif not in 
(select k.unidadecod from UAE_TC k
where k.anomes_fim='299912'
group by k.unidadecod )
group by t.ano,t.mes,t.cae3
order by t.ano,t.mes,t.cae3





