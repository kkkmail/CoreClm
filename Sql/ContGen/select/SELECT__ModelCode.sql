;with q as
(
select top (10000) 
	RowNumber,
	Code,
	trim(right(Code, len(Code) - charindex('//', Code) - 1)) as Comment,
	right(Code, 3) as Catalyst
from ModelData
where
	contains(ModelData.Code, 'catalytic')
	and contains(ModelData.Code, 'ligation')
	and len(Code) - len(replace(Code, '*', '')) = 3
	and Code not like '-%'
)
--select *
--from q
----where Catalyst = 'RAM' collate Latin1_General_CS_AS
----where Catalyst = 'Nif' collate Latin1_General_CS_AS
--order by RowNumber

select distinct Catalyst collate Latin1_General_CS_AS as Cat
from q
order by 1