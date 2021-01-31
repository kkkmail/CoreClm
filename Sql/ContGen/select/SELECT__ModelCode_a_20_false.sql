;with q as
(
select top (10000000) 
	RowNumber,
	Code,
	right(Code, 3) as Catalyst,
	case when patindex('%backward%', Code) = 0 then 0 else 1 end as Backward,
	case when patindex('%forward%', Code) = 0 then 0 else 1 end as Forward,
	trim(replace(replace(left(right(Code, len(Code) - patindex('%consumption%', Code) - 10), patindex('%:%', right(Code, len(Code) - patindex('%consumption%', Code) - 10)) - 1), '(', ''), ')', '')) as Pair,
	trim(right(Code, len(Code) - charindex('//', Code) - 1)) as Comment
from ModelCode_a_20_false__004
where
	(1 = 1)
	--and (contains(ModelCode.Code, 'forward'))
	--and (contains(ModelCode.Code, 'backward'))
	and (contains(Code, 'catalytic'))
	and contains(Code, 'ligation')
	--and Code like '%| ligation%'
	--and len(Code) - len(replace(Code, '*', '')) = 3
	and trim(Code) not like '-%'
),
w as
(
select
	RowNumber,
	Catalyst,
	Forward,
	Backward,
	Pair,
	trim(left(Pair, patindex('%+%', Pair) - 1)) as A1,
	trim(right(Pair, len(Pair) - patindex('%+%', Pair) - 1)) as A2,
	Code,
	Comment
from q
)
-------------------------------------------------------
-------------------------------------------------------
--select 
--	*
---- IQT + abb | activated backward catalytic ligation with energy consumption (I + Q): I + QT + abb* <-> IQT + abb
--from w
----where Catalyst = 'abb' collate Latin1_General_CS_AS
--where Catalyst = 'ABB' collate Latin1_General_CS_AS and comment like '% I + Q + %' and Pair = 'I + Q' collate Latin1_General_CS_AS
----where Catalyst = 'acc' collate Latin1_General_CS_AS
----where Catalyst = 'Nif' collate Latin1_General_CS_AS
--order by RowNumber
-------------------------------------------------------
-------------------------------------------------------
 --All catalysts
select distinct 
	Catalyst collate Latin1_General_CS_AS as Cat,
	Backward,
	Forward,
	Pair collate Latin1_General_CS_AS as Pair,
	A1 collate Latin1_General_CS_AS as A1,
	A2 collate Latin1_General_CS_AS as A2
from w
order by 1
-------------------------------------------------------
-------------------------------------------------------

--select count(*)
--from q

-- ligation: 
--  777600

-- forward catalytic ligation
-- 1334880

-- backward catalytic ligation
-- 1334880

-- catalytic ligation
-- 2669760

-- Catalysts
-- 820

-- abb
-- 1620

