use clm601
go

; with w as
(
	select
		clmDefaultValueId,
		isnull(cast(dbo.getWasteRecyclingRate(clmDefaultValueId) as nvarchar(20)), '') as wasteRecyclingRate,
		isnull(cast(dbo.getCatSynthSim(clmDefaultValueId) as nvarchar(20)), '') as catSynthSim,
		isnull(cast(dbo.getCatSynthScarcity(clmDefaultValueId) as nvarchar(20)), '') as catSynthScarcity,
		isnull(cast(dbo.getCatDestrSim(clmDefaultValueId) as nvarchar(20)), '') as catDestrSim,
		isnull(cast(dbo.getCatDestrScarcity(clmDefaultValueId) as nvarchar(20)), '') as catDestrScarcity,
		dbo.getGroupId(clmDefaultValueId) as groupId,
		isnull(cast(dbo.getCatLigScarcity(clmDefaultValueId) as nvarchar(20)), '') as catLigScarcity,
		isnull(cast(dbo.getCatLigMult(clmDefaultValueId) as nvarchar(20)), '') as catLigMult,
		description
	from ClmDefaultValue
--	where clmDefaultValueId >= 4000000000
)
select * 
from w
--where
--	((catSynthScarcity = '50' and catSynthSim = '0.2') or (catSynthScarcity = '100' and catSynthSim = '0.1'))
--	and cast(wasteRecyclingRate as float) <= 0.0005
where catSynthSim = '0.1'
order by clmDefaultValueId
