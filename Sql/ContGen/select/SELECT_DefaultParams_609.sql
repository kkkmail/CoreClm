use clm609
go

; with w as
(
	select
		clmDefaultValueId,
		isnull(cast(dbo.getWasteRecyclingRate(clmDefaultValueId) as nvarchar(20)), '') as wasteRecyclingRate,

		--isnull(cast(dbo.getCatSynthSim(clmDefaultValueId) as nvarchar(20)), '') as catSynthSim,
		--isnull(cast(dbo.getCatSynthScarcity(clmDefaultValueId) as nvarchar(20)), '') as catSynthScarcity,
		--isnull(cast(dbo.getCatDestrSim(clmDefaultValueId) as nvarchar(20)), '') as catDestrSim,
		--isnull(cast(dbo.getCatDestrScarcity(clmDefaultValueId) as nvarchar(20)), '') as catDestrScarcity,
		--dbo.getGroupId(clmDefaultValueId) as groupId,
		--isnull(cast(dbo.getCatLigScarcity(clmDefaultValueId) as nvarchar(20)), '') as catLigScarcity,
		--isnull(cast(dbo.getCatLigMult(clmDefaultValueId) as nvarchar(20)), '') as catLigMult,

		--isnull(cast(dbo.getLigBkw(clmDefaultValueId) as nvarchar(20)), '') as ligBkw,
		--isnull(cast(dbo.getLigFwd(clmDefaultValueId) as nvarchar(20)), '') as ligFwd,

		isnull(cast(dbo.getAcCatSynthScarcity(clmDefaultValueId) as nvarchar(20)), '') as acCatSynthScarcity,
		isnull(cast(dbo.getAcCatDestrScarcity(clmDefaultValueId) as nvarchar(20)), '') as acCatDestrScarcity,
		isnull(cast(dbo.getAcFwdCatLigScarcity(clmDefaultValueId) as nvarchar(20)), '') as acFwdCatLigScarcity,
		isnull(cast(dbo.getAcBkwCatLigScarcity(clmDefaultValueId) as nvarchar(20)), '') as acBkwCatLigScarcity,

		isnull(cast(dbo.getSugarForward(clmDefaultValueId) as nvarchar(20)), '') as sugarForward,
		isnull(cast(dbo.getSugarBackward(clmDefaultValueId) as nvarchar(20)), '') as sugarBackward,
		isnull(cast(dbo.getSugarScarcity(clmDefaultValueId) as nvarchar(20)), '') as sugarScarcity,

		description
	from ClmDefaultValue
	where clmDefaultValueId >= 4005000000 and clmDefaultValueId not in (4005000000, 4005000001, 4005000002, 4005000003, 4005000016)
)
select * 
from w
--where
--	((catSynthScarcity = '50' and catSynthSim = '0.2') or (catSynthScarcity = '100' and catSynthSim = '0.1'))
--	and cast(wasteRecyclingRate as float) <= 0.0005

--where catSynthSim = '0.1'

--where acCatSynthScarcity = ''
order by clmDefaultValueId
