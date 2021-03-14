use clm604
go

select count(*) as noOfResults from ResultData
go


-- Calculates how often the symmetry is broken for all processed default sets.
declare @maxWeightedAverageAbsEe float, @maxLastEe float, @runeTimeEst float
set @maxWeightedAverageAbsEe = 0.003
set @maxLastEe = 0.003
set @runeTimeEst = 1.60

; with
a as
(
	select
		d.clmDefaultValueId as defaultSetIndex,
		t.numberOfAminoAcids,
		t.maxPeptideLength,
		sum(
			case
				when q.RunQueueStatusId = 0 then 1.0 
				when q.RunQueueStatusId = 2 then (1.0 - q.progress) 
				else 0 
		end) as remainingRepetitions
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on m.clmTaskId = t.clmTaskId
		inner join RunQueue q on q.modelDataId = m.modelDataId
	group by d.clmDefaultValueId, t.numberOfAminoAcids, t.maxPeptideLength
),
b as
(
	select
		d.clmDefaultValueId as defaultSetIndex,
		t.numberOfAminoAcids,
		t.maxPeptideLength,
		m.modelDataId,
		case 
			when q.errorMessage like 'The run queue was cancelled at: %' then 1
			when r.maxWeightedAverageAbsEe > @maxWeightedAverageAbsEe or r.maxLastEe > @maxLastEe then 1 
			else 0 
		end as isSymmetryBroken,
		cast(datediff(minute, isnull(q.startedOn, m.createdOn), r.createdOn) as float) / 1440.0 as runTime
	from
		RunQueue q
		inner join ModelData m on q.modelDataId = m.modelDataId
		inner join ClmTask t on m.clmTaskId = t.clmTaskId
		inner join ClmDefaultValue d on t.clmDefaultValueId = d.clmDefaultValueId
		left outer join ResultData r on m.modelDataId = r.modelDataId
	where isnull(r.maxEe, 0) <= 1 and q.runQueueStatusId = 3
),
c as
(
	select distinct
		defaultSetIndex,
		numberOfAminoAcids,
		maxPeptideLength,
		modelDataId,
		max(isSymmetryBroken) as isSymmetryBroken,
		avg(runTime) as runTime
	from b
	group by defaultSetIndex, numberOfAminoAcids, maxPeptideLength, modelDataId
),
d as
(
	select
		defaultSetIndex,
		numberOfAminoAcids,
		maxPeptideLength,
		count(*) as modelCount,
		avg(runTime) as runTime
	from c
	group by defaultSetIndex, numberOfAminoAcids, maxPeptideLength
),
e as
(
	select
		defaultSetIndex,
		numberOfAminoAcids,
		maxPeptideLength,
		count(*) as symmBrokenCount
	from c
	where isSymmetryBroken = 1
	group by defaultSetIndex, numberOfAminoAcids, maxPeptideLength
),
f as
(
	select
		a.numberOfAminoAcids,
		a.maxPeptideLength,
		a.defaultSetIndex,
		isnull(d.modelCount, 0) as modelCount,
		isnull(e.symmBrokenCount, 0) as symmBrokenCount,
		cast(isnull(cast(isnull(e.symmBrokenCount, 0) as float) / cast(d.modelCount as float), 0) as money) as symmBrokenPct,
		isnull(cast(cast(d.runTime as decimal(10, 2)) as nvarchar(20)), '') as runTime,
		isnull(cast(dbo.getWasteRecyclingRate(a.defaultSetIndex) as nvarchar(20)), '') as wasteRecyclingRate,
		isnull(cast(dbo.getCatSynthSim(a.defaultSetIndex) as nvarchar(20)), '') as catSynthSim,
		isnull(cast(dbo.getCatSynthScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catSynthScarcity,
		isnull(cast(dbo.getCatDestrSim(a.defaultSetIndex) as nvarchar(20)), '') as catDestrSim,
		isnull(cast(dbo.getCatDestrScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catDestrScarcity,
		isnull(cast(dbo.getLigFwd(a.defaultSetIndex) as nvarchar(20)), '') as ligFwd,
		isnull(cast(dbo.getLigBkw(a.defaultSetIndex) as nvarchar(20)), '') as ligBkw,
		isnull(cast(dbo.getCatLigScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catLigScarcity,
		isnull(cast(dbo.getCatLigMult(a.defaultSetIndex) as nvarchar(20)), '') as catLigMult,
		isnull(cast(dbo.getCatLigSim(a.defaultSetIndex) as nvarchar(20)), '') as catLigSim,
		cast(a.remainingRepetitions * isnull(d.runTime, @runeTimeEst) as decimal(10, 2)) as remainingRunTime
	from a
		left outer join d on a.defaultSetIndex = d.defaultSetIndex and a.numberOfAminoAcids = d.numberOfAminoAcids
		left outer join e on a.defaultSetIndex = e.defaultSetIndex and a.numberOfAminoAcids = e.numberOfAminoAcids
)

select 
	* 
from f
order by numberOfAminoAcids, defaultSetIndex
