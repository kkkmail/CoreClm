use clm607
go

-- Calculates how often the symmetry is broken for all processed default sets.
declare @startRunQueueOrder bigint
set @startRunQueueOrder = 21

declare @eps1 float, @eps2 float
set @eps1 = 0.50
set @eps2 = 0.25

declare @maxWeightedAverageAbsEe float, @maxLastEe float, @runeTimeEst float
set @maxLastEe = 1.30
set @maxWeightedAverageAbsEe = 0.30
set @runeTimeEst = 1.60

; with
a as
(
	select
		d.clmDefaultValueId as defaultSetIndex,
		t.clmTaskStatusId,
		t.numberOfAminoAcids,
		t.maxPeptideLength,
		sum(
			case
				when q.RunQueueStatusId = 0 then 1.0 
				when q.RunQueueStatusId = 2 then (1.0 - q.progress) 
				else 0 
		end) as remainingRepetitions,
		isnull(
			sum(
				case
					when q.RunQueueStatusId = 2 then 1
					else 0 
			end), 0) as running,
		sum(
			case
				when q.RunQueueStatusId = 2 then q.progress
				else 0 
		end) as progress
	from
		ClmDefaultValue d 
		inner join ClmTask t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join ModelData m on m.clmTaskId = t.clmTaskId
		inner join RunQueue q on q.modelDataId = m.modelDataId
		where q.runQueueOrder >= @startRunQueueOrder
	group by d.clmDefaultValueId, t.clmTaskStatusId, t.numberOfAminoAcids, t.maxPeptideLength
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
			when q.maxWeightedAverageAbsEe > @maxWeightedAverageAbsEe or q.maxLastEe > @maxLastEe then 1 
			else 0 
		end as isSymmetryBroken,
		cast(datediff(minute, isnull(q.startedOn, m.createdOn), q.modifiedOn) as float) / 1440.0 as runTime
	from
		RunQueue q
		inner join ModelData m on q.modelDataId = m.modelDataId
		inner join ClmTask t on m.clmTaskId = t.clmTaskId
		inner join ClmDefaultValue d on t.clmDefaultValueId = d.clmDefaultValueId
	where isnull(q.maxEe, 0) <= 1 and q.runQueueStatusId = 3 and q.runQueueOrder >= @startRunQueueOrder
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
		cast(isnull(count(*), 0) as float) as modelCount,
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
		cast(isnull(count(*), 0) as float) as symmBrokenCount
	from c
	where isSymmetryBroken = 1
	group by defaultSetIndex, numberOfAminoAcids, maxPeptideLength
),
f as
(
	select
		--a.numberOfAminoAcids,
		--a.maxPeptideLength,
		a.defaultSetIndex,
		a.clmTaskStatusId,
		isnull(d.modelCount, 0) as modelCount,
		a.running,
		a.progress / (case when a.running > 0 then a.running else 1 end) as progress,
		isnull(e.symmBrokenCount, 0) as symmBrokenCount,

		-- Not corrected.
		cast(isnull(cast(isnull(e.symmBrokenCount, 0) as float) / cast(d.modelCount as float), 0) as money) as symmBrokenPct,

		-- Corrected to account for a long running tail.
		cast(isnull(e.symmBrokenCount * (d.modelCount + @eps1 * a.running) / (d.modelCount * (d.modelCount + a.running)), 0) as money) as symmBrokenPctCorr1,
		cast(isnull(e.symmBrokenCount * (d.modelCount + @eps2 * a.running) / (d.modelCount * (d.modelCount + a.running)), 0) as money) as symmBrokenPctCorr2,

		--isnull(cast(dbo.getWasteRecyclingRate(a.defaultSetIndex) as nvarchar(20)), '') as wasteRecyclingRate,
		--isnull(cast(dbo.getCatSynthSim(a.defaultSetIndex) as nvarchar(20)), '') as catSynthSim,
		--isnull(cast(dbo.getCatSynthScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catSynthScarcity,
		--isnull(cast(dbo.getCatDestrSim(a.defaultSetIndex) as nvarchar(20)), '') as catDestrSim,
		--isnull(cast(dbo.getCatDestrScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catDestrScarcity,
		--isnull(cast(dbo.getLigFwd(a.defaultSetIndex) as nvarchar(20)), '') as ligFwd,
		--isnull(cast(dbo.getLigBkw(a.defaultSetIndex) as nvarchar(20)), '') as ligBkw,
		--isnull(cast(dbo.getCatLigScarcity(a.defaultSetIndex) as nvarchar(20)), '') as catLigScarcity,
		--isnull(cast(dbo.getCatLigMult(a.defaultSetIndex) as nvarchar(20)), '') as catLigMult,
		--isnull(cast(dbo.getCatLigSim(a.defaultSetIndex) as nvarchar(20)), '') as catLigSim,

		isnull(cast(dbo.getAcCatSynthScarcity(a.defaultSetIndex) as nvarchar(20)), '') as acCatSynthScarcity,
		isnull(cast(dbo.getAcCatSynthSimilarity(a.defaultSetIndex) as nvarchar(20)), '') as acCatSynthSimilarity,

		isnull(cast(dbo.getAcCatDestrScarcity(a.defaultSetIndex) as nvarchar(20)), '') as acCatDestrScarcity,
		isnull(cast(dbo.getAcCatDestrSimilarity(a.defaultSetIndex) as nvarchar(20)), '') as acCatDestrSimilarity,

		isnull(cast(dbo.getAcFwdCatLigScarcity(a.defaultSetIndex) as nvarchar(20)), '') as acFwdCatLigScarcity,
		isnull(cast(dbo.getAcFwdCatLigSimilarity(a.defaultSetIndex) as nvarchar(20)), '') as acFwdCatLigSimilarity,

		isnull(cast(dbo.getAcBkwCatLigScarcity(a.defaultSetIndex) as nvarchar(20)), '') as acBkwCatLigScarcity,
		isnull(cast(dbo.getAcBkwCatLigSimilarity(a.defaultSetIndex) as nvarchar(20)), '') as acBkwCatLigSimilarity,

		isnull(cast(dbo.getSugarForward(a.defaultSetIndex) as nvarchar(20)), '') as sugarForward,
		isnull(cast(dbo.getSugarBackward(a.defaultSetIndex) as nvarchar(20)), '') as sugarBackward

		--,isnull(cast(cast(d.runTime as decimal(10, 2)) as nvarchar(20)), '') as runTime
		--,cast(a.remainingRepetitions * isnull(d.runTime, @runeTimeEst) as decimal(10, 2)) as remainingRunTime
	from a

		left outer join d on a.defaultSetIndex = d.defaultSetIndex and a.numberOfAminoAcids = d.numberOfAminoAcids
		left outer join e on a.defaultSetIndex = e.defaultSetIndex and a.numberOfAminoAcids = e.numberOfAminoAcids
)

select 
	* 
from f
order by defaultSetIndex
