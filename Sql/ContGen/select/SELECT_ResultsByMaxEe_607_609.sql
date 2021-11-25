-- Calculates how often the symmetry is broken for all processed default sets across clm607 and clm609 databases.

-- Must use clm609 database as only it has all the new default values.
use clm609
go

------------------------------------
------------------------------------

declare @startRunQueueOrder607 bigint
set @startRunQueueOrder607 = 21

declare @eps1 float, @eps2 float
set @eps1 = 0.50
set @eps2 = 0.25

declare @maxWeightedAverageAbsEe float, @maxLastEe float, @runeTimeEst float
set @maxLastEe = 0.10
set @maxWeightedAverageAbsEe = @maxLastEe
set @runeTimeEst = 1.60

; with
q as
(
	select * from clm607.dbo.RunQueue where runQueueOrder >= @startRunQueueOrder607
	union all
	select * from clm609.dbo.RunQueue
),
d as
(
	select * from clm609.dbo.ClmDefaultValue
),
t as
(
	select clmTaskId, clmDefaultValueId, clmTaskStatusId, numberOfAminoAcids, maxPeptideLength from clm607.dbo.ClmTask
	union all
	select clmTaskId, clmDefaultValueId, clmTaskStatusId, numberOfAminoAcids, maxPeptideLength from clm609.dbo.ClmTask
),
m as
(
	select modelDataId, clmTaskId, createdOn from clm607.dbo.ModelData
	union all
	select modelDataId, clmTaskId, createdOn from clm609.dbo.ModelData
),
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
		d 
		inner join t on d.clmDefaultValueId = t.clmDefaultValueId
		inner join m on m.clmTaskId = t.clmTaskId
		inner join q on q.modelDataId = m.modelDataId
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
		q
		inner join m on q.modelDataId = m.modelDataId
		inner join t on m.clmTaskId = t.clmTaskId
		inner join d on t.clmDefaultValueId = d.clmDefaultValueId
	where isnull(q.maxEe, 0) <= 1 and q.runQueueStatusId = 3
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
e as
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
f as
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
g as
(
	select
		--a.numberOfAminoAcids,
		--a.maxPeptideLength,
		a.defaultSetIndex,
		a.clmTaskStatusId,
		isnull(e.modelCount, 0) as modelCount,
		a.running,
		a.progress / (case when a.running > 0 then a.running else 1 end) as progress,
		isnull(f.symmBrokenCount, 0) as symmBrokenCount,

		-- Not corrected.
		cast(isnull(cast(isnull(f.symmBrokenCount, 0) as float) / cast(e.modelCount as float), 0) as money) as symmBrokenPct,

		-- Corrected to account for a long running tail.
		cast(isnull(f.symmBrokenCount * (e.modelCount + @eps1 * a.running) / (e.modelCount * (e.modelCount + a.running)), 0) as money) as symmBrokenPctCorr1,
		cast(isnull(f.symmBrokenCount * (e.modelCount + @eps2 * a.running) / (e.modelCount * (e.modelCount + a.running)), 0) as money) as symmBrokenPctCorr2,

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
		isnull(cast(dbo.getSugarBackward(a.defaultSetIndex) as nvarchar(20)), '') as sugarBackward,
		isnull(cast(dbo.getSugarScarcity(a.defaultSetIndex) as nvarchar(20)), '') as sugarScarcity

		--,isnull(cast(cast(e.runTime as decimal(10, 2)) as nvarchar(20)), '') as runTime
		--,cast(a.remainingRepetitions * isnull(e.runTime, @runeTimeEst) as decimal(10, 2)) as remainingRunTime
	from a
		left outer join e on a.defaultSetIndex = e.defaultSetIndex and a.numberOfAminoAcids = e.numberOfAminoAcids
		left outer join f on a.defaultSetIndex = f.defaultSetIndex and a.numberOfAminoAcids = f.numberOfAminoAcids
)

select 
	* 
from g
order by defaultSetIndex

