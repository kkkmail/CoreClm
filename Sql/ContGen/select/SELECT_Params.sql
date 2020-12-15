declare @clmDefaultValueId bigint
set @clmDefaultValueId = 8000

select 
	dbo.getWasteRecyclingRate(@clmDefaultValueId) as wasteRecyclingRate,
	dbo.getCatSynthSim(@clmDefaultValueId) as catSynthSim,
	dbo.getCatSynthScarcity(@clmDefaultValueId) as catSynthScarcity,
	dbo.getCatDestrSim(@clmDefaultValueId) as catDestrSim,
	dbo.getCatDestrScarcity(@clmDefaultValueId) as catDestrScarcity


