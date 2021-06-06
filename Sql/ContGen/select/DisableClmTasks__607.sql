select
	   [clmTaskId]
      ,[clmTaskOrder]
      ,[clmDefaultValueId]
      ,[clmTaskStatusId]
      ,[numberOfAminoAcids]
      ,[maxPeptideLength]
      ,[numberOfRepetitions]
      ,[remainingRepetitions]
      ,[createdOn]
      ,[modifiedOn]
from [clm607].[dbo].[ClmTask]
--where clmDefaultValueId = 4005000016
--where clmDefaultValueId = 4005000067
order by [clmTaskOrder]

-- Suspended temporarily to clarify other data points. Turn back on if needed.
-- These one should be turned on FIRST (if needed).
-- update [clm607].[dbo].[ClmTask] set [clmTaskStatusId] = 1 where [clmDefaultValueId] in (4005000005, 4005000008, 4005000011, 4005000012, 4005000005, 4005000009, 4005000005, 4005000010)


-- Suspended temporarily to clarify other data points. Turn back on if needed. 
-- update [clm607].[dbo].[ClmTask] set [clmTaskStatusId] = 1 where [clmDefaultValueId] in (4005000022, 4005000032, 4005000021, 4005000020, 4005000014, 4005000013, 4005000005, 4005000007, 4005000044, 4005000045, 4005000058, 4005000057, 4005000063, 4005000061, 1000016002, 1000018001)


-- Disable tasks, which are no longer needed or can wait.
-- update [clm607].[dbo].[ClmTask] set [clmTaskStatusId] = 1 where [clmDefaultValueId] in (4005000040, 4005000041, 4005000042, 4005000047, 4005000048, 4005000059, 4005000060, 4005000064, 4005000065, 4005000066, 4005000031, 4005000033, 4005000037, 4005000043)


-- update [clm607].[dbo].[ClmTask] set [clmTaskStatusId] = 1 where [clmTaskId] = 'D0E33520-096D-453C-AFBA-94B614EF0270'
-- update [clm607].[dbo].[ClmTask] set [clmTaskStatusId] = 0 where [clmTaskId] <> 'D0E33520-096D-453C-AFBA-94B614EF0270'


-- Disable ALL.
-- update [clm607].[dbo].[ClmTask] set [clmTaskStatusId] = 1
