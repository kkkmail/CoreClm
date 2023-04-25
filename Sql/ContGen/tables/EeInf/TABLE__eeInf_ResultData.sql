IF OBJECT_ID('[eeInf].[ResultData]') IS NULL begin
	print 'Creating table [eeInf].[ResultData] ...'

	CREATE TABLE [eeInf].[ResultData](
		[resultDataId] [uniqueidentifier] NOT NULL, -- resultId is always runQueueId
		[resultDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[modelDataId] [uniqueidentifier] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_eeInf_ResultData] PRIMARY KEY CLUSTERED 
	(
		[resultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [eeInf].[ResultData] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [eeInf].[ResultData] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [eeInf].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_ResultlData_ModelData] FOREIGN KEY([modelDataId])
	REFERENCES [eeInf].[ModelData] ([eeInfModelDataId])
	ALTER TABLE [eeInf].[ResultData] CHECK CONSTRAINT [FK_eeInf_ResultlData_ModelData]

	ALTER TABLE [eeInf].[ResultData]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_ResultData_RunQueue] FOREIGN KEY([resultDataId])
	REFERENCES [eeInf].[RunQueue] ([runQueueId])
	ALTER TABLE [eeInf].[ResultData] CHECK CONSTRAINT [FK_eeInf_ResultData_RunQueue]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_eeInf_ResultData] ON [eeInf].[ResultData]
	(
		[resultDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [eeInf].[ResultData] already exists ...'
end
go




