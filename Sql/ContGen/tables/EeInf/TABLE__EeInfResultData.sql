IF OBJECT_ID('[dbo].[EeInfResultData]') IS NULL begin
	print 'Creating table [dbo].[EeInfResultData] ...'

	CREATE TABLE [dbo].[EeInfResultData](
		[eeInfResultDataId] [uniqueidentifier] NOT NULL, -- resultId is always runQueueId
		[eeInfResultDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[eeInfModelDataId] [uniqueidentifier] NOT NULL,
		[createdOn] [datetime] NOT NULL,
		[modifiedOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_EeInfResultData] PRIMARY KEY CLUSTERED 
	(
		[eeInfResultDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[EeInfResultData] ADD  DEFAULT (getdate()) FOR [createdOn]
	ALTER TABLE [dbo].[EeInfResultData] ADD  DEFAULT (getdate()) FOR [modifiedOn]

	ALTER TABLE [dbo].[EeInfResultData]  WITH CHECK ADD  CONSTRAINT [FK_EeInfResultlData_EeInfModelData] FOREIGN KEY([eeInfModelDataId])
	REFERENCES [dbo].[EeInfModelData] ([eeInfModelDataId])
	ALTER TABLE [dbo].[EeInfResultData] CHECK CONSTRAINT [FK_EeInfResultlData_EeInfModelData]

	ALTER TABLE [dbo].[EeInfResultData]  WITH CHECK ADD  CONSTRAINT [FK_EeInfResultData_RunQueue] FOREIGN KEY([eeInfResultDataId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [dbo].[EeInfResultData] CHECK CONSTRAINT [FK_EeInfResultData_RunQueue]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ResultData] ON [dbo].[EeInfResultData]
	(
		[eeInfResultDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

end else begin
	print 'Table [dbo].[EeInfResultData] already exists ...'
end
go




