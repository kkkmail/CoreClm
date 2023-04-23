IF OBJECT_ID('[dbo].[EeInfRunQueue]') IS NULL begin
	print 'Creating table [dbo].[EeInfRunQueue] ...'

	CREATE TABLE [dbo].[EeInfRunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[eeInfModelDataId] [uniqueidentifier] NOT NULL,
		[tEnd] [money] NOT NULL,
	 CONSTRAINT [PK_EeInfRunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[EeInfRunQueue]  WITH CHECK ADD  CONSTRAINT [FK_EeInfRunQueue_RunQueue] FOREIGN KEY([runQueueId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [dbo].[EeInfRunQueue] CHECK CONSTRAINT [FK_EeInfRunQueue_RunQueue]

	ALTER TABLE [dbo].[EeInfRunQueue]  WITH CHECK ADD  CONSTRAINT [FK_EeInfRunQueue_EeInfModelData] FOREIGN KEY([eeInfModelDataId])
	REFERENCES [dbo].[EeInfModelData] ([eeInfModelDataId])
	ALTER TABLE [dbo].[EeInfRunQueue] CHECK CONSTRAINT [FK_EeInfRunQueue_EeInfModelData]

end else begin
	print 'Table [dbo].[EeInfRunQueue] already exists ...'
end
go



