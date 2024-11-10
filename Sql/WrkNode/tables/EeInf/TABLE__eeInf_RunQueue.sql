IF OBJECT_ID('[eeInf].[RunQueue]') IS NULL begin
	print 'Creating table [eeInf].[RunQueue] ...'

	CREATE TABLE [eeInf].[RunQueue](
		[runQueueId] [uniqueidentifier] NOT NULL,
		[dummy] [float] NOT NULL,
	 CONSTRAINT [PK_eeInf_RunQueue] PRIMARY KEY CLUSTERED 
	(
		[runQueueId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [eeInf].[RunQueue] ADD DEFAULT ((0)) FOR [dummy]

	ALTER TABLE [eeInf].[RunQueue]  WITH CHECK ADD  CONSTRAINT [FK_eeInf_RunQueue_RunQueue] FOREIGN KEY([runQueueId])
	REFERENCES [dbo].[RunQueue] ([runQueueId])
	ALTER TABLE [eeInf].[RunQueue] CHECK CONSTRAINT [FK_eeInf_RunQueue_RunQueue]

end else begin
	print 'Table [eeInf].[RunQueue] already exists ...'
end
go



