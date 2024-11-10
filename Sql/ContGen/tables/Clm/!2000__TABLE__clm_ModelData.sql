IF OBJECT_ID('[clm].[ModelData]') IS NULL begin
	print 'Creating table [clm].[ModelData] ...'

	CREATE TABLE [clm].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[modelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[taskId] [uniqueidentifier] NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_clm_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [clm].[ModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [clm].[ModelData]  WITH CHECK ADD  CONSTRAINT [FK_clm_ModelData_Task] FOREIGN KEY([taskId])
	REFERENCES [clm].[Task] ([taskId])
	ALTER TABLE [clm].[ModelData] CHECK CONSTRAINT [FK_clm_ModelData_Task]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_clm_ModelData] ON [clm].[ModelData]
	(
		[modelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [clm].[ModelData] already exists ...'
end
go




