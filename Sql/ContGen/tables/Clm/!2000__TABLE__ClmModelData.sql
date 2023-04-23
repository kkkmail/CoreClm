IF OBJECT_ID('[dbo].[ClmModelData]') IS NULL begin
	print 'Creating table [dbo].[ClmModelData] ...'

	CREATE TABLE [dbo].[ClmModelData](
		[clmModelDataId] [uniqueidentifier] NOT NULL,
		[clmModelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ClmModelData] PRIMARY KEY CLUSTERED 
	(
		[clmModelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ClmModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [dbo].[ClmModelData]  WITH CHECK ADD  CONSTRAINT [FK_ClmModelData_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].[ClmTask] ([clmTaskId])
	ALTER TABLE [dbo].[ClmModelData] CHECK CONSTRAINT [FK_ClmModelData_ClmTask]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ClmModelData] ON [dbo].[ClmModelData]
	(
		[clmModelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmModelData] already exists ...'
end
go




