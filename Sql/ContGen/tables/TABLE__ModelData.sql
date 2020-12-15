IF OBJECT_ID('[dbo].[ModelData]') IS NULL begin
	print 'Creating table [dbo].[ModelData] ...'

	CREATE TABLE [dbo].[ModelData](
		[modelDataId] [uniqueidentifier] NOT NULL,
		[modelDataOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[fileStructureVersion] [money] NOT NULL,
		[seedValue] [int] NULL,
		[modelDataParams] [nvarchar](max) NOT NULL,
		[modelBinaryData] [varbinary](max) NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ModelData] PRIMARY KEY CLUSTERED 
	(
		[modelDataId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

	ALTER TABLE [dbo].[ModelData] ADD  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [dbo].[ModelData]  WITH CHECK ADD  CONSTRAINT [FK_ModelData_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].[ClmTask] ([clmTaskId])

	ALTER TABLE [dbo].[ModelData] CHECK CONSTRAINT [FK_ModelData_ClmTask]

	CREATE UNIQUE NONCLUSTERED INDEX [UX_ModelData] ON [dbo].[ModelData]
	(
		[modelDataOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ModelData] already exists ...'
end
go




