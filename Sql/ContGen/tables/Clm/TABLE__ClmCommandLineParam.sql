IF OBJECT_ID('[dbo].[ClmCommandLineParam]') IS NULL begin
	print 'Creating table [dbo].[ClmCommandLineParam] ...'

	CREATE TABLE [dbo].[ClmCommandLineParam](
		[clmCommandLineParamId] [uniqueidentifier] NOT NULL,
		[clmCommandLineParamOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_ClmCommandLineParam] PRIMARY KEY CLUSTERED 
	(
		[clmCommandLineParamId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[ClmCommandLineParam] ADD DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[ClmCommandLineParam] ADD DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [dbo].[ClmCommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_ClmCommandLineParam_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].[ClmTask] ([clmTaskId])

	ALTER TABLE [dbo].[ClmCommandLineParam] CHECK CONSTRAINT [FK_ClmCommandLineParam_ClmTask]

	CREATE NONCLUSTERED INDEX [UX_ClmCommandLineParam] ON [dbo].[ClmCommandLineParam]
	(
		[clmCommandLineParamOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[ClmCommandLineParam] already exists ...'
end
go



