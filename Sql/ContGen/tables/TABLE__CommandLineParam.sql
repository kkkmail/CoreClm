IF OBJECT_ID('[dbo].[CommandLineParam]') IS NULL begin
	print 'Creating table [dbo].[CommandLineParam] ...'

	CREATE TABLE [dbo].[CommandLineParam](
		[commandLineParamId] [uniqueidentifier] NOT NULL,
		[commandLineParamOrder] [bigint] IDENTITY(1,1) NOT NULL,
		[clmTaskId] [uniqueidentifier] NOT NULL,
		[y0] [money] NOT NULL,
		[tEnd] [money] NOT NULL,
		[useAbundant] [bit] NOT NULL,
		[createdOn] [datetime] NOT NULL,
	 CONSTRAINT [PK_TCommandLineParam] PRIMARY KEY CLUSTERED 
	(
		[commandLineParamId] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
	) ON [PRIMARY]

	ALTER TABLE [dbo].[CommandLineParam] ADD  CONSTRAINT [DF__CommandLi__useAb__403A8C7D]  DEFAULT ((0)) FOR [useAbundant]
	ALTER TABLE [dbo].[CommandLineParam] ADD  CONSTRAINT [DF__CommandLi__creat__412EB0B6]  DEFAULT (getdate()) FOR [createdOn]

	ALTER TABLE [dbo].[CommandLineParam]  WITH CHECK ADD  CONSTRAINT [FK_CommandLineParam_ClmTask] FOREIGN KEY([clmTaskId])
	REFERENCES [dbo].[ClmTask] ([clmTaskId])

	ALTER TABLE [dbo].[CommandLineParam] CHECK CONSTRAINT [FK_CommandLineParam_ClmTask]

	CREATE NONCLUSTERED INDEX [UX_CommandLineParam] ON [dbo].[CommandLineParam]
	(
		[commandLineParamOrder] ASC
	)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
end else begin
	print 'Table [dbo].[CommandLineParam] already exists ...'
end
go



