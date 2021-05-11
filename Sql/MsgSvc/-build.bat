md .\!All
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b .\tables\*.sql + .\data\*.sql .\!All\all.sql
