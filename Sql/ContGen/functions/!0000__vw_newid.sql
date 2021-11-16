drop view if exists vw_newid
go


create view vw_newid
as
select newid() as new_id
go

