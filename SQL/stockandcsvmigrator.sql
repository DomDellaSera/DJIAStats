USE djiadb;
#DROP table IF EXISTS wikiviews;
CREATE TABLE IF NOT EXISTS wikiviews (
	id int(11) NOT NULL AUTO_INCREMENT,
    `number` int(11),
    `date`date ,
    views int(5),
    firm char(45),
    stocktickers char(5),
    `Open` FLOAT, 
    High FLOAT,
    Low FLOAT,
    `Close` FLOAT,
    Volume int(11),
    Adjusted FLOAT,
    primary key (`id`)
    );

load data local infile '/Users/Dominic/Documents/Programming/bigwikiviewstabletobeexported.csv' into table wikiviews fields terminated by ','
enclosed by '"'
lines terminated by '\n'
(`number`,`date`, views, firm, stocktickers, `open`, high, low, `close`, volume, adjusted);
