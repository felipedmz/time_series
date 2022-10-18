-- init
create database wallmart;

-- carregando dados
create table raw_data (
    `Store` varchar(255) default null,
    `Date` varchar(255) default null,
    `Weekly_Sales` varchar(255) default null
);

LOAD DATA LOCAL INFILE "clean-walmart-sales-dataset-of-45stores.csv"
INTO TABLE raw_data
COLUMNS TERMINATED BY ';'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

-- convertendo valores
create table `data` (
    `store` int not null,
    `date` date not null,
    `sales` decimal(10, 2) not null
);

insert into `data` select
cast(`Store` as unsigned) as `store`,
str_to_date(`Date`, '%d/%m/%Y') as `date`,
cast(`Weekly_Sales` as DECIMAL(10, 2)) as `sales`
from raw_data;

-- agrupando vendas por mes 
select 
date_format(date, '%b') as month,
date_format(date, '%y') as year,
sum(sales) as monthly_sales
from `data`
group by 
date_format(date, '%b'),
date_format(date, '%y');

