-- Covid-19 data analyses

-- Check whether all tables have the same number of rows
select count(*) cases_count from cases;
select count(*) deaths_count from deaths;
select count(*) society_count from society_coef;
select count(*) vaccinations_count from vaccinations;

-- ---------------------------------------------------------------
-- Check cases data in general
select * from cases c;
select location, max(total_cases) from cases c
	where continent != ''
	group by 1
	order by 2 desc;

select location, max(total_cases_per_million) from cases c
	where continent != ''
	group by 1
	order by 2 desc; 

select location, max(cast(total_cases_per_million as integer)) from cases c
	where continent != ''
	group by 1
	order by 2 desc; 
-- ---------------------------------------------------------------

-- Total cases in continents
select distinct(continent) from cases c;
select distinct(location) from cases c;
-- select distinct(iso_code) from cases c 
--  	where continent = "";

select sum(new_cases) from cases c
	where continent != '';
select sum(new_cases) from cases c 
	where location = "World"

select continent, sum(new_cases) from cases c
	where continent != ''
	group by continent 
	order by 2 desc;

-- ---------------------------------------------------------------
-- Checking max number of cases in specific country (Poland)
select date, new_cases from cases c 
	where location = 'Poland'
	order by 2 desc 
	limit 5;
-- Checking number of cases in specific country (Poland) in last 10 days
select date, new_cases from cases c 
	where location = 'Poland'
	order by 1 desc 
	limit 10;


-- ---------------------------------------------------------------
-- Checking deaths data in World
select * from deaths d ;
select location, max(total_deaths) from deaths d 
	where continent != ''
	group by 1
	order by 2 desc;

select location, max(total_deaths_per_million) from deaths d 
	where continent != ''
	group by 1
	order by 2 desc; 

select d.location, max(d.total_deaths), max(c.total_cases) tot_cases, max(d.total_deaths)/max(c.total_cases)*100 death_percentage 
	from deaths d 
	join cases c 
	on d.id = c.id
	where d.continent = 'Europe'
	group by d.location
	having tot_cases > 10000
	order by 4 desc;

select iso_code ,location, max(total_deaths_per_million), max(total_deaths)
	from deaths d 
	where iso_code like "OWID%" and continent ='' and location in ('Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America')
	group by iso_code
	order by 3 desc;

-- ---------------------------------------------------------------
-- Checking deaths data for specific country (Poland)
select * from deaths d 
	where location = 'Poland';

-- Checking number of deaths in specific country (Poland) in worst 5 days vs last 5 days
select date, new_deaths from deaths d 
	where location = 'Poland'
	order by 2 desc 
	limit 5;
select date, new_deaths from deaths d 
	where location = 'Poland' 
	order by date desc
	limit 5;
-- Checking fatality rate in Poland
select d.date, d.total_deaths, c.total_cases, d.total_deaths/c.total_cases*100 as death_percentage
	from deaths d 
	join cases c 
	on d.id = c.id
	where d.location = 'Poland';

-- Checking days that fit red, orange and green zones of epidemic restrictions in Poland
select c.`date`, round(c.new_cases_smoothed), d.new_deaths_smoothed , c.new_cases_smoothed_per_million, d.new_deaths_smoothed_per_million, c.hosp_patients_per_million, 
	case 
		when c.new_cases_smoothed_per_million < 60 then 'green'
		when c.new_cases_smoothed_per_million < 120 then 'orange'
		else 'red'
	end as 'zone'
	from cases c 
	join deaths d 
	on c.id = d.id 
	where c.location = 'Poland';

-- ---------------------------------------------------------------
-- Vaccination in world
select location, max(total_vaccinations) from vaccinations v 
	where continent != ''
	group by location 
	order by 2 desc;
-- People fully vaccinated per 100 in continents
select iso_code ,location, max(people_fully_vaccinated_per_hundred) from vaccinations v 
	where iso_code like "OWID%" and continent ='' and location in ('Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America')
	group by iso_code
	order by 3 desc;

select location, round(max(total_vaccinations_per_hundred)) tot_vacc_per_100 from vaccinations v 
	where continent != ''
	group by location 
	order by 2 desc;

select location, round(max(people_fully_vaccinated_per_hundred), 1) peop_vacc_per_100, round(max(total_boosters_per_hundred),1) boost_per_100
	from vaccinations v 
	where continent != ''
	group by location 
	order by 2 desc;

-- People fully vaccinated vs GDP per capita vs HDI
select v.location, round(max(people_fully_vaccinated_per_hundred), 1) peop_vacc_per_100, sd.gdp_per_capita, sd.human_development_index 
	from vaccinations v 
	join society_data sd 
	on v.id = sd.id 
	where v.continent != ''
	group by v.location 
	order by 2 desc;
