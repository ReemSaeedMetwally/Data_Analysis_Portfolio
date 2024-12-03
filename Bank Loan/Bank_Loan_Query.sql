-- Total loan applications
select count(id) as Total_Loan_Applications 
from bank_loan_data


-- Total funded amount in 2021 year 
select sum(loan_amount) Total_Loan_Amount 
from bank_loan_data


-- Total amount received in 2021 year 
select sum(total_payment) Total_Amount_Recieved from bank_loan_data


-- Average interest rate
select round(avg(int_rate),4) * 100 as "Avg_Interest_Rate %"
from bank_loan_data


-- Average debt to income
select round(avg(dti),4) * 100 as "Avg_Debt_To_Income %"
from bank_loan_data


-- Good loan percentage
select (count(case when loan_status = 'Fully Paid' OR loan_status = 'Current' 
                   then id END) * 100) / count(id) as Good_Loan_Percentage
from bank_loan_data


-- Good loan applications
select count(id) as Good_Loan_Applications
from bank_loan_data
where loan_status = 'Fully Paid' OR loan_status = 'Current' 


-- Good loan funded amount
select sum(loan_amount) as Good_Loan_Funded_Amount
from bank_loan_data
where loan_status = 'Fully Paid' OR loan_status = 'Current' 


-- Good loan total received amount
select sum(total_payment) as Good_Loan_Recieved_Amount
from bank_loan_data
where loan_status = 'Fully Paid' OR loan_status = 'Current'
 

-- Bad loan percentage
select (count(case when loan_status = 'Charged Off' 
                   then id END) * 100)/ count(id) as Bad_Loan_Percentage
from bank_loan_data


-- Bad loan applications
select count(id) as Bad_Loan_Applications
from bank_loan_data
where loan_status = 'Charged Off'


-- Bad loan funded amount
select sum(loan_amount) as Bad_Loan_Funded_Amount
from bank_loan_data
where loan_status = 'Charged Off'


-- Bad loan total received amount
select sum(total_payment) as Bad_Loan_Recieved_Amount
from bank_loan_data
where loan_status = 'Charged Off'


-- Bad loan total not received amount
select (sum(loan_amount) - sum(total_payment)) as Bad_Loan_Not_Recieved_Amount
from bank_loan_data
where loan_status = 'Charged Off'


-- Loan status review
select loan_status, 
       count(id) as Total_loan_applications,
       sum(loan_amount) as Funded_amount, 
       sum(total_payment) as Recieved_amount,
       avg(int_rate * 100) as 'Avg_intrest_rate %', 
       avg(dti * 100) as 'Avg_debt_to_loan %'
from bank_loan_data
group by loan_status


-- Monthly trends by issue date
select Datename("month", issue_date) as Issue_month, 
       count(id) as loan_applications, sum(loan_amount) as Funded_amount,
       sum(total_payment) as Recieved_amount
from bank_loan_data
group by Datename("month", issue_date)
order by count(id);


-- Loans according to state
select address_state as Address_state, 
       count(id) as loan_applications,
       sum(loan_amount) as Funded_amount, 
       sum(total_payment) as Recieved_amount
from bank_loan_data
group by address_state
order by count(id) Desc;


-- Loan term analysis
select term as Loan_term, 
       count(id) as loan_applications, 
       sum(loan_amount) as Funded_amount, 
       sum(total_payment) as Recieved_amount
from bank_loan_data
group by term


-- Employee length analysis
select emp_length as Employee_length, 
       count(id) as loan_applications,
       sum(loan_amount) as Funded_amount, 
       sum(total_payment) as Recieved_amount
from bank_loan_data
group by emp_length 
order by loan_applications desc


-- Loan purpose analysis
select purpose as Loan_purpose, 
       count(id) as loan_applications,
       sum(loan_amount) as Funded_amount, 
       sum(total_payment) as Recieved_amount
from bank_loan_data
group by purpose 
order by loan_applications desc 


-- Home ownership analysis
select home_ownership as Home_ownership, 
       count(id) as loan_applications,
       sum(loan_amount) as Funded_amount, 
       sum(total_payment) as Recieved_amount
from bank_loan_data
group by home_ownership
order by loan_applications desc


-- What is the avg int rate for each month
select issue_month, 
       round(avg(int_rate),4) * 100 as "Avg_Interest_Rate %"
from(select Datetrunc("month", issue_date) as issue_month, 
            int_rate
     from bank_loan_data) as tab1
group by issue_month
order by 1;


-- What is the average debt to income for each month
select issue_month, 
       round(avg(dti),4) * 100 as "Avg_Debt_To_Income %"
from(select Datetrunc("month", issue_date) as issue_month, 
            dti
     from bank_loan_data) as tab1
group by issue_month
order by 1;

