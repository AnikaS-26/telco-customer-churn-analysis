SELECT 1;
SELECT * FROM demographics LIMIT 5;
SELECT * FROM location LIMIT 5;
SELECT * FROM population LIMIT 5;
SELECT * FROM services LIMIT 5;
SELECT * FROM status LIMIT 5;

DROP VIEW IF EXISTS telco_customers;

ALTER TABLE services
RENAME COLUMN Custome_ID TO Customer_ID;


CREATE VIEW telco_customers AS
SELECT
    s.Customer_ID                AS Customer_ID,
    s.Churn_Label                AS churn_label,
    CASE WHEN s.Churn_Label = 'Yes' THEN 1 ELSE 0 END AS churn_flag,
    s.Customer_Status            AS customer_status,
    s.Satisfaction_Score         AS satisfaction_score,
    s.CLTV                       AS cltv,

    sv.Tenure_in_Months          AS tenure_months,
    sv.Monthly_Charge            AS monthly_charge,
    sv.Total_Charges             AS total_charges,
    sv.Phone_Service             AS phone_service,
    sv.Internet_Service          AS internet_service,
    sv.Streaming_TV              AS streaming_tv,
    sv.Streaming_Movies          AS streaming_movies,

    d.Gender                     AS gender,
    d.Age                        AS age,
    d.Under_30                   AS under_30,
    d.Senior_Citizen             AS senior_citizen,
    d.Married                    AS married,
    d.Dependents                 AS dependents,
    d.Number_of_Dependents       AS num_dependents,

    l.Country                    AS country,
    l.State                      AS state,
    l.City                       AS city,
    l.Zip_Code                   AS zip_code

FROM status      s
LEFT JOIN demographics d ON s.Customer_ID = d.Customer_ID
LEFT JOIN location     l ON s.Customer_ID = l.Customer_ID
LEFT JOIN services     sv ON s.Customer_ID = sv.Customer_ID;


SELECT * FROM telco_customers;

