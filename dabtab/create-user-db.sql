DROP DATABASE IF EXISTS dabtab_db;   
CREATE DATABASE IF NOT EXISTS dabtab_db;   
USE dabtab_db; 

DROP TABLE IF EXISTS user; 

CREATE TABLE IF NOT EXISTS user 
  ( 
     id         INT PRIMARY KEY auto_increment, 
     username   VARCHAR(25) UNIQUE NOT NULL, 
     password   CHAR(60) NOT NULL, 
     first_name VARCHAR(50) NOT NULL, 
     last_name  VARCHAR(50) NOT NULL, 
     email      VARCHAR(100) UNIQUE NOT NULL, 
     role       ENUM('Admin', 'SuperUser', 'AdvancedUser', 'BasicUser')
  ); 