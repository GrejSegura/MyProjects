
THIS IS THE INSTRUCTION TO EXECUTE THE TASK FOR INGO AUTOMATION


1. download the data for acquisitions, advocates and DBMS.

2. rename the columns for the respective files as follows:
	a. acquisitions/advocates file
		First Name -> name
		Surname -> surname
		Company -> company
		Position -> position
		E-mail -> email
	b. DBMS file
		Card.Number -> id
		First Name -> name
		Surname -> surname
		Company -> company
		Position -> position
		E-mail -> email

2. save the data as follows: acquisitions.csv , advocates.csv, and dbms.csv

3. Clickthe Ingo Report Generate file
	NOTE: make sure to install.packages the following in R before running.
		tidyverse, data.table

4. Output is saved as ingo_matching_final.csv in the /dta folder.