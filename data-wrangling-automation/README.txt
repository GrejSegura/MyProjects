This is the automation process for the onsite team.

NOTES:
1. This execution time may be longer on the first run. This is because R will install all the dependencies on the first run.
2. To make this program work, all data input should be in a particular data structure below:

	column 1 - id		--- this is the visitor card number
	column 2 - session	--- this is the session code
	column 3 - date		--- date of format dd/mm/yy
	column 4 - time		--- time of format hh:mm:ss
	column 5 - type_of_log	--- this indicates the type of time log, i.e. "IN" or "OUT"


TO RUN THE PROGRAM

1. use the "main.R" script to execute.
2. a dialog box will prompt - enter the file location of this folder together with this folder's name
	example: C:/Users/Grejell/Documents/onsiteManagersProject/
3. the 2nd dialog box will ask for the input data, click the data and wait for the output. NOTE: input data should be named "input_data.csv"
4. the output data will be saved in the output folder as "output_data.xlsx".

