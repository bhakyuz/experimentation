import os

print('In `settings.py` MY_VAR = ' + str(os.getenv('MY_VAR')))

def my_var_via_function():
	return(os.getenv('MY_VAR'))

my_var_as_constant = os.getenv('MY_VAR')
