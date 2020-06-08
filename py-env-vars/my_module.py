import os
from settings import my_var_via_function, my_var_as_constant


print('In `my_module.py` MY_VAR = ' + str(os.getenv('MY_VAR')))

# Let set a specific value for this env var
# Imagine it is Airflow setting this for you
os.environ['MY_VAR'] = 'Some value'	
print('Now (we just set it), in `my_module.py` MY_VAR = ' + str(os.getenv('MY_VAR')))

my_var_imported_via_function = my_var_via_function()
my_var_imported_as_constant = my_var_as_constant


print('my_var_imported_via_function = ' + str(my_var_imported_via_function))
print('my_var_imported_as_constant = ' + str(my_var_imported_as_constant))


