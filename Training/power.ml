let rec power a b = 
if b = 1 then a else a * (power a (b-1));;

print_string (string_of_int (power 2 3));
print_string "\n"
