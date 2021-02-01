#!/bin/sh
# Ryan Mulhall 1/2020
# Creates input files for mpp testing

[[ "$1" = "nml" ]] && cat <<_EOF > input_base.nml
 
_EOF

if [[ "$1" = "ascii" ]]; then

cat <<_EOF > ascii_5
"this is an ascii file with 5 lines"
"it will contain commas inside quotes", "as well as outside quotes"
"it will not have the same number of fields on every line"
some lines will not have quotes
"there might be a line with the character string \n"
_EOF

cat <<_EOF > ascii_long
"this is an ascii file with 5 lines"
""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes""it will contain commas inside quotes", "as well as outside quotes"it will contain commas inside quotes", "as well as outside quotes"
"it will not have the same number of fields on every line"
some lines will not have quotes
"there might be a line with the character string \n"
_EOF

cat <<_EOF > ascii_skip
"this is an ascii file with 5 lines"
"it will contain commas inside quotes", "as well as outside quotes"
"it will not have the same number of fields on every line"

"there might be a blank line beforehand"
_EOF

touch ascii_25
for i in 1 2 3 4 5
do
cat ascii_5 >> ascii_25
done

echo "" > ascii_0

fi
