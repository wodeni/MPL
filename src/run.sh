set +e

fullname=$(basename "$1")
extension="${filename##*.}"
filename="${fullname%.*}"
llfilename="${filename}.ll"
sfilename="${filename}.s"

make clean && make 
./mpl.native < $1 > $llfilename
llc -relocation-model=pic $llfilename
# llc  $llfilename
# gcc -o main utils.o $sfilename
clang -o main utils.o $sfilename
./main

set -e
