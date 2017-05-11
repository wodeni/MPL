set +x
set -e

# Process file name and path
fullname=$(basename "$1")
extension="${filename##*.}"
filename="${fullname%.*}"
llfilename="${filename}.ll"
sfilename="${filename}.s"

make clean && make 
./mpl.native < $1 > $llfilename
llc -relocation-model=pic $llfilename
clang -o main utils.o $sfilename
./main

