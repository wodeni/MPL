set +x
set -e

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
# Combine the input and output to the same image and display it
convert lena.pgm lena-out.pgm +append combined.pgm
display combined.pgm

