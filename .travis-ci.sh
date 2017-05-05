# OPAM version to install
export OPAM_VERSION=1.2.2

# install ocaml from apt
sudo apt-get update
sudo apt-get install -y ocaml m4 llvm opam
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`

# compile & run tests (here assuming OASIS DevFiles)
cd src
make clean
make
./testall.sh
