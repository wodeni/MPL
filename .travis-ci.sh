# OPAM version to install
export OPAM_VERSION=1.2.2

# install ocaml from apt
sudo add-apt-repository ppa:avsm/ocaml42+opam120 -y
sudo apt-get update
sudo apt-get install -y ocaml m4 llvm opam
opam init -y
opam install llvm.3.4 ocamlfind -y
eval `opam config env`


# compile & run tests (here assuming OASIS DevFiles)
cd src
make clean
make
./testall.sh
