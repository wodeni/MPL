#!/bin/bash -x


pause() {
    echo -e "\e[33m"
    read -n1 -p "$*"
    echo -e "\e[0m"
}


echo -e " \e[31m===================MPL Hello World Demo===================\e[0m"


pause "\e[20mPress enter to continue"
cd src/
make clean && make 2> /dev/null
pause "Press enter to continue"
./mpl.native < test-prints.mpl > hello.ll
pause "Press enter to continue"
cat hello.ll
pause "Press enter to continue"
 lli hello.ll
pause "Press enter to continue"

cd -

echo -e " \e[31m===================End of Demo===================\e[0m"
