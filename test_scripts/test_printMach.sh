# Get current pwd and store as curpwd
ROOT_PWD=$(pwd)
cd $ROOT_PWD/CompCert
make -j $(nproc) all
cd $ROOT_PWD
./CompCert/ccomp -dmach test_scripts/sample.c
