./polygo.native < ./test.pg > test.ll && /usr/local/opt/llvm38/bin/lli-3.8 test.ll > test.out