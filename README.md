
# ddisasm-ascent

To create a dataset from a binary, you can use the following command :
```bash
ddisasm -I --debug-dir /tmp </path/to/binary> # wait this this command to finish or throw an error
cp /tmp/disassembly/binary.gtirb <dataset_dir>
ddisasm --asm=</path/to/asm> -j 12 --debug-dir <dataset_dir> </path/to/binary>
```

To run souffle on the dataset, you can use the following command :
```bash
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}`pwd`
souffle -L /usr/local/src/ddisasm/build/lib/ -j 12 -o ./extracted.bin ./ddisasm.dl
DDISASM_DEBUG_DIR=</path/to/db>/disassembly DDISASM_GTIRB_MODULE_NAME=<binary> /usr/bin/time -v ./extracted.bin -j 16  -F </path/to/db>/disassembly -D </path/to/result>
```

To run this artifacts you can use :
```bash
cargo build --release
DDISASM_GTIRB_MODULE_NAME=<binary> DDISASM_DEBUG_DIR=./data/disassembly/ LD_LIBRARY_PATH=/usr/local/src/ddisasm/build/lib:$LD_LIBRARY_PATH ./target/release/ddisasm-ascent ./data </path/to/binary>
```


RAYON_NUM_THREADS=12 DDISASM_GTIRB_MODULE_NAME=cvc5 DDISASM_DEBUG_DIR=./data/cvc5 LD_LIBRARY_PATH=/usr/local/src/ddisasm/build/lib:$LD_LIBRARY_PATH ./target/release/ddisasm-ascent ./data/cvc5 ./data/bin/cvc5 > output2
RAYON_NUM_THREADS=12 DDISASM_GTIRB_MODULE_NAME=clippy-driver DDISASM_DEBUG_DIR=./data/clippy-driver LD_LIBRARY_PATH=/usr/local/src/ddisasm/build/lib:$LD_LIBRARY_PATH ./target/release/ddisasm-ascent ./data/clippy-driver ./data/bin/clippy-driver > ./data/clippy-driver/output
RAYON_NUM_THREADS=12 DDISASM_GTIRB_MODULE_NAME=z3 DDISASM_DEBUG_DIR=./data/z3 LD_LIBRARY_PATH=/usr/local/src/ddisasm/build/lib:$LD_LIBRARY_PATH ./target/release/ddisasm-ascent ./data/z3 ./data/bin/z3 > ./data/z3/output
