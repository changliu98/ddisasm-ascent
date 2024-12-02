
# ddisasm-ascent

To create a dataset from a binary, you can use the following command :
```bash
ddisasm --asm=</path/to/asm> -j 12 --debug-dir <dataset_dir> </path/to/binary>
```

To run souffle on the dataset, you can use the following command :
```bash
export FUNCTOR=/usr/local/src/ddisasm/build/lib/libfunctors.so ./
export LD_LIBRARY_PATH=$FUNCOR:$LD_LIBRARY_PATH
souffle -L /usr/local/src/ddisasm/build/lib/ -j 12 -o ./extracted.bin ./ddisasm.dl
DDISASM_DEBUG_DIR=</path/to/db>/disassembly DDISASM_GTIRB_MODULE_NAME=<binary> /usr/bin/time -v ./extracted.bin -j 16  -F </path/to/db>/disassembly -D </path/to/result>
```

To run this artifacts you can use :
```bash
cargo build --release
DDISASM_GTIRB_MODULE_NAME=<binary> DDISASM_DEBUG_DIR=./data/disassembly/ LD_LIBRARY_PATH=/usr/local/src/ddisasm/build/lib:$LD_LIBRARY_PATH ./target/release/ddisasm-ascent ./data </path/to/binary>
```

