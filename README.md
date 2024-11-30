
# ddisasm - ascent

use :
```bash
cargo build --release
DDISASM_GTIRB_MODULE_NAME=libgtirb.so DDISASM_DEBUG_DIR=./data/disassembly/ LD_LIBRARY_PATH=/usr/local/src/ddisasm/build/lib:$LD_LIBRARY_PATH ./target/r
elease/ddisasm-ascent ./data /usr/local/lib/libgtirb.so
```

