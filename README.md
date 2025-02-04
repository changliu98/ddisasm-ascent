# Compcert in Ascent

This repository contains the code to implement some of compcert's passes in ascent.

## Installation

```bash
git clone https://github.com/StarGazerM/ascent-plusplus.git
git clone https://github.com/AbsInt/CompCert.git
rm CompCert/backend/PrintMach.ml
ln -s $(pwd)/misc/printMach.ml CompCert/backend/PrintMach.ml
```
