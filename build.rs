
fn main() {
    println!("cargo:rustc-link-search=native=/usr/local/src/ddisasm/build/lib/\n\
              cargo:rustc-link-lib=dylib=functors");
}
