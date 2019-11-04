fn main() {
    cc::Build::new()
        .file("src/chocolat.m")
        .flag("-fobjc-arc")
        .compile("chocolat");
    println!("cargo:rustc-link-lib=framework=Foundation")
}
