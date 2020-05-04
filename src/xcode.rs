use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::process::Command;

#[derive(Copy, Clone)]
pub enum AppleSdk {
    MacOs,
    IOs,
    IOsSimulator,
    TvOs,
    TvOsSimulator,
    WatchOs,
    WatchOsSimulator,
}

impl AppleSdk {
    pub fn sdk_name(&self) -> &str {
        match *self {
            AppleSdk::MacOs => "macosx",
            AppleSdk::IOs => "iphoneos",
            AppleSdk::IOsSimulator => "iphonesimulator",
            AppleSdk::TvOs => "appletvos",
            AppleSdk::TvOsSimulator => "appletvsimulator",
            AppleSdk::WatchOs => "watchos",
            AppleSdk::WatchOsSimulator => "watchsimulator",
        }
    }
}

pub fn sdk_path(sdk: AppleSdk) -> String {
    let output = Command::new("xcrun")
        .args(&["--sdk", sdk.sdk_name(), "--show-sdk-path"])
        .output()
        .expect("xcrun command failed to start");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

pub fn libclang_path() -> std::path::PathBuf {
    // Instead of hard-coding it, using `xcodebuild -find-library libclang.dylib` might be better?
    let path = std::path::Path::new(DEVELOPER_DIR.as_str())
        .join("Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib");
    assert!(
        path.exists(),
        "Could not find libclang at {}",
        path.to_string_lossy()
    );
    path
}

pub static DEVELOPER_DIR: Lazy<String> = Lazy::new(|| {
    let output = Command::new("xcode-select")
        .args(&["--print-path"])
        .output()
        .expect("xcode-select command failed to start");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().to_string()
});

pub fn clang_options(sdk: AppleSdk) -> Vec<Cow<'static, str>> {
    vec![
        // Our temporary files don't have an Objective-C extension so set the language explicitely
        // (for some reason -ObjC doesn't seem to work properly)
        Cow::Borrowed("-x"),
        Cow::Borrowed("objective-c"),
        // We want ARC (Automatic Reference Counting) ON.
        Cow::Borrowed("-fobjc-arc"),
        Cow::Borrowed("-isysroot"),
        Cow::Owned(sdk_path(sdk)),
    ]
}

pub fn preprocess_objc(source: &str, sdk: AppleSdk) -> String {
    use std::io::Write;
    use std::process::Stdio;

    let clang_opts = clang_options(sdk);
    // "-E" means only do preprocessing.
    let mut xcrun_args = vec!["clang", "-E"];
    xcrun_args.extend(clang_opts.iter().map(|opt| opt.as_ref()));
    xcrun_args.push("-"); // read from stdin
    let mut child = Command::new("xcrun")
        .args(&xcrun_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("clang command failed to start");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        stdin
            .write_all(source.as_bytes())
            .expect("Failed to write to stdin");
    }

    let output = child.wait_with_output().expect("Failed to read stdout");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).to_string()
}
